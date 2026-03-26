;;;; lexer.lisp - Python tokenization

(in-package #:python-cl)

;;; Token Types

(deftype token-type ()
  '(member :number :string :identifier :operator :delimiter :keyword :newline :indent :dedent :eof))

(defstruct token
  type
  value
  line
  column
  source)

;;; Lexical Analysis

(defparameter *python-keywords*
  '("False" "None" "True" "and" "as" "assert" "async" "await" "break" 
    "class" "continue" "def" "del" "elif" "else" "except" "finally" 
    "for" "from" "global" "if" "import" "in" "is" "lambda" "nonlocal" 
    "not" "or" "pass" "raise" "return" "try" "while" "with" "yield"))

(defparameter *python-operators*
  '("+" "-" "*" "/" "//" "%" "**" "<<" ">>" "&" "|" "^" "~" 
    "<" ">" "<=" ">=" "==" "!=" "=" "+=" "-=" "*=" "/=" "//=" 
    "%=" "**=" "<<=" ">>=" "&=" "|=" "^=" "->" ":="))

(defparameter *python-delimiters*
  '("(" ")" "[" "]" "{" "}" "," ":" ";" "@" "." "..." "->"))

(defun whitespace-p (char)
  "Check if character is Python whitespace (excluding newline)"
  (member char '(#\Space #\Tab #\Form) :test #'char=))

(defun identifier-start-p (char)
  "Check if character can start a Python identifier"
  (or (alpha-char-p char) (char= char #\_)))

(defun identifier-char-p (char)
  "Check if character can be part of a Python identifier"
  (or (alphanumericp char) (char= char #\_)))

(defun digit-char-p* (char &optional (radix 10))
  "Extended digit check supporting different radixes"
  (digit-char-p char radix))

(defun scan-number (source start)
  "Scan a numeric literal from source starting at position start"
  (let ((pos start)
        (len (length source))
        (has-dot nil)
        (has-exp nil)
        (radix 10))
    
    ;; Handle hex/oct/bin prefixes
    (when (and (< (+ pos 1) len)
               (char= (char source pos) #\0))
      (case (char-downcase (char source (1+ pos)))
        (#\x (setf radix 16 pos (+ pos 2)))
        (#\o (setf radix 8 pos (+ pos 2)))
        (#\b (setf radix 2 pos (+ pos 2)))))
    
    ;; Scan digits
    (loop while (and (< pos len)
                     (or (digit-char-p* (char source pos) radix)
                         (and (= radix 10) (char= (char source pos) #\.))
                         (and (= radix 10) (member (char-downcase (char source pos)) '(#\e) :test #'char=))))
          do (case (char-downcase (char source pos))
               (#\. (if has-dot 
                        (return)
                        (setf has-dot t)))
               ((#\e #\E) (setf has-exp t pos (+ pos 1))
                          ;; Handle optional +/- after e
                          (when (and (< pos len)
                                     (member (char source pos) '(#\+ #\-) :test #'char=))
                            (incf pos)))
               (t nil))
             (incf pos))
    
    ;; Handle imaginary numbers (j suffix)
    (when (and (< pos len)
               (member (char-downcase (char source pos)) '(#\j) :test #'char=))
      (incf pos))
    
    (values pos (subseq source start pos))))

(defun scan-string (source start)
  "Scan a string literal from source starting at position start"
  (let ((pos start)
        (len (length source))
        (quote-char (char source start))
        (is-raw nil)
        (is-bytes nil)
        (is-fstring nil)
        (is-triple nil))
    
    ;; Handle prefixes (r, b, f, combinations)
    (when (and (> start 0)
               (member (char-downcase (char source (1- start))) '(#\r #\b #\f) :test #'char=))
      (case (char-downcase (char source (1- start)))
        (#\r (setf is-raw t))
        (#\b (setf is-bytes t))
        (#\f (setf is-fstring t))))
    
    ;; Check for triple quotes
    (when (and (< (+ pos 2) len)
               (char= (char source pos) quote-char)
               (char= (char source (+ pos 1)) quote-char)
               (char= (char source (+ pos 2)) quote-char))
      (setf is-triple t)
      (incf pos 3))
    
    (unless is-triple
      (incf pos))
    
    ;; Scan until closing quote(s)
    (loop while (< pos len)
          do (let ((current-char (char source pos)))
               (cond
                 ((and is-triple
                       (< (+ pos 2) len)
                       (char= current-char quote-char)
                       (char= (char source (+ pos 1)) quote-char)
                       (char= (char source (+ pos 2)) quote-char))
                  ;; Found closing triple quote
                  (incf pos 3)
                  (return))
                 ((and (not is-triple) (char= current-char quote-char))
                  ;; Found closing single quote
                  (incf pos)
                  (return))
                 ((and (not is-raw) (char= current-char #\\))
                  ;; Escape sequence (skip next char)
                  (incf pos 2))
                 (t
                  (incf pos)))))
    
    (values pos (subseq source start pos))))

(defun scan-identifier (source start)
  "Scan an identifier from source starting at position start"
  (let ((pos start)
        (len (length source)))
    
    (loop while (and (< pos len)
                     (identifier-char-p (char source pos)))
          do (incf pos))
    
    (values pos (subseq source start pos))))

(defun tokenize (source)
  "Tokenize Python source code into a list of tokens"
  (let ((tokens '())
        (pos 0)
        (len (length source))
        (line 1)
        (column 1))
    
    (flet ((add-token (type value)
             (push (make-token :type type :value value :line line :column column :source source) tokens)))
      
      (loop while (< pos len)
            do (let ((char (char source pos)))
                 (cond
                   ;; Whitespace (skip)
                   ((whitespace-p char)
                    (incf pos)
                    (incf column))
                   
                   ;; Newline
                   ((char= char #\Newline)
                    (add-token :newline "\\n")
                    (incf pos)
                    (incf line)
                    (setf column 1))
                   
                   ;; Comments (skip to end of line)
                   ((char= char #\#)
                    (loop while (and (< pos len) (char/= (char source pos) #\Newline))
                          do (incf pos)))
                   
                   ;; String literals
                   ((member char '(#\' #\") :test #'char=)
                    (multiple-value-bind (new-pos token-value)
                        (scan-string source pos)
                      (add-token :string token-value)
                      (setf pos new-pos)
                      (incf column (- new-pos pos))))
                   
                   ;; Numbers
                   ((digit-char-p char)
                    (multiple-value-bind (new-pos token-value)
                        (scan-number source pos)
                      (add-token :number token-value)
                      (setf pos new-pos)
                      (incf column (- new-pos pos))))
                   
                   ;; Identifiers and keywords (check for string prefixes first)
                   ((identifier-start-p char)
                    (multiple-value-bind (new-pos token-value)
                        (scan-identifier source pos)
                      ;; Check if this is a string prefix (r, u, b, f followed by quote)
                      (if (and (< new-pos len)
                               (member (string-downcase token-value) '("r" "u" "b" "f" "rf" "fr" "rb" "br") :test #'string=)
                               (member (char source new-pos) '(#\' #\") :test #'char=))
                          ;; This is a string prefix - scan the string
                          (multiple-value-bind (string-end string-content)
                              (scan-string source new-pos)
                            (add-token :string (concatenate 'string token-value string-content))
                            (setf pos string-end)
                            (incf column (- string-end pos)))
                          ;; Regular identifier or keyword
                          (progn
                            (if (member token-value *python-keywords* :test #'string=)
                                (add-token :keyword token-value)
                                (add-token :identifier token-value))
                            (setf pos new-pos)
                            (incf column (- new-pos pos))))))
                   
                   ;; Operators and delimiters
                   (t
                    ;; Try to match multi-character operators first
                    (let ((matched nil))
                      (dolist (op (sort (copy-list *python-operators*) #'> :key #'length))
                        (when (and (<= (+ pos (length op)) len)
                                   (string= (subseq source pos (+ pos (length op))) op))
                          (add-token :operator op)
                          (incf pos (length op))
                          (incf column (length op))
                          (setf matched t)
                          (return)))
                      
                      (unless matched
                        ;; Try delimiters
                        (dolist (delim *python-delimiters*)
                          (when (and (<= (+ pos (length delim)) len)
                                     (string= (subseq source pos (+ pos (length delim))) delim))
                            (add-token :delimiter delim)
                            (incf pos (length delim))
                            (incf column (length delim))
                            (setf matched t)
                            (return)))
                        
                        (unless matched
                          ;; Unknown character
                          (error "Unknown character: ~A at line ~A, column ~A" char line column))))))))
      
      ;; Add EOF token
      (add-token :eof nil)
      
      (nreverse tokens))))

;;; Token Utilities

(defun token-value-as-number (token)
  "Convert a number token's value to a Lisp number"
  (assert (eq (token-type token) :number))
  (let ((value (token-value token)))
    (cond
      ;; Imaginary numbers
      ((and (> (length value) 0)
            (member (char (string-downcase value) (1- (length value))) '(#\j) :test #'char=))
       (let ((real-part (subseq value 0 (1- (length value)))))
         (cond
           ;; Pure imaginary (e.g., "4j")
           ((= (length real-part) 0)
            (complex 0 1))
           ;; Check for complex number format (e.g., "3+4" from "3+4j")
           ((find #\+ real-part :start 1)  ; Skip first char in case of negative
            (let ((plus-pos (position #\+ real-part :start 1)))
              (complex (read-from-string (subseq real-part 0 plus-pos))
                       (read-from-string (subseq real-part (1+ plus-pos))))))
           ((find #\- real-part :start 1)  ; Handle subtraction (3-4j)
            (let ((minus-pos (position #\- real-part :start 1)))
              (complex (read-from-string (subseq real-part 0 minus-pos))
                       (- (read-from-string (subseq real-part (1+ minus-pos)))))))
           ;; Simple imaginary (e.g., "4j")
           (t
            (complex 0 (read-from-string real-part))))))
      
      ;; Hex numbers
      ((and (> (length value) 2)
            (string-equal (subseq value 0 2) "0x"))
       (parse-integer (subseq value 2) :radix 16))
      
      ;; Octal numbers
      ((and (> (length value) 2)
            (string-equal (subseq value 0 2) "0o"))
       (parse-integer (subseq value 2) :radix 8))
      
      ;; Binary numbers
      ((and (> (length value) 2)
            (string-equal (subseq value 0 2) "0b"))
       (parse-integer (subseq value 2) :radix 2))
      
      ;; Float numbers
      ((or (find #\. value) (find #\e value) (find #\E value))
       (read-from-string value))
      
      ;; Integer numbers
      (t
       (parse-integer value)))))

(defun parse-number (string)
  "Parse a number string into a Lisp number"
  (cond
    ;; Handle hex
    ((and (> (length string) 2) (string-equal (subseq string 0 2) "0x"))
     (parse-integer (subseq string 2) :radix 16))
    
    ;; Handle octal
    ((and (> (length string) 2) (string-equal (subseq string 0 2) "0o"))
     (parse-integer (subseq string 2) :radix 8))
    
    ;; Handle binary
    ((and (> (length string) 2) (string-equal (subseq string 0 2) "0b"))
     (parse-integer (subseq string 2) :radix 2))
    
    ;; Handle float/complex
    (t (read-from-string string))))
