;;;; parser.lisp - Python parser with indentation-aware block parsing

(in-package #:python-cl)

(defstruct parse-state
  tokens
  position
  current-token)

(defparameter *parser-debug* nil
  "When non-nil, emit parser decision logging to *trace-output*.")

(defun parser-debug (format-string &rest args)
  "Emit debug logging for parser decision points when enabled."
  (when *parser-debug*
    (apply #'format *trace-output*
           (concatenate 'string "[parser] " format-string "~%")
           args)))

(defun make-parser-state (tokens)
  (make-parse-state :tokens tokens :position 0 :current-token (first tokens)))

(defun advance-token (state)
  (when (< (1+ (parse-state-position state)) (length (parse-state-tokens state)))
    (incf (parse-state-position state))
    (setf (parse-state-current-token state)
          (nth (parse-state-position state) (parse-state-tokens state))))
  (parse-state-current-token state))

(defun peek-token (state)
  (parse-state-current-token state))

(defun peek-next-token (state)
  (let ((next-pos (1+ (parse-state-position state))))
    (when (< next-pos (length (parse-state-tokens state)))
      (nth next-pos (parse-state-tokens state)))))

(defun token-matches-p (state type &optional value)
  (let ((token (peek-token state)))
    (and token
         (eq (token-type token) type)
         (or (null value)
             (equal (token-value token) value)))))

(defun consume-token (state type &optional value)
  (if (token-matches-p state type value)
      (prog1 (peek-token state)
        (advance-token state))
      (error "Expected ~A~@[ with value ~A~], got ~A" type value (peek-token state))))

(defun skip-newlines (state)
  (loop while (token-matches-p state :newline)
        do (advance-token state))
  state)

(defun operator-symbol (token)
  (let ((value (token-value token)))
    (cond
      ((eq (token-type token) :keyword)
       (intern (string-upcase value) :keyword))
      (t
       (intern value :keyword)))))

(defun operator-precedence (token)
  (case (operator-symbol token)
    (:** 14)
    ((:* :/ :// :%) 12)
    ((:+ :-) 11)
    ((:<< :>>) 10)
    (:& 9)
    (:^ 8)
    (:\| 7)
    ((:< :<= :> :>= :!= :==) 6)
    (:AND 5)
    (:OR 4)
    (t -1)))

(defun parse-expression (state)
  (parse-conditional-expression state))

(defun parse-conditional-expression (state)
  "Parse Python conditional expressions: value if condition else alternative.
These are right-associative and lower precedence than boolean OR."
  (let ((consequent (parse-binary-op state 0)))
    (if (token-matches-p state :keyword "if")
        (progn
          (consume-token state :keyword "if")
          (let ((test (parse-binary-op state 0)))
            (consume-token state :keyword "else")
            (make-py-conditional-expression consequent
                                            test
                                            (parse-conditional-expression state))))
        consequent)))

(defun parse-binary-op (state min-precedence)
  (let ((left (parse-unary state)))
    (loop for token = (peek-token state)
          while (and token
                     (or (eq (token-type token) :operator)
                         (and (eq (token-type token) :keyword)
                              (member (token-value token) '("and" "or") :test #'string=)))
                     (>= (operator-precedence token) min-precedence))
          do (let* ((op-token token)
                    (op (operator-symbol op-token))
                    (precedence (operator-precedence op-token))
                    (next-min (if (eq op :**) precedence (1+ precedence))))
               (advance-token state)
               (setf left (make-py-binop left op (parse-binary-op state next-min)))))
    left))

(defun parse-unary (state)
  (let ((token (peek-token state)))
    (if (or (and (eq (token-type token) :operator)
                 (member (token-value token) '("+" "-" "~") :test #'string=))
            (and (eq (token-type token) :keyword)
                 (string= (token-value token) "not")))
        (let ((op (operator-symbol token)))
          (advance-token state)
          (make-py-unaryop op (parse-unary state)))
        (parse-primary state))))

(defun parse-call-arguments (state)
  (let ((args nil))
    (unless (token-matches-p state :delimiter ")")
      (loop
        (push (parse-expression state) args)
        (unless (token-matches-p state :delimiter ",")
          (return))
        (consume-token state :delimiter ",")))
    (reverse args)))

(defun parse-primary (state)
  (let ((token (peek-token state)))
    (cond
      ((eq (token-type token) :number)
       (advance-token state)
       (make-py-num (token-value-as-number token)))
      ((eq (token-type token) :string)
       (advance-token state)
       (make-py-str (token-value token)))
      ((and (eq (token-type token) :keyword)
            (member (token-value token) '("True" "False") :test #'string=))
       (advance-token state)
       (make-py-bool (string= (token-value token) "True")))
      ((and (eq (token-type token) :keyword)
            (string= (token-value token) "None"))
       (advance-token state)
       (make-py-name 'none))
      ((eq (token-type token) :identifier)
       (let ((expr (make-py-name (token-value token))))
         (advance-token state)
         (loop while (token-matches-p state :delimiter "(")
               do (consume-token state :delimiter "(")
                  (setf expr (make-py-call expr (parse-call-arguments state)))
                  (consume-token state :delimiter ")"))
         expr))
      ((token-matches-p state :delimiter "(")
       (consume-token state :delimiter "(")
       (let ((expr (parse-expression state)))
         (consume-token state :delimiter ")")
         expr))
      (t
       (error "Unexpected token in expression: ~A" token)))))

(defun parse-assignment (state)
  (let* ((target (make-py-name (token-value (consume-token state :identifier))))
         (op-token (consume-token state :operator)))
    (cond
      ((string= (token-value op-token) "=")
       (make-py-assign target (parse-expression state)))
      ((member (token-value op-token) '("+=" "-=" "*=" "/=" "//=" "%=" "**=") :test #'string=)
       (make-py-aug-assign target
                           (intern (subseq (token-value op-token) 0 (1- (length (token-value op-token)))) :keyword)
                           (parse-expression state)))
      (t
       (error "Unsupported assignment operator: ~A" (token-value op-token))))))

(defun parse-return-statement (state)
  (consume-token state :keyword "return")
  (if (or (token-matches-p state :newline)
          (token-matches-p state :dedent)
          (token-matches-p state :eof))
      (make-py-return nil)
      (make-py-return (parse-expression state))))

(defun parse-statement-list (state &optional stop-on-dedent)
  (let ((statements nil))
    (skip-newlines state)
    (loop while (and (not (token-matches-p state :eof))
                     (not (and stop-on-dedent (token-matches-p state :dedent))))
          do (push (parse-statement state) statements)
             (skip-newlines state))
    (reverse statements)))

(defun parse-suite (state)
  (if (token-matches-p state :newline)
      (progn
        (consume-token state :newline)
        (consume-token state :indent)
        (let ((body (parse-statement-list state t)))
          (consume-token state :dedent)
          body))
      (prog1 (list (parse-statement state))
        (when (token-matches-p state :newline)
          (consume-token state :newline)))))

(defun parse-if-statement (state)
  (consume-token state :keyword "if")
  (let ((test (parse-expression state)))
    (consume-token state :delimiter ":")
    (let ((body (parse-suite state)))
      (skip-newlines state)
      (cond
        ((token-matches-p state :keyword "elif")
         (make-py-if test body (list (parse-if-statement state))))
        ((token-matches-p state :keyword "else")
         (consume-token state :keyword "else")
         (consume-token state :delimiter ":")
         (make-py-if test body (parse-suite state)))
        (t
         (make-py-if test body nil))))))

(defun parse-while-statement (state)
  (consume-token state :keyword "while")
  (let ((test (parse-expression state)))
    (consume-token state :delimiter ":")
    (make-py-while test (parse-suite state))))

(defun parse-argument-list (state)
  (let ((args nil))
    (unless (token-matches-p state :delimiter ")")
      (loop
        (push (token-value (consume-token state :identifier)) args)
        (unless (token-matches-p state :delimiter ",")
          (return))
        (consume-token state :delimiter ",")))
    (reverse args)))

(defun parse-function-def (state)
  (consume-token state :keyword "def")
  (let ((name (token-value (consume-token state :identifier))))
    (consume-token state :delimiter "(")
    (let ((args (parse-argument-list state)))
      (consume-token state :delimiter ")")
      (consume-token state :delimiter ":")
      (make-py-function-def name args (parse-suite state)))))

(defun statement-keyword-p (token)
  "Return true when TOKEN starts a statement handled by this parser."
  (and token
       (eq (token-type token) :keyword)
       (member (token-value token)
               '("def" "if" "while" "return" "break" "continue")
               :test #'string=)))

(defun assignment-start-p (state)
  "Return true when the current parse position starts an assignment."
  (let ((token (peek-token state)))
    (and token
         (eq (token-type token) :identifier)
         (let ((next (peek-next-token state)))
           (and next
                (eq (token-type next) :operator)
                (member (token-value next)
                        '("=" "+=" "-=" "*=" "/=" "//=" "%=" "**=")
                        :test #'string=))))))

(defun expression-start-token-p (token)
  "Return true when TOKEN can start an expression supported by this parser."
  (and token
       (or (member (token-type token) '(:number :string :identifier) :test #'eq)
           (and (eq (token-type token) :delimiter)
                (string= (token-value token) "("))
           (and (eq (token-type token) :operator)
                (member (token-value token) '("+" "-" "~") :test #'string=))
           (and (eq (token-type token) :keyword)
                (member (token-value token) '("True" "False" "None" "not") :test #'string=)))))

(defun parse-statement (state)
  (let ((token (peek-token state)))
    (parser-debug "parse-statement at position ~D with token ~S"
                  (parse-state-position state)
                  token)
    (cond
      ((token-matches-p state :keyword "def")
       (parse-function-def state))
      ((token-matches-p state :keyword "if")
       (parse-if-statement state))
      ((token-matches-p state :keyword "while")
       (parse-while-statement state))
      ((token-matches-p state :keyword "return")
       (parse-return-statement state))
      ((token-matches-p state :keyword "break")
       (advance-token state)
       (make-py-break))
      ((token-matches-p state :keyword "continue")
       (advance-token state)
       (make-py-continue))
      ((assignment-start-p state)
       (parse-assignment state))
      (t
       (make-py-expr-stmt (parse-expression state))))))

(defun ensure-no-trailing-tokens (state)
  "Require that parsing consumed all remaining non-newline tokens."
  (skip-newlines state)
  (unless (token-matches-p state :eof)
    (error "Unexpected trailing token after parse: ~A" (peek-token state))))

(defun parse-tokens (tokens)
  (let ((state (make-parser-state tokens)))
    (skip-newlines state)
    (when (token-matches-p state :eof)
      (return-from parse-tokens nil))
    (let ((first-token (peek-token state)))
      (parser-debug "top-level parse starts with token ~S" first-token)
      (cond
        ((or (statement-keyword-p first-token)
             (assignment-start-p state))
         (parser-debug "top-level decided: statement")
         (let ((ast (parse-statement state)))
           (ensure-no-trailing-tokens state)
           ast))
        ((expression-start-token-p first-token)
         (parser-debug "top-level decided: expression")
         (let ((ast (parse-expression state)))
           (ensure-no-trailing-tokens state)
           ast))
        (t
         (error "Unable to determine whether input is a statement or expression: ~A"
                first-token))))))

(defun parse-python (source)
  (parse-tokens (tokenize source)))
