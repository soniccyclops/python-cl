;;;; parser.lisp - Simple Python parser (MVP implementation)

(in-package #:python-cl)

;;; Simple Recursive Descent Parser

(defstruct parse-state
  tokens
  position
  current-token)

(defun advance-token (state)
  "Advance to next token in parse state"
  (when (< (1+ (parse-state-position state)) (length (parse-state-tokens state)))
    (incf (parse-state-position state))
    (setf (parse-state-current-token state)
          (nth (parse-state-position state) (parse-state-tokens state))))
  (parse-state-current-token state))

(defun peek-token (state)
  "Look at current token without consuming it"
  (parse-state-current-token state))

(defun token-matches-p (state type &optional value)
  "Check if current token matches type and optional value"
  (let ((token (peek-token state)))
    (and token
         (eq (token-type token) type)
         (or (null value)
             (string= (token-value token) value)))))

(defun consume-token (state type &optional value)
  "Consume token if it matches, otherwise error"
  (if (token-matches-p state type value)
      (prog1 (peek-token state)
        (advance-token state))
      (error "Expected ~A~@[ with value ~A~], got ~A"
             type value (peek-token state))))

;;; Expression Parsing (Precedence Climbing)

(defun parse-expression (state)
  "Parse a Python expression"
  (parse-binary-op state 0))

(defun parse-binary-op (state min-precedence)
  "Parse binary operations with precedence climbing"
  (let ((left (parse-unary state)))
    (loop while (and (peek-token state)
                     (eq (token-type (peek-token state)) :operator)
                     (>= (operator-precedence (token-value (peek-token state))) min-precedence))
          do (let* ((op-token (consume-token state :operator))
                    (op (intern (string-upcase (token-value op-token))))
                    (precedence (operator-precedence (token-value op-token)))
                    (right (parse-binary-op state (1+ precedence))))
               (setf left (make-py-binop left op right))))
    left))

(defun parse-unary (state)
  "Parse unary operations"
  (let ((token (peek-token state)))
    (if (and (eq (token-type token) :operator)
             (member (token-value token) '("+" "-" "~" "not") :test #'string=))
        ;; Unary operator
        (let* ((op-token (consume-token state :operator))
               (op (intern (string-upcase (token-value op-token))))
               (operand (parse-unary state)))  ; Right-associative
          (make-py-unaryop op operand))
        ;; Not a unary operator, parse primary
        (parse-primary state))))

(defun parse-primary (state)
  "Parse primary expressions (literals, names, parentheses)"
  (let ((token (peek-token state)))
    (cond
      ;; Numbers
      ((eq (token-type token) :number)
       (advance-token state)
       (make-py-num (token-value-as-number token)))
      
      ;; Strings
      ((eq (token-type token) :string)
       (advance-token state)
       (make-py-str (token-value token)))
      
      ;; Identifiers
      ((eq (token-type token) :identifier)
       (advance-token state)
       (make-py-name (token-value token)))
      
      ;; Parenthesized expressions
      ((token-matches-p state :delimiter "(")
       (consume-token state :delimiter "(")
       (let ((expr (parse-expression state)))
         (consume-token state :delimiter ")")
         expr))
      
      (t (error "Unexpected token in expression: ~A" token)))))

(defun operator-precedence (op-string)
  "Return precedence for Python operators (higher = tighter binding)"
  (case (intern (string-upcase op-string) :keyword)
    (:** 14)     ; Exponentiation (right associative)
    ((:+ :-) 13) ; Unary plus/minus
    ((:* :/ :/\/ :%) 12) ; Multiplication, division, modulo
    ((:+ :-) 11) ; Addition, subtraction
    (t 0)))      ; Unknown operators

;;; Statement Parsing

(defun parse-statement (state)
  "Parse a Python statement"
  (cond
    ;; Simple expression statement
    ((or (eq (token-type (peek-token state)) :number)
         (eq (token-type (peek-token state)) :identifier))
     (let ((expr (parse-expression state)))
       (make-instance 'py-expr-stmt :value expr)))
    
    (t (error "Unknown statement type: ~A" (peek-token state)))))

;;; Top-level Parser Interface

(defun parse-tokens (tokens)
  "Parse tokens into an AST"
  (when (null tokens)
    (return-from parse-tokens nil))
  
  (let ((state (make-parse-state :tokens tokens
                                 :position 0
                                 :current-token (first tokens))))
    
    ;; Skip EOF tokens at the end
    (when (eq (token-type (peek-token state)) :eof)
      (return-from parse-tokens nil))
    
    ;; Parse single expression for now
    (parse-expression state)))

(defun parse-python (source)
  "Parse Python source code into AST"
  (let ((tokens (tokenize source)))
    (parse-tokens tokens)))