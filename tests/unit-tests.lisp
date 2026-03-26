;;;; tests/unit-tests.lisp

(in-package #:python-cl/tests)

(def-suite python-cl-suite
  :description "Python-CL unit tests")

(in-suite python-cl-suite)

;;; Lexer Tests

(test tokenize-numbers
  "Test numeric literal tokenization"
  (let ((tokens (tokenize "42")))
    (is (= (length tokens) 3))
    (is (eq (token-type (first tokens)) :number))
    (is (string= (token-value (first tokens)) "42")))

  (let ((tokens (tokenize "3.14")))
    (is (= (length tokens) 3))
    (is (eq (token-type (first tokens)) :number))
    (is (string= (token-value (first tokens)) "3.14")))

  (let ((tokens (tokenize "0xFF")))
    (is (= (length tokens) 3))
    (is (eq (token-type (first tokens)) :number))
    (is (string= (token-value (first tokens)) "0xFF"))))

(test tokenize-identifiers
  "Test identifier tokenization"
  (let ((tokens (tokenize "hello")))
    (is (= (length tokens) 3))
    (is (eq (token-type (first tokens)) :identifier))
    (is (string= (token-value (first tokens)) "hello")))

  (let ((tokens (tokenize "def")))
    (is (= (length tokens) 3))
    (is (eq (token-type (first tokens)) :keyword))
    (is (string= (token-value (first tokens)) "def"))))

(test tokenize-operators
  "Test operator tokenization"
  (let ((tokens (tokenize "+")))
    (is (= (length tokens) 3))
    (is (eq (token-type (first tokens)) :operator))
    (is (string= (token-value (first tokens)) "+")))

  (let ((tokens (tokenize "+=")))
    (is (= (length tokens) 3))
    (is (eq (token-type (first tokens)) :operator))
    (is (string= (token-value (first tokens)) "+="))))

(test tokenize-expressions
  "Test tokenizing simple expressions"
  (let ((tokens (tokenize "2 + 3")))
    (is (= (length tokens) 5))
    (is (eq (token-type (first tokens)) :number))
    (is (eq (token-type (second tokens)) :operator))
    (is (eq (token-type (third tokens)) :number))
    (is (eq (token-type (fourth tokens)) :newline))))

(test tokenize-indentation
  "Tokenizer emits INDENT/DEDENT tokens for Python blocks"
  (let* ((source (format nil "if x > 5:~%    y = 1~%    z = 2~%w = 3~%"))
         (tokens (tokenize source))
         (types (mapcar #'token-type tokens)))
    (is (equal types
               '(:keyword :identifier :operator :number :delimiter :newline
                 :indent
                 :identifier :operator :number :newline
                 :identifier :operator :number :newline
                 :dedent
                 :identifier :operator :number :newline
                 :eof)))))

(test indentation-errors
  "Invalid indentation patterns raise errors"
  (signals error (tokenize (format nil "if x:~%  y = 1~% z = 2~%")))
  (signals error (tokenize (format nil "if x:~% 	y = 1~%"))))

;;; AST / Object Tests

(test ast-creation
  "Test AST node creation"
  (let ((num-node (make-py-num 42)))
    (is (typep num-node 'py-num))
    (is (= (py-value num-node) 42)))

  (let ((name-node (make-py-name 'x)))
    (is (typep name-node 'py-name))
    (is (eq (py-id name-node) 'x)))

  (let ((binop-node (make-py-binop (make-py-num 2) :+ (make-py-num 3))))
    (is (typep binop-node 'py-binop))
    (is (eq (py-op binop-node) :+))
    (is (= (py-value (py-left binop-node)) 2))
    (is (= (py-value (py-right binop-node)) 3))))

(test python-objects
  "Test Python object creation"
  (let ((py-int (make-py-int 42)))
    (is (typep py-int 'py-int))
    (is (= (py-value py-int) 42))
    (is (equal (symbol-name (py-type-name py-int)) "INT")))

  (let ((py-str (make-py-str "hello")))
    (is (typep py-str 'py-str))
    (is (string= (py-value py-str) "hello"))
    (is (equal (symbol-name (py-type-name py-str)) "STR")))

  (let ((py-list (make-py-list 1 2 3)))
    (is (typep py-list 'py-list))
    (is (= (length (py-elements py-list)) 3))))

(test type-conversion
  "Test Lisp <-> Python type conversion"
  (is (= (python-to-lisp (make-py-int 42)) 42))
  (is (string= (python-to-lisp (make-py-str "hello")) "hello"))
  (is (equal (python-to-lisp (make-py-list 1 2 3)) '(1 2 3)))
  (is (typep (lisp-to-python 42) 'py-int))
  (is (typep (lisp-to-python "hello") 'py-str))
  (is (typep (lisp-to-python '(1 2 3)) 'py-list)))

;;; Environment / Evaluation Tests

(test environment-basics
  "Test environment creation and lookup"
  (let ((env (make-py-env)))
    (is (typep env 'py-env))
    (py-bind 'x (make-py-int 42) env)
    (is (= (py-value (py-lookup 'x env)) 42))
    (is (py-bound-p 'print env))))

(test scoping-rules
  "Test Python LEGB scoping"
  (let ((global-env (make-py-env)))
    (py-bind 'x (make-py-int 10) global-env :scope :global)
    (let ((local-env (py-push-scope global-env)))
      (is (= (py-value (py-lookup 'x local-env)) 10))
      (py-bind 'x (make-py-int 20) local-env :scope :local)
      (is (= (py-value (py-lookup 'x local-env)) 20))
      (is (= (py-value (py-lookup 'x global-env)) 10)))))

(test simple-evaluation
  "Test basic expression evaluation"
  (setf python-cl::*python-environment* (make-py-env))
  (is (= (py-eval "42") 42))
  (is (= (py-eval "3.14") 3.14))
  (is (= (py-eval "0xFF") 255))
  (is (= (py-eval "2 + 3") 5))
  (is (= (py-eval "10 - 4") 6))
  (is (= (py-eval "6 * 7") 42))
  (is (= (py-eval "15 / 3") 5))
  (is (= (py-eval "17 // 3") 5))
  (is (= (py-eval "17 % 3") 2))
  (is (= (py-eval "2 ** 8") 256)))

(test built-in-functions
  "Test built-in function implementations"
  (is (= (py-value (py-int-constructor 42.7)) 42))
  (is (= (py-value (py-float-constructor 42)) 42.0))
  (is (string= (py-value (py-str-constructor 42)) "42"))
  (is (= (py-len (make-py-str "hello")) 5))
  (is (= (py-len (make-py-list 1 2 3)) 3))
  (is (equal (symbol-name (py-type (make-py-int 42))) "INT"))
  (is (equal (symbol-name (py-type (make-py-str "hello"))) "STR")))

(test parse-top-level-expressions-as-expressions
  "Top-level expressions should parse as expressions, not expression statements."
  (let ((ast (python-cl::parse-python "x + 1")))
    (is (typep ast 'python-cl::py-binop))
    (is (eq (python-cl::py-op ast) :+))))

(test parse-top-level-statements-as-statements
  "Top-level statements should stay statements."
  (is (typep (python-cl::parse-python "x = 1") 'python-cl::py-assign))
  (is (typep (python-cl::parse-python "return 1") 'python-cl::py-return))
  (is (typep (python-cl::parse-python "break") 'python-cl::py-break))
  (is (typep (python-cl::parse-python "continue") 'python-cl::py-continue))
  (is (typep (python-cl::parse-python (format nil "if 1:~%    x~%")) 'python-cl::py-if))
  (is (typep (python-cl::parse-python (format nil "while 1:~%    x~%")) 'python-cl::py-while)))

(test reject-invalid-statement-fallbacks
  "Malformed statements must not silently parse."
  (signals error (python-cl::parse-python "x + 1 = 2"))
  (signals error (python-cl::parse-python "break + 1"))
  (signals error (python-cl::parse-python "continue + 1"))
  (signals error (python-cl::parse-python "x ="))
  (signals error (python-cl::parse-python "return = 1")))

(test parser-debug-logging-is-available
  "Parser decision logging should be available behind a debug flag."
  (let ((output (with-output-to-string (stream)
                  (let ((*trace-output* stream)
                        (python-cl::*parser-debug* t))
                    (python-cl::parse-python "x = 1")))))
    (is (search "top-level decided: statement" output))
    (is (search "parse-statement" output))))

(test block-parsing-and-execution
  "Indented blocks execute for functions, if statements, and while loops"
  (setf python-cl::*python-environment* (make-py-env))
  (is (= (py-eval (format nil "x = 0~%if 2 > 1:~%    x = 7~%x~%")) 7))
  (setf python-cl::*python-environment* (make-py-env))
  (is (= (py-eval (format nil "counter = 0~%while counter < 3:~%    counter += 1~%counter~%")) 3))
  (setf python-cl::*python-environment* (make-py-env))
  (is (= (py-eval (format nil "def add(a, b):~%    total = a + b~%    return total~%add(2, 3)~%")) 5))
  (setf python-cl::*python-environment* (make-py-env))
  (is (= (py-eval (format nil "value = 0~%if 1 < 2:~%    if 3 < 4:~%        value = 11~%value~%")) 11)))

(defun run-tests ()
  "Run all Python-CL unit tests"
  (run! 'python-cl-suite))
