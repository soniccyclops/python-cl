;;;; python-cl.lisp - Main interpreter interface

(in-package #:python-cl)

;;; Main Interpreter Interface

(defvar *python-environment* nil
  "Global Python environment for interactive use")

(defun py-eval (source)
  "Evaluate a Python expression and return the result"
  (unless *python-environment*
    (setf *python-environment* (make-py-env)))
  
  ;; Parse the source into AST and evaluate
  (let ((ast (parse-python source)))
    (if ast
        (python-to-lisp (py-eval-ast ast *python-environment*))
        (error "Failed to parse: ~A" source))))



(defun py-exec (source)
  "Execute Python statements (no return value)"
  (py-eval source)
  nil)

(defun py-load (filename)
  "Load and execute a Python file"
  (with-open-file (stream filename :direction :input)
    (let ((source (make-string (file-length stream))))
      (read-sequence source stream)
      (py-exec source))))

;;; Conformance Test Integration

(defun run-conformance-tests (&key (section nil) (verbose nil))
  "Run conformance tests against Python-CL implementation"
  (format t "~&Running conformance tests~@[ for section ~A~]...~%" section)
  
  ;; This will integrate with the actual conformance test suite
  ;; For now, run some basic validation tests
  (let ((passed 0)
        (failed 0)
        (errors '()))
    
    (flet ((test-case (name expression expected)
             (format t "~&Testing ~A: ~A => " name expression)
             (handler-case
                 (let ((result (py-eval expression)))
                   (if (equal result expected)
                       (progn
                         (format t "PASS~%")
                         (incf passed))
                       (progn
                         (format t "FAIL (got ~A, expected ~A)~%" result expected)
                         (push (list name expression result expected) errors)
                         (incf failed))))
               (error (e)
                 (format t "ERROR: ~A~%" e)
                 (push (list name expression e expected) errors)
                 (incf failed)))))
      
      ;; Basic numeric literal tests (Section 2.6)
      (test-case "integer-literal" "42" 42)
      (test-case "float-literal" "3.14" 3.14)
      (test-case "hex-literal" "0xFF" 255)
      (test-case "binary-literal" "0b1010" 10)
      (test-case "octal-literal" "0o777" 511)
      (test-case "complex-literal" "3+4j" #C(3 4))
      
      ;; Basic arithmetic (Section 6.7)
      (test-case "addition" "2 + 3" 5)
      (test-case "subtraction" "10 - 4" 6)
      (test-case "multiplication" "6 * 7" 42)
      (test-case "division" "15 / 3" 5)
      (test-case "integer-division" "17 // 3" 5)
      (test-case "modulo" "17 % 3" 2)
      (test-case "exponentiation" "2 ** 8" 256))
    
    (format t "~&~%Results: ~A passed, ~A failed~%" passed failed)
    
    (when (and verbose errors)
      (format t "~&Failures:~%")
      (dolist (error errors)
        (format t "  ~A~%" error)))
    
    (= failed 0)))

(defun test-section (section-number)
  "Test a specific Language Reference section"
  (format t "~&Testing Python Language Reference Section ~A~%" section-number)
  (run-conformance-tests :section section-number :verbose t))

;;; Development and Debugging Utilities

(defun py-tokenize (source)
  "Tokenize Python source and return tokens (for debugging)"
  (tokenize source))

(defun py-tokens (source)
  "Pretty-print tokens for a Python expression (debugging utility)"
  (dolist (token (tokenize source))
    (format t "~A: ~S~%" (token-type token) (token-value token))))

;;; Interactive Development

(defun python-repl ()
  "Start an interactive Python-CL REPL"
  (format t "~&Python-CL Interactive Interpreter~%")
  (format t "Type 'quit()' to exit~%~%")
  
  (loop
    (format t ">>> ")
    (force-output)
    (let ((input (read-line)))
      (cond
        ((string= input "quit()")
         (format t "Goodbye!~%")
         (return))
        ((string= input "")
         ;; Empty line, continue
         )
        (t
         (handler-case
             (let ((result (py-eval input)))
               (unless (null result)
                 (format t "~A~%" result)))
           (error (e)
             (format t "Error: ~A~%" e))))))))

;;; Initialization

(defun initialize-python-cl ()
  "Initialize the Python-CL interpreter"
  (setf *python-environment* (make-py-env))
  (format t "Python-CL ~A initialized~%" 
          (asdf:component-version (asdf:find-system :python-cl))))

;; Initialize on load
(eval-when (:load-toplevel :execute)
  (initialize-python-cl))