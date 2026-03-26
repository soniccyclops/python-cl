;;;; runtime/eval.lisp - Expression evaluation engine

(in-package #:python-cl)

;;; Expression Evaluation

(defgeneric py-eval-ast (node env)
  (:documentation "Evaluate a Python AST node in the given environment"))

;; Literals
(defmethod py-eval-ast ((node py-num) env)
  (lisp-to-python (py-value node)))

(defmethod py-eval-ast ((node py-str) env)
  (make-py-str (py-value node)))

(defmethod py-eval-ast ((node py-bool) env)
  (make-py-bool (py-value node)))

;; Names (variable lookup)
(defmethod py-eval-ast ((node py-name) env)
  (py-lookup (py-id node) env))

;; Binary operations
(defmethod py-eval-ast ((node py-binop) env)
  (let ((op (py-op node)))
    (case op
      ;; Short-circuiting boolean operations
      (AND
       (let ((left (py-eval-ast (py-left node) env)))
         (if (py-truthy-p left)
             (py-eval-ast (py-right node) env)  ; Return right value
             left)))  ; Return left (falsy) value
      
      (OR  
       (let ((left (py-eval-ast (py-left node) env)))
         (if (py-truthy-p left)
             left  ; Return left (truthy) value
             (py-eval-ast (py-right node) env))))  ; Return right value
      
      ;; Regular binary operations (evaluate both sides)
      (t
       (let ((left (py-eval-ast (py-left node) env))
             (right (py-eval-ast (py-right node) env)))
         (case op
           ;; Arithmetic operators
           (+ (py-add left right))
           (- (py-sub left right))
           (* (py-mul left right))
           (/ (py-div left right))
           (/\/ (py-floordiv left right))
           (% (py-mod left right))
           (** (py-power left right))
           
           ;; Comparison operators
           (< (py-lt left right))
           (<= (py-le left right))
           (> (py-gt left right))
           (>= (py-ge left right))
           (== (py-eq left right))
           (!= (py-ne left right))
           
           ;; Bitwise operators
           (& (py-bitwise-and left right))
           (\| (py-bitwise-or left right))
           (^ (py-bitwise-xor left right))
           (<< (py-left-shift left right))
           (>> (py-right-shift left right))
           
           (t (error "Unsupported binary operator: ~A" op))))))))

;; Unary operations
(defmethod py-eval-ast ((node py-unaryop) env)
  (let ((operand (py-eval-ast (py-operand node) env))
        (op (py-op node)))
    (case op
      (+ operand)  ; Unary plus (no-op for numbers)
      (- (py-neg operand))  ; Unary minus
      (~ (py-invert operand))  ; Bitwise NOT
      (NOT (py-not operand))   ; Logical NOT
      (t (error "Unsupported unary operator: ~A" op)))))

;; Statements
(defmethod py-eval-ast ((node py-assign) env)
  (let ((value (py-eval-ast (py-value node) env)))
    (dolist (target (py-targets node))
      (py-bind (py-id target) value env))
    value))

(defmethod py-eval-ast ((node py-aug-assign) env)
  (let ((target-name (py-id (py-target node)))
        (new-value (py-eval-ast (py-value node) env)))
    ;; Get current value
    (let ((current-value (py-lookup target-name env)))
      ;; Apply the operation
      (let ((result (case (py-op node)
                      (:+ (py-add current-value new-value))
                      (:- (py-sub current-value new-value))
                      (:* (py-mul current-value new-value))
                      (:/ (py-div current-value new-value))
                      (:/\/ (py-floordiv current-value new-value))
                      (:% (py-mod current-value new-value))
                      (:** (py-pow current-value new-value))
                      (t (error "Unknown augmented assignment operator: ~A" (py-op node))))))
        ;; Bind the result
        (py-bind target-name result env)
        result))))

(defmethod py-eval-ast ((node py-if) env)
  (let ((test-result (py-eval-ast (py-test node) env)))
    (if (py-truthy-p test-result)
        ;; Execute body statements
        (let (last-result)
          (dolist (stmt (py-body node))
            (setf last-result (py-eval-ast stmt env)))
          (or last-result (make-py-none)))  ; Return None if no statements
        ;; Execute else clause if it exists
        (if (py-orelse node)
            (let (last-result)
              (dolist (stmt (py-orelse node))
                (setf last-result (py-eval-ast stmt env)))
              (or last-result (make-py-none)))
            (make-py-none)))))  ; Return None if condition is false and no else

(defmethod py-eval-ast ((node py-while) env)
  (let (last-result)
    (catch 'break
      (loop while (py-truthy-p (py-eval-ast (py-test node) env))
            do (catch 'continue
                 (dolist (stmt (py-body node))
                   (setf last-result (py-eval-ast stmt env))))))
    (or last-result (make-py-none))))  ; Return None if loop never executed

(defmethod py-eval-ast ((node py-function-def) env)
  (let ((func-obj (make-py-function (py-name node) (py-args node) (py-body node))))
    (py-bind (py-name node) func-obj env)
    func-obj))

(defmethod py-eval-ast ((node py-return) env)
  (if (py-value node)
      (throw 'return (py-eval-ast (py-value node) env))
      (throw 'return (make-py-none))))

(defmethod py-eval-ast ((node py-break) env)
  (throw 'break nil))

(defmethod py-eval-ast ((node py-continue) env)
  (throw 'continue nil))

(defmethod py-eval-ast ((node py-call) env)
  (let ((func (py-eval-ast (py-func node) env))
        (args (mapcar (lambda (arg) (py-eval-ast arg env)) (py-args node))))
    (py-call-function func args env)))

(defmethod py-eval-ast ((node py-expr-stmt) env)
  (py-eval-ast (py-value node) env))

;;; Function Calls

(defun py-call-function (func args env)
  "Call a Python function with arguments"
  (etypecase func
    (py-function (py-call-user-function func args env))
    (t (error "TypeError: '~A' object is not callable" (py-type-name func)))))

(defun py-call-user-function (func args env)
  "Call a user-defined Python function"
  (let ((func-args (py-args func))
        (func-body (py-body func)))
    ;; Check argument count
    (unless (= (length args) (length func-args))
      (error "TypeError: ~A() takes ~A arguments but ~A were given"
             (py-name func) (length func-args) (length args)))
    
    ;; Create new environment for function scope
    (let ((func-env (make-py-env :enclosing env)))
      ;; Bind arguments to parameters
      (loop for arg in args
            for param in func-args
            do (py-bind param arg func-env))
      
      ;; Execute function body with return handling
      (catch 'return
        (let (last-result)
          (dolist (stmt func-body)
            (setf last-result (py-eval-ast stmt func-env)))
          ;; Return None if no explicit return
          (or last-result (make-py-none)))))))

(defmethod py-eval-ast ((node py-return) env)
  ;; TODO: Implement proper return handling with conditions
  (if (py-value node)
      (py-eval-ast (py-value node) env)
      nil))

;;; Statement Execution

(defun py-exec-stmt (stmt env)
  "Execute a Python statement in the given environment"
  (py-eval-ast stmt env))

(defun py-exec-stmts (stmts env)
  "Execute a list of Python statements"
  (let (result)
    (dolist (stmt stmts result)
      (setf result (py-exec-stmt stmt env)))))

;;; Module Execution

(defun py-exec-module (stmts)
  "Execute Python statements as a module (top-level)"
  (let ((module-env (make-py-env)))
    (py-exec-stmts stmts module-env)))