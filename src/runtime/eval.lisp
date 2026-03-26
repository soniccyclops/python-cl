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

;; Names (variable lookup)
(defmethod py-eval-ast ((node py-name) env)
  (py-lookup (py-id node) env))

;; Binary operations
(defmethod py-eval-ast ((node py-binop) env)
  (let ((left (py-eval-ast (py-left node) env))
        (right (py-eval-ast (py-right node) env))
        (op (py-op node)))
    (case op
      (+ (py-add left right))
      (- (py-sub left right))
      (* (py-mul left right))
      (/ (py-div left right))
      (/\/ (py-floordiv left right))
      (% (py-mod left right))
      (** (py-power left right))
      (t (error "Unsupported binary operator: ~A" op)))))

;; Statements
(defmethod py-eval-ast ((node py-assign) env)
  (let ((value (py-eval-ast (py-value node) env)))
    (dolist (target (py-targets node))
      (py-bind (py-id target) value env))
    value))

(defmethod py-eval-ast ((node py-expr-stmt) env)
  (py-eval-ast (py-value node) env))

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