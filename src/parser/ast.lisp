;;;; ast.lisp - Python AST node definitions

(in-package #:python-cl)

;;; Base AST Node Types

(defclass py-ast-node ()
  ((source-location :initarg :source-location
                    :accessor source-location
                    :initform nil
                    :documentation "Source code location for error reporting")))

(defclass py-expr (py-ast-node)
  ()
  (:documentation "Base class for Python expressions"))

(defclass py-stmt (py-ast-node)
  ()
  (:documentation "Base class for Python statements"))

;;; Expression Nodes

(defclass py-num (py-expr)
  ((value :initarg :value
          :accessor py-value
          :type number
          :documentation "Numeric value (int, float, or complex)")))

(defclass py-str (py-expr)
  ((value :initarg :value
          :accessor py-value
          :type string
          :documentation "String literal value")))

(defclass py-name (py-expr)
  ((id :initarg :id
       :accessor py-id
       :type symbol
       :documentation "Identifier name")))

(defclass py-binop (py-expr)
  ((left :initarg :left
         :accessor py-left
         :type py-expr
         :documentation "Left operand")
   (op :initarg :op
       :accessor py-op
       :type symbol
       :documentation "Binary operator (+, -, *, /, etc.)")
   (right :initarg :right
          :accessor py-right
          :type py-expr
          :documentation "Right operand")))

(defclass py-unaryop (py-expr)
  ((op :initarg :op
       :accessor py-op
       :type symbol
       :documentation "Unary operator (+, -, ~, not)")
   (operand :initarg :operand
            :accessor py-operand
            :type py-expr
            :documentation "Operand expression")))

(defclass py-list-expr (py-expr)
  ((elements :initarg :elements
             :accessor py-elements
             :type list
             :documentation "List of expressions")))

(defclass py-dict-expr (py-expr)
  ((keys :initarg :keys
         :accessor py-keys
         :type list
         :documentation "List of key expressions")
   (values :initarg :values
           :accessor py-values
           :type list
           :documentation "List of value expressions")))

;;; Statement Nodes

(defclass py-assign (py-stmt)
  ((targets :initarg :targets
            :accessor py-targets
            :type list
            :documentation "List of assignment targets")
   (value :initarg :value
          :accessor py-value
          :type py-expr
          :documentation "Value to assign")))

(defclass py-expr-stmt (py-stmt)
  ((value :initarg :value
          :accessor py-value
          :type py-expr
          :documentation "Expression to evaluate")))

(defclass py-function-def (py-stmt)
  ((name :initarg :name
         :accessor py-name
         :type symbol
         :documentation "Function name")
   (args :initarg :args
         :accessor py-args
         :type list
         :documentation "Argument list")
   (body :initarg :body
         :accessor py-body
         :type list
         :documentation "Function body statements")))

(defclass py-return (py-stmt)
  ((value :initarg :value
          :accessor py-value
          :type (or py-expr null)
          :documentation "Return value expression (nil for bare return)")))

(defclass py-if (py-stmt)
  ((test :initarg :test
         :accessor py-test
         :type py-expr
         :documentation "Condition expression")
   (body :initarg :body
         :accessor py-body
         :type list
         :documentation "Statements to execute if true")
   (orelse :initarg :orelse
           :accessor py-orelse
           :type list
           :initform nil
           :documentation "Else clause statements")))

;;; AST Construction Utilities

(defun make-py-num (value &key source-location)
  "Create a numeric literal AST node"
  (make-instance 'py-num :value value :source-location source-location))

(defun make-py-str (value &key source-location)
  "Create a string literal AST node"
  (make-instance 'py-str :value value :source-location source-location))

(defun make-py-bool (value &key source-location)
  "Create a boolean literal AST node"
  (make-instance 'py-bool :value value :source-location source-location))

(defun make-py-name (id &key source-location)
  "Create a name/identifier AST node"
  (make-instance 'py-name :id (if (symbolp id) id (intern (string-upcase id)))
                 :source-location source-location))

(defun make-py-unaryop (op operand &key source-location)
  "Create a unary operation AST node"
  (make-instance 'py-unaryop 
                 :op op 
                 :operand operand
                 :source-location source-location))

(defun make-py-binop (left op right &key source-location)
  "Create a binary operation AST node"
  (make-instance 'py-binop 
                 :left left 
                 :op op 
                 :right right
                 :source-location source-location))

(defun make-py-assign (targets value &key source-location)
  "Create an assignment statement AST node"
  (make-instance 'py-assign 
                 :targets (ensure-list targets)
                 :value value
                 :source-location source-location))

(defun make-py-if (test body orelse &key source-location)
  "Create an if statement AST node"
  (make-instance 'py-if
                 :test test
                 :body body
                 :orelse orelse
                 :source-location source-location))

(defun make-py-expr-stmt (value &key source-location)
  "Create an expression statement AST node"
  (make-instance 'py-expr-stmt 
                 :value value
                 :source-location source-location))

;;; AST Printing for Debugging

(defmethod print-object ((node py-num) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~A" (py-value node))))

(defmethod print-object ((node py-str) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~S" (py-value node))))

(defmethod print-object ((node py-name) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~A" (py-id node))))

(defmethod print-object ((node py-binop) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "(~A ~A ~A)" 
            (py-left node) (py-op node) (py-right node))))