;;;; runtime/objects.lisp - Python object model

(in-package #:python-cl)

;;; Python Object System

(defclass py-object ()
  ((type-name :initarg :type-name
              :accessor py-type-name
              :type symbol
              :documentation "Python type name")))

(defclass py-int (py-object)
  ((value :initarg :value
          :accessor py-value
          :type integer
          :initform 0))
  (:default-initargs :type-name 'int))

(defclass py-float (py-object)
  ((value :initarg :value
          :accessor py-value
          :type float
          :initform 0.0))
  (:default-initargs :type-name 'float))

(defclass py-complex (py-object)
  ((value :initarg :value
          :accessor py-value
          :type complex
          :initform #C(0 0)))
  (:default-initargs :type-name 'complex))

(defclass py-str (py-object)
  ((value :initarg :value
          :accessor py-value
          :type string
          :initform ""))
  (:default-initargs :type-name 'str))

(defclass py-list (py-object)
  ((elements :initarg :elements
             :accessor py-elements
             :type list
             :initform nil))
  (:default-initargs :type-name 'list))

(defclass py-dict (py-object)
  ((table :initarg :table
          :accessor py-table
          :initform (make-hash-table :test 'equal)))
  (:default-initargs :type-name 'dict))

(defclass py-function (py-object)
  ((name :initarg :name
         :accessor py-name
         :type symbol)
   (args :initarg :args
         :accessor py-args
         :type list)
   (body :initarg :body
         :accessor py-body
         :type list)
   (closure :initarg :closure
            :accessor py-closure
            :type py-env))
  (:default-initargs :type-name 'function))

;;; Object Construction

(defun make-py-int (value)
  (make-instance 'py-int :value value))

(defun make-py-float (value)
  (make-instance 'py-float :value (float value)))

(defun make-py-complex (value)
  (make-instance 'py-complex :value value))

;;; Type Predicates

(defun py-object-p (obj)
  "Check if object is a Python object"
  (typep obj 'py-object))

(defun py-int-p (obj)
  "Check if object is a Python int"
  (typep obj 'py-int))

(defun py-float-p (obj)
  "Check if object is a Python float"
  (typep obj 'py-float))

(defun py-complex-p (obj)
  "Check if object is a Python complex"
  (typep obj 'py-complex))

(defun py-str-p (obj)
  "Check if object is a Python str"
  (typep obj 'py-str))

(defun py-list-p (obj)
  "Check if object is a Python list"
  (typep obj 'py-list))

(defun py-dict-p (obj)
  "Check if object is a Python dict"
  (typep obj 'py-dict))

(defun make-py-str (value)
  (make-instance 'py-str :value (string value)))

(defun make-py-list (&rest elements)
  (make-instance 'py-list :elements (mapcar #'lisp-to-python elements)))

(defun make-py-dict (&key (alist nil) (plist nil))
  "Create a Python dict from an alist or plist"
  (let ((dict (make-instance 'py-dict)))
    (cond
      (alist
       (dolist (pair alist)
         (setf (gethash (car pair) (py-table dict))
               (lisp-to-python (cdr pair)))))
      (plist
       (loop for (key value) on plist by #'cddr
             do (setf (gethash key (py-table dict))
                      (lisp-to-python value)))))
    dict))

;;; Type Conversion

(defun lisp-to-python (lisp-value)
  "Convert a Lisp value to the appropriate Python object"
  (typecase lisp-value
    (integer (make-py-int lisp-value))
    (float (make-py-float lisp-value))
    (complex (make-py-complex lisp-value))
    (string (make-py-str lisp-value))
    (list (apply #'make-py-list lisp-value))
    (py-object lisp-value)  ; Already a Python object
    (t (error "Cannot convert Lisp value to Python: ~A" lisp-value))))

(defun python-to-lisp (py-object)
  "Convert a Python object to the appropriate Lisp value"
  (etypecase py-object
    (py-int (py-value py-object))
    (py-float (py-value py-object))
    (py-complex (py-value py-object))
    (py-str (py-value py-object))
    (py-list (mapcar #'python-to-lisp (py-elements py-object)))
    (py-dict 
     (let (result)
       (maphash (lambda (key value)
                  (push (cons key (python-to-lisp value)) result))
                (py-table py-object))
       result))))

;;; Truth Testing

(defun py-truthy-p (obj)
  "Python truth testing rules"
  (etypecase obj
    (py-int (not (zerop (py-value obj))))
    (py-float (not (zerop (py-value obj))))
    (py-complex (not (zerop (py-value obj))))
    (py-str (not (string= (py-value obj) "")))
    (py-list (not (null (py-elements obj))))
    (py-dict (not (zerop (hash-table-count (py-table obj)))))
    (null nil)  ; None
    (py-object t)))  ; Default to True for other objects

;;; Object Printing

(defmethod print-object ((obj py-int) stream)
  (format stream "~A" (py-value obj)))

(defmethod print-object ((obj py-float) stream)
  (format stream "~A" (py-value obj)))

(defmethod print-object ((obj py-complex) stream)
  (let ((real (realpart (py-value obj)))
        (imag (imagpart (py-value obj))))
    (cond
      ((zerop real) (format stream "~Aj" imag))
      ((plusp imag) (format stream "(~A+~Aj)" real imag))
      (t (format stream "(~A~Aj)" real imag)))))

(defmethod print-object ((obj py-str) stream)
  (format stream "'~A'" (py-value obj)))

(defmethod print-object ((obj py-list) stream)
  (format stream "[~{~A~^, ~}]" (py-elements obj)))

(defmethod print-object ((obj py-dict) stream)
  (format stream "{")
  (let ((first t))
    (maphash (lambda (key value)
               (unless first
                 (format stream ", "))
               (setf first nil)
               (format stream "'~A': ~A" key value))
             (py-table obj)))
  (format stream "}"))

(defmethod print-object ((obj py-function) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A" (py-name obj))))