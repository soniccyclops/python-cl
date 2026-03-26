;;;; compiler/builtins.lisp - Python built-in functions

(in-package #:python-cl)

;;; Built-in Functions Implementation

(defun py-print (&rest args)
  "Python print() function"
  (format t "~{~A~^ ~}~%" 
          (mapcar (lambda (arg)
                    (if (stringp arg)
                        arg
                        (format nil "~A" arg)))
                  args))
  nil)  ; Python print() returns None

(defun py-len (obj)
  "Python len() function"
  (etypecase obj
    (py-str (length (py-value obj)))
    (py-list (length (py-elements obj)))
    (py-dict (hash-table-count (py-table obj)))
    (t (error "TypeError: object of type '~A' has no len()" 
              (py-type-name obj)))))

(defun py-type (obj)
  "Python type() function - returns type name"
  (etypecase obj
    (py-object (py-type-name obj))
    (integer 'int)
    (float 'float)
    (complex 'complex)
    (string 'str)
    (list 'list)
    (null 'none)
    (t (type-of obj))))

;;; Type Constructors

(defun py-int-constructor (&optional (value 0))
  "Python int() constructor"
  (etypecase value
    (integer (make-py-int value))
    (float (make-py-int (truncate value)))
    (string 
     (handler-case
         (make-py-int (parse-integer (string-trim '(#\Space #\Tab #\Newline) value)))
       (error ()
         (error "ValueError: invalid literal for int(): '~A'" value))))
    (py-int value)  ; Already an int
    (py-float (make-py-int (truncate (py-value value))))
    (py-str 
     (handler-case
         (make-py-int (parse-integer (string-trim '(#\Space #\Tab #\Newline) (py-value value))))
       (error ()
         (error "ValueError: invalid literal for int(): '~A'" (py-value value)))))))

(defun py-float-constructor (&optional (value 0.0))
  "Python float() constructor"
  (etypecase value
    (number (make-py-float (float value)))
    (string
     (handler-case
         (make-py-float (read-from-string (string-trim '(#\Space #\Tab #\Newline) value)))
       (error ()
         (error "ValueError: could not convert string to float: '~A'" value))))
    (py-int (make-py-float (float (py-value value))))
    (py-float value)  ; Already a float
    (py-str
     (handler-case
         (make-py-float (read-from-string (string-trim '(#\Space #\Tab #\Newline) (py-value value))))
       (error ()
         (error "ValueError: could not convert string to float: '~A'" (py-value value)))))))

(defun py-str-constructor (&optional (value ""))
  "Python str() constructor"
  (etypecase value
    (string (make-py-str value))
    (py-str value)  ; Already a string
    (py-object (make-py-str (format nil "~A" value)))
    (t (make-py-str (format nil "~A" value)))))

(defun py-list-constructor (&optional (iterable nil))
  "Python list() constructor"
  (cond
    ((null iterable) (make-py-list))
    ((listp iterable) (apply #'make-py-list iterable))
    ((py-list-p iterable) iterable)  ; Already a list
    ((py-str-p iterable) 
     ;; String to list of characters
     (make-py-list (map 'list (lambda (c) (make-py-str (string c))) 
                        (py-value iterable))))
    (t (error "TypeError: '~A' object is not iterable" (py-type iterable)))))

;;; Type Predicates

(defun py-int-p (obj)
  "Check if object is a Python int"
  (typep obj 'py-int))

(defun py-float-p (obj)
  "Check if object is a Python float"
  (typep obj 'py-float))

(defun py-str-p (obj)
  "Check if object is a Python string"
  (typep obj 'py-str))

(defun py-list-p (obj)
  "Check if object is a Python list"
  (typep obj 'py-list))

(defun py-dict-p (obj)
  "Check if object is a Python dict"
  (typep obj 'py-dict))

;;; Arithmetic Operations

(defun py-add (left right)
  "Python addition (+) operation"
  (cond
    ;; Number + Number
    ((and (or (py-int-p left) (py-float-p left) (py-complex-p left))
          (or (py-int-p right) (py-float-p right) (py-complex-p right)))
     (let ((left-val (py-value left))
           (right-val (py-value right)))
       (lisp-to-python (+ left-val right-val))))
    
    ;; String + String (concatenation)
    ((and (py-str-p left) (py-str-p right))
     (make-py-str (concatenate 'string (py-value left) (py-value right))))
    
    ;; List + List (concatenation)
    ((and (py-list-p left) (py-list-p right))
     (make-instance 'py-list 
                    :elements (append (py-elements left) (py-elements right))))
    
    (t (error "TypeError: unsupported operand type(s) for +: '~A' and '~A'"
              (py-type left) (py-type right)))))

(defun py-sub (left right)
  "Python subtraction (-) operation"
  (cond
    ((and (or (py-int-p left) (py-float-p left) (py-complex-p left))
          (or (py-int-p right) (py-float-p right) (py-complex-p right)))
     (let ((left-val (py-value left))
           (right-val (py-value right)))
       (lisp-to-python (- left-val right-val))))
    
    (t (error "TypeError: unsupported operand type(s) for -: '~A' and '~A'"
              (py-type left) (py-type right)))))

(defun py-mul (left right)
  "Python multiplication (*) operation"
  (cond
    ;; Number * Number
    ((and (or (py-int-p left) (py-float-p left) (py-complex-p left))
          (or (py-int-p right) (py-float-p right) (py-complex-p right)))
     (let ((left-val (py-value left))
           (right-val (py-value right)))
       (lisp-to-python (* left-val right-val))))
    
    ;; String * Integer (repetition)
    ((and (py-str-p left) (py-int-p right))
     (let ((count (py-value right)))
       (if (>= count 0)
           (make-py-str (apply #'concatenate 'string 
                               (make-list count :initial-element (py-value left))))
           (make-py-str ""))))
    
    ;; Integer * String (repetition)
    ((and (py-int-p left) (py-str-p right))
     (py-mul right left))
    
    ;; List * Integer (repetition)
    ((and (py-list-p left) (py-int-p right))
     (let ((count (py-value right)))
       (if (>= count 0)
           (make-instance 'py-list
                          :elements (apply #'append 
                                           (make-list count :initial-element (py-elements left))))
           (make-py-list))))
    
    ;; Integer * List (repetition)
    ((and (py-int-p left) (py-list-p right))
     (py-mul right left))
    
    (t (error "TypeError: unsupported operand type(s) for *: '~A' and '~A'"
              (py-type left) (py-type right)))))

(defun py-div (left right)
  "Python division (/) operation - always returns float"
  (cond
    ((and (or (py-int-p left) (py-float-p left) (py-complex-p left))
          (or (py-int-p right) (py-float-p right) (py-complex-p right)))
     (let ((left-val (py-value left))
           (right-val (py-value right)))
       (when (zerop right-val)
         (error "ZeroDivisionError: division by zero"))
       (lisp-to-python (/ left-val right-val))))
    
    (t (error "TypeError: unsupported operand type(s) for /: '~A' and '~A'"
              (py-type left) (py-type right)))))

(defun py-floordiv (left right)
  "Python integer division (//) operation"
  (cond
    ((and (or (py-int-p left) (py-float-p left))
          (or (py-int-p right) (py-float-p right)))
     (let ((left-val (py-value left))
           (right-val (py-value right)))
       (when (zerop right-val)
         (error "ZeroDivisionError: integer division or modulo by zero"))
       (lisp-to-python (floor left-val right-val))))
    
    (t (error "TypeError: unsupported operand type(s) for //: '~A' and '~A'"
              (py-type left) (py-type right)))))

(defun py-mod (left right)
  "Python modulo (%) operation"
  (cond
    ((and (or (py-int-p left) (py-float-p left))
          (or (py-int-p right) (py-float-p right)))
     (let ((left-val (py-value left))
           (right-val (py-value right)))
       (when (zerop right-val)
         (error "ZeroDivisionError: integer division or modulo by zero"))
       (lisp-to-python (mod left-val right-val))))
    
    (t (error "TypeError: unsupported operand type(s) for %: '~A' and '~A'"
              (py-type left) (py-type right)))))

(defun py-power (left right)
  "Python exponentiation (**) operation"
  (cond
    ((and (or (py-int-p left) (py-float-p left) (py-complex-p left))
          (or (py-int-p right) (py-float-p right) (py-complex-p right)))
     (let ((left-val (py-value left))
           (right-val (py-value right)))
       (lisp-to-python (expt left-val right-val))))
    
    (t (error "TypeError: unsupported operand type(s) for **: '~A' and '~A'"
              (py-type left) (py-type right)))))

;;; Unary Operations

(defun py-neg (operand)
  "Python unary minus (-) operation"
  (cond
    ((or (py-int-p operand) (py-float-p operand) (py-complex-p operand))
     (lisp-to-python (- (py-value operand))))
    
    (t (error "TypeError: bad operand type for unary -: '~A'"
              (py-type operand)))))

(defun py-pos (operand)
  "Python unary plus (+) operation"
  (cond
    ((or (py-int-p operand) (py-float-p operand) (py-complex-p operand))
     operand)  ; No change for positive numbers
    
    (t (error "TypeError: bad operand type for unary +: '~A'"
              (py-type operand)))))

(defun py-invert (operand)
  "Python bitwise NOT (~) operation"
  (cond
    ((py-int-p operand)
     (lisp-to-python (lognot (py-value operand))))
    
    (t (error "TypeError: bad operand type for unary ~: '~A'"
              (py-type operand)))))

(defun py-not (operand)
  "Python logical NOT operation"
  (lisp-to-python (if (py-truthy-p operand) nil t)))

;;; Comparison Operations

(defun py-eq (left right)
  "Python equality (==) comparison"
  (cond
    ((and (py-object-p left) (py-object-p right))
     (and (eq (class-of left) (class-of right))
          (equal (py-value left) (py-value right))))
    ((equal left right) t)
    (t nil)))

(defun py-ne (left right)
  "Python inequality (!=) comparison"
  (not (py-eq left right)))

(defun py-lt (left right)
  "Python less-than (<) comparison"
  (cond
    ((and (or (py-int-p left) (py-float-p left))
          (or (py-int-p right) (py-float-p right)))
     (< (py-value left) (py-value right)))
    
    ((and (py-str-p left) (py-str-p right))
     (string< (py-value left) (py-value right)))
    
    (t (error "TypeError: unsupported operand type(s) for <: '~A' and '~A'"
              (py-type left) (py-type right)))))

;;; Helper functions

(defun py-object-p (obj)
  "Check if object is a Python object"
  (typep obj 'py-object))