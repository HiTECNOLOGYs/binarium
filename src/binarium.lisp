(in-package #:binarium)

;;; **************************************************************************
;;;  Foundation
;;; **************************************************************************

;; ----------------
;; Basic classes, functions and macros

(defvar *binary-types* (make-hash-table)
  "Stores available types.")

(defclass binary-type ()
  ((name :initarg :name
         :accessor binary-type-name)
   (size :initarg :size
         :accessor binary-type-size))
  (:documentation "Basic atomic binary type."))

(defclass binary-array ()
  ((prefix-type :initarg :prefix-type
                :accessor binary-array-prefix-type)
   (element-type :initarg :element-type
                 :accessor binary-array-element-type))
  (:documentation "An array of binary type of type `element-type'.
Length is determined by element of type `prefix-type'."))

(defun get-type (name)
  "Returns type by its name."
  (gethash name *binary-types*))

(defun (setf get-type) (new-value name)
  "Sets type to new one or adds new type. Checks that supplied value is heir of
BINARY-TYPE class as well."
  (check-type new-value (or binary-type binary-array))
  (setf (gethash name *binary-types*) new-value))

(defmethod binary-type-size ((type symbol))
  (binary-type-size (get-type type)))

(defgeneric write-binary-type (type data buffer)
  (:documentation "Writes data of given `type' to supplised `buffer'.")
  (:method ((type (eql nil)) data buffer)
   (error "NIL is not a type!"))
  (:method ((type symbol) data buffer)
   (write-binary-type (get-type type) data buffer)))

(defgeneric read-binary-type (type buffer)
  (:documentation "Reads binary type of `type' from given `buffer'.")
  (:method ((type (eql nil)) buffer)
   (error "NIL is not a type!"))
  (:method ((type symbol) buffer)
   (read-binary-type (get-type type) buffer)))

(defmacro define-binary-type (type name size &body arguments)
  "Defines new binary type."
  `(setf (get-type ',name) (make-instance ',type
                                          :name ',name
                                          :size ,size
                                          ,@arguments)))

(defmacro define-binary-array (type name prefix-type element-type &body arguments)
  "Defines new binary array."
  `(setf (get-type ',name) (make-instance ',type
                                          :prefix-type ',prefix-type
                                          :element-type ',element-type
                                          ,@arguments)))

;;; **************************************************************************
;;;  Classes
;;; **************************************************************************

(defclass basic-type (binary-type) ()
  (:documentation "Basic class for binary types."))

(defclass composite-type (binary-type)
  ((structure :initarg :structure
              :accessor composite-type-structure))
  (:documentation "A type that is composed of one or more basic types."))

;;; **************************************************************************
;;;  Basic types
;;; **************************************************************************

(defun read-prefix (type buffer)
  "Reads array length prefix and returns length."
  (read-binary-type type buffer))

(defmethod read-binary-type ((type basic-type) buffer)
  (with-slots (size) type
    (let ((vector (fast-io:make-octet-vector size)))
      (fast-io:fast-read-sequence vector buffer 0 size)
      vector)))

(defmethod read-binary-type ((type binary-array) buffer)
  (with-slots (prefix-type element-type) type
    (loop repeat (read-prefix prefix-type buffer)
          collecting (read-binary-type element-type buffer))))

(defmethod write-binary-type ((type basic-type) (data fixnum) buffer)
  (check-type data (unsigned-byte 8))
  (fast-io:fast-write-byte data buffer))

(defmethod write-binary-type ((type binary-array) (data fixnum) buffer)
  (check-type data (unsigned-byte 8))
  (fast-io:fast-write-byte data buffer))

(defmethod write-binary-type ((type basic-type) (data vector) buffer)
  (fast-io:fast-write-sequence data buffer))

(defmethod write-binary-type ((type binary-array) (data vector) buffer)
  (write-binary-type (binary-array-prefix-type type) (length data) buffer)
  (fast-io:fast-write-sequence data buffer))

;;; **************************************************************************
;;;  Composite types
;;; **************************************************************************

;; TODO Implement support for non-fixed-sized type (i.e. with arrays)
(defun caclculate-composite-type-size (type)
  "Calculates composite type size by summing its basic type sizes."
  (reduce #'+ (composite-type-structure type)
          :key #'binary-type-size))

(defmethod initialize-instance :after ((instance composite-type) &rest initargs)
  (declare (ignore initargs))
  (unless (slot-boundp instance 'structure)
    (setf (slot-value instance 'size) (caclculate-composite-type-size instance))))

(define-condition invalid-structure (error)
  ((required-structure :initarg :required-structure)
   (given-data :initarg :given-data))
  (:documentation "Signaled when incorrect composite type structure is
encountered.")
  (:report
    (lambda (condition stream)
      (with-slots (required-data given-data) condition
        (format stream "Was expecting: ~A~%Got: ~A"
                required-data given-data)))))

(defmacro define-composite-type (name &body fields)
  "Defines new composite type.
The structure of field is the following: (type name)
Currenrly the name has no use but it will be made use of."
  `(progn
     (defclass ,name ()
       ,(mapcar #'second fields))
     (define-binary-type composite-type ,name nil
       :structure ',(mapcar #'first fields))))

(defmethod write-binary-type :before ((type composite-type) (data list) buffer)
  (with-slots (structure) type
    (unless (= (length structure) (length data))
      (error 'invalid-structure
             :required-structure structure
             :given-data data))))

(defmethod write-binary-type ((type composite-type) (data list) buffer)
  (loop for field in (composite-type-structure type)
        for value in data
        doing (write-binary-type field value buffer)))

(defmethod read-binary-type ((type composite-type) buffer)
  (mapcar #'(lambda (field)
              (read-binary-type field buffer))
          (composite-type-structure type)))

;;; **************************************************************************
;;;  Convenience macros
;;; **************************************************************************

(defmacro with-binary-input ((buffer data) &body body)
  "Creates and binds buffer with data to given variables and executes body with
this binding."
  `(fast-io:with-fast-input (,buffer ,data)
     ,@body))

(defmacro with-binary-output ((buffer) &body body)
  "Creates and binds buffer to given variables and executes body with
this binding. Afterwards, returns data written to buffer."
  `(fast-io:with-fast-output (,buffer)
     ,@body))
