(in-package #:binarium)

;;; **************************************************************************
;;;  Unsigned integer
;;; **************************************************************************

(defclass binarium.types:unsigned-integer (basic-type) ())

(defmacro define-unsigned-integer (name size)
  `(define-binary-type binarium.types:unsigned-integer ,name ,size))

(define-unsigned-integer binarium.types:u8 1)
(define-unsigned-integer binarium.types:u16 2)
(define-unsigned-integer binarium.types:u32 4)
(define-unsigned-integer binarium.types:u64 8)
(define-unsigned-integer binarium.types:u128 16)

(defmethod read-binary-type ((type binarium.types:unsigned-integer) buffer)
  (case (binary-type-size type)
    (1 (fast-io:readu8-be buffer))
    (2 (fast-io:readu16-be buffer))
    (4 (fast-io:readu32-be buffer))
    (8 (fast-io:readu64-be buffer))
    (16 (fast-io:readu128-be buffer))
    (otherwise (error "Non-standard lengths are currently not supported."))))

(defmethod write-binary-type ((type binarium.types:unsigned-integer) (data number) buffer)
  (case (binary-type-size type)
    (1 (fast-io:writeu8-be data buffer))
    (2 (fast-io:writeu16-be data buffer))
    (4 (fast-io:writeu32-be data buffer))
    (8 (fast-io:writeu64-be data buffer))
    (16 (fast-io:writeu128-be data buffer))
    (otherwise (error "Non-standard lengths are currently not supported."))))

;;; **************************************************************************
;;;  Signed integer
;;; **************************************************************************

(defclass binarium.types:signed-integer (basic-type) ())

(defmacro define-signed-integer (name size)
  `(define-binary-type binarium.types:signed-integer ,name ,size))

(define-signed-integer binarium.types:s8 1)
(define-signed-integer binarium.types:s16 2)
(define-signed-integer binarium.types:s32 4)
(define-signed-integer binarium.types:s64 8)
(define-signed-integer binarium.types:s128 16)

(defmethod read-binary-type ((type binarium.types:signed-integer) buffer)
  (case (binary-type-size type)
    (1 (fast-io:read8-be buffer))
    (2 (fast-io:read16-be buffer))
    (4 (fast-io:read32-be buffer))
    (8 (fast-io:read64-be buffer))
    (16 (fast-io:read128-be buffer))
    (otherwise (error "Non-standard lengths are currently not supported."))))

(defmethod write-binary-type ((type binarium.types:signed-integer) (data number) buffer)
  (case (binary-type-size type)
    (1 (fast-io:write8-be data buffer))
    (2 (fast-io:write16-be data buffer))
    (4 (fast-io:write32-be data buffer))
    (8 (fast-io:write64-be data buffer))
    (16 (fast-io:write128-be data buffer))
    (otherwise (error "Non-standard lengths are currently not supported."))))

;;; **************************************************************************
;;;  VarNums
;;; **************************************************************************

(defclass binarium.types:var-num (binarium.types:signed-integer) ())

(defmacro define-var-num (name max-size)
  ;; VarNums have variable size so type value
  ;; stored here represents maximum VarNum size.
  `(define-binary-type binarium.types:var-num ,name ,max-size))

(define-var-num binarium.types:var-int 4)
(define-var-num binarium.types:var-long 8)

(defmethod read-binary-type ((type binarium.types:var-num) buffer)
  (with-slots (size) type
    (let ((bits-len (* 8 size))
          (unsigned 0))
      (loop for i below size
            for pos from 0 by 7
            for byte = (fast-io:fast-read-byte buffer)
            do (setf (ldb (byte 8 pos) unsigned) (ldb (byte 7 0) byte))
            until (zerop (ldb (byte 1 7) byte)))
      (logior unsigned (- (mask-field (byte 1 (1- bits-len)) unsigned))))))

(defmethod write-binary-type ((type binarium.types:var-num) (data number) buffer)
  ;; Adding special handling for zerop is faster and easier
  (if (zerop data)
    (fast-io:fast-write-byte 0 buffer) ; Writing zerop
    (loop with number-length = (integer-length data)
          for i below number-length by 7
          doing (fast-io:fast-write-byte (logior (ldb (byte 7 i) data)
                                                 (if (< (+ i 7) number-length)
                                                   (ash 1 7)
                                                   (ash 0 7)))
                                         buffer))))

;;; **************************************************************************
;;;  Floating point numbers
;;; **************************************************************************

(defclass binarium.types:float (basic-type) ())

(defmacro define-float (name size)
  `(define-binary-type binarium.types:float ,name ,size))

(define-float binarium.types:f32 4)
(define-float binarium.types:f64 8)

(defmethod read-binary-type ((type binarium.types:float) buffer)
  (case (binary-type-size type)
    (4 (ieee-floats:decode-float32 (fast-io:readu32-be buffer)))
    (8 (ieee-floats:decode-float64 (fast-io:readu64-be buffer)))
    (otherwise (error "Non-standard lengths are currently not supported."))))

(defmethod write-binary-type ((type binarium.types:float) (data float) buffer)
  (case (binary-type-size type)
    (4 (fast-io:writeu32-be (ieee-floats:encode-float32 data) buffer))
    (8 (fast-io:writeu64-be (ieee-floats:encode-float64 data) buffer))
    (otherwise (error "Non-standard lengths are currently not supported."))))

;;; **************************************************************************
;;;  Characters
;;; **************************************************************************

(defclass binarium.types:character (basic-type) ())

(defmacro define-character (name size)
  `(define-binary-type binarium.types:character ,name ,size))

(define-character binarium.types:char 1)

(defmethod read-binary-type ((type binarium.types:character) buffer)
  (code-char (fast-io:fast-read-byte buffer)))

(defmethod write-binary-type ((type binarium.types:character) (data character) buffer)
  (fast-io:fast-write-byte (char-code data) buffer))

;;; **************************************************************************
;;;  Strings
;;; **************************************************************************

(defclass binarium.types:string (binary-array) ())

(defmacro define-string (name prefix-type)
  `(define-binary-array binarium.types:string ,name ,prefix-type binarium.types:u8))

(define-string binarium.types:string binarium.types:var-int)

(defmethod read-binary-type ((type binarium.types:string) buffer)
  (with-slots (prefix-type) type
    (let* ((prefix (read-prefix prefix-type buffer))
           (vector (fast-io:make-octet-vector prefix)))
      (fast-io:fast-read-sequence vector buffer 0 prefix)
      (babel:octets-to-string vector))))

(defmethod write-binary-type ((type binarium.types:string) (data string) buffer)
  (write-binary-type type (babel:string-to-octets data) buffer))

;;; **************************************************************************
;;;  Booleans
;;; **************************************************************************

(defclass binarium.types:boolean (basic-type) ())

(defmacro define-boolean (name size)
  `(define-binary-type binarium.types:boolean ,name ,size))

(define-boolean binarium.types:bool 1)

(defmethod read-binary-type ((type binarium.types:boolean) buffer)
  (unless (zerop (aref (call-next-method) 0))
    t))

(defmethod write-binary-type ((type binarium.types:boolean) (data symbol) buffer)
  (check-type data boolean)
  (write-binary-type type (if data 1 0) buffer))

;;; **************************************************************************
;;;  UUID
;;; **************************************************************************

(defclass binarium.types:UUID (basic-type) ())

(defmacro define-uuid (name size)
  `(define-binary-type binarium.types:UUID ,name ,size))

(define-uuid binarium.types:UUID 16)

(defmethod read-binary-type ((type binarium.types:UUID) buffer)
  (uuid:byte-array-to-uuid (call-next-method)))

(defmethod write-binary-type ((type binarium.types:UUID) (data uuid:uuid) buffer)
  (write-binary-type type (uuid:uuid-to-byte-array data) buffer))

;;; **************************************************************************
;;;  Byte array
;;; **************************************************************************

;; This is done though basic type because it's faster this way.

(defclass binarium.types:byte-array (basic-type) ())

(defmacro define-byte-array (name)
  `(define-binary-type binarium.types:byte-array ,name 0))

(define-byte-array binarium.types:byte-array)

(defmethod read-binary-type ((type binarium.types:byte-array) buffer)
  (let* ((prefix (read-binary-type 'bintype:var-int buffer))
         (vector (fast-io:make-octet-vector prefix)))
    (fast-io:fast-read-sequence vector buffer 0 prefix)
    vector))

(defmethod write-binary-type ((type binarium.types:byte-array) (data vector) buffer)
  (write-binary-type 'bintype:var-int (length data) buffer)
  (fast-io:fast-write-sequence data buffer))

;;; **************************************************************************
;;;  Emuns
;;; **************************************************************************

(defclass binarium.types:symbol-map (basic-type)
  ((symbols-map :initarg :symbols-map
                :initform (make-hash-table :test 'equal)
                :documentation "Defines number-to-symbol mappings.")
   (reverse-symbols-map :initarg :reverse-symbols-map
                        :initform (make-hash-table :test 'equal)
                        :documentation "Defines symbol-to-number mappings.")
   (default-symbol :initarg :default-symbol
                   :initform 'binarium.types:undefined)
   (default-binary :initarg :default-binary
                   :initform -1)
   (binary-type :initarg :binary-type
                :initform 'binarium.types:u32
                :documentation "Defines type of binary representation of symbols.")))

(defmethod initialize-instance :after ((instance binarium.types:symbol-map) &rest initargs)
  (declare (ignore initargs))
  (with-slots (symbols-map reverse-symbols-map) instance
    (maphash #'(lambda (key value)
                 (setf (gethash value reverse-symbols-map) key))
             symbols-map)))

(defun (setf symbol->binary) (new-value data map)
  (check-type map binarium.types:symbol-map)
  (with-slots (reverse-symbols-map) map
    (setf (gethash data reverse-symbols-map) new-value)))

(defun (setf binary->symbol) (new-value data map)
  (check-type map binarium.types:symbol-map)
  (with-slots (symbols-map) map
    (setf (gethash data symbols-map) new-value)))

(defun symbol->binary (data map)
  (check-type map binarium.types:symbol-map)
  (with-slots (reverse-symbols-map default-binary) map
    (gethash data reverse-symbols-map default-binary)))

(defun binary->symbol (data map)
  (check-type map binarium.types:symbol-map)
  (with-slots (symbols-map default-symbol) map
    (gethash data symbols-map default-symbol)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-used-binary-values (mappings)
    (loop for mapping in mappings
          when (listp mapping)
            collect (car mapping)))

  (defun get-first-unused-value (counter used-values)
    (if (not (find counter used-values))
      counter
      (get-first-unused-value (1+ counter) used-values)))

  (defun make-mappings-hash-table (mappings &key (start 0))
    (let ((table (make-hash-table :test 'equal))
          (used-values (find-used-binary-values mappings))
          (counter start))
      (dolist (mapping mappings table)
        (typecase mapping
          (symbol
            (setf counter (get-first-unused-value counter used-values))
            (push counter used-values)
            (setf (gethash counter table) mapping))
          (list
            (destructuring-bind (binary symbol) mapping
              (setf (gethash binary table) symbol))))))))

(defmacro define-enum (name (&key (binary-type 'binarium.types:u32)
                                  (default-binary -1)
                                  (default-symbol 'binarium.types:undefined)
                                  (start 0))
                       &body mappings)
  "Defines new enum. The mappins are lists of format: (binary-value symbol) | symbol
If binary value is ommited, it's automatically asssigned.
Be careful: it does not verify that given type can hold all the symbols.
`default-binary' is used when symbol could not be encoded.
`default-symbol' is used when binary data could not be decoded.
The reason these are not conditions is that conditions create unwanted overhead
and this could potentially lead to DOS if somebody decides to send improper
symbols over a wire."
  (check-type default-binary fixnum)
  (check-type default-symbol symbol)
  (check-type start fixnum)
  `(define-binary-type binarium.types:symbol-map ,name (binary-type-size ',binary-type)
     :symbols-map ,(make-mappings-hash-table mappings :start start)
     :binary-type ',binary-type
     ,@(when default-binary (list :default-binary default-binary))
     ,@(when default-symbol (list :default-symbol default-symbol))))

(defmethod read-binary-type ((type binarium.types:symbol-map) buffer)
  (with-slots (binary-type symbols-map) type
    (binary->symbol (read-binary-type binary-type buffer) symbols-map)))

(defmethod write-binary-type ((type binarium.types:symbol-map) (data symbol) buffer)
  (with-slots (binary-type reverse-symbols-map) type
    (write-binary-type binary-type (symbol->binary data reverse-symbols-map) buffer)))
