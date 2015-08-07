(in-package #:binarium)

;;; **************************************************************************
;;;  Unsigned integer
;;; **************************************************************************

(defclass binarium.types:unsigned-integer (basic-type) ())

(defmacro define-unsigned-integer (name size)
  `(define-binary-type binarium.types:unsigned-integer ,name ,size))

(define-unsigned-integer binarium.types:u1 1)
(define-unsigned-integer binarium.types:u2 2)
(define-unsigned-integer binarium.types:u4 4)
(define-unsigned-integer binarium.types:u8 8)
(define-unsigned-integer binarium.types:u16 16)

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

(define-signed-integer binarium.types:s1 1)
(define-signed-integer binarium.types:s2 2)
(define-signed-integer binarium.types:s4 4)
(define-signed-integer binarium.types:s8 8)
(define-signed-integer binarium.types:s16 16)

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
  (loop with number-length = (integer-length data)
        for i to number-length by 7
        doing (fast-io:fast-write-byte (logior (ldb (byte 7 i) data)
                                               (if (< (+ i 7) number-length)
                                                 (ash 1 7)
                                                 (ash 0 7)))
                                       buffer)))

;;; **************************************************************************
;;;  Floating point numbers
;;; **************************************************************************

(defclass binarium.types:float (basic-type) ())

(defmacro define-float (name size)
  `(define-binary-type binarium.types:float ,name ,size))

(define-float binarium.types:f4 4)
(define-float binarium.types:f8 8)

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
  `(define-binary-array binarium.types:string ,name ,prefix-type binarium.types:u1))

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
