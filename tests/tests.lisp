(in-package #:binarium.tests)

;;; **************************************************************************
;;;  Generic
;;; **************************************************************************

(defmacro bytes-array (&rest elements)
  `(make-array ,(length elements)
               :element-type '(unsigned-byte 8)
               :initial-contents (list ,@elements)))

(defun read-type (data type)
  (fast-io:with-fast-input (buffer data)
    (read-binary-type type buffer)))

(defun write-type (data type)
  (fast-io:with-fast-output (buffer)
    (write-binary-type type data buffer)))

(define-composite-type a-bunch-of-stuff
  (binarium.types:u8 unsigned-long)
  (binarium.types:s8 signed-long)
  (binarium.types:var-int varint)
  (binarium.types:f4 float)
  (binarium.types:char character)
  (binarium.types:string string)
  (binarium.types:bool boolean)
  (binarium.types:uuid uuid)
  (binarium.types:byte-array byte-array))

(defun gen-boolean ()
  #'(lambda ()
      (when (= 1 (random 2))
        t)))

(defun gen-uuid ()
  #'(lambda ()
      (uuid:make-v4-uuid)))

(defun gen-ascii-char ()
  (gen-character :code-limit (funcall (gen-integer :min 32 :max 255))))

(defun generate-sample-composite-data ()
  (mapcar #'funcall
          (list (gen-integer :min 0 :max 255)
                (gen-integer :min -128 :max 127)
                (gen-integer :min 0 :max 65535)
                (gen-float)
                (gen-ascii-char)
                (gen-string :elements (gen-ascii-char))
                (gen-boolean)
                (gen-uuid)
                (gen-buffer))))

(defparameter *u1*
  `((255 . ,(bytes-array #xFF))))

(defparameter *u2*
  `((48350 . ,(bytes-array #xBC #xDE))))

(defparameter *u4*
  `((2883221305 . ,(bytes-array #xAB #xDA #x77 #x39))))

(defparameter *u8*
  `((1839742453595042431 . ,(bytes-array #x19 #x88 #x14 #x03
                                         #xA0 #xBC #xF2 #x7F))))

(defparameter *u16*
  `((33937258203006218737495434190295007232 . ,(bytes-array #x19 #x88 #x14 #x03
                                                            #xA0 #xBC #xF2 #x7F
                                                            #x04 #x08 #x15 #x16
                                                            #x23 #x42 #x00 #x00))))

(defparameter *composite*
  `(((171
      33
      16786
      2.5992815e38
      #\W
      "TEST STRNG"
      T
      ,(uuid:make-uuid-from-string "C2B4A3F4-5009-46DE-86CB-ACF2D245A2BA")
      ,(bytes-array 191 248 137 38 164 96 179 238 64 47 226 6 118 90 40 154 241 42 208 252 6 143
                    245 178 28 172 74 26 155 8 73 179 255 216 58 170 70 7 12 82 71))
     .
     ,(bytes-array 0 0 0 0 0 0 0 171 0 0 0 0 0 0 0 33 146 131 1 0 0 0 0 127 67 140 87 87 10 84
                   69 83 84 32 83 84 82 78 71 1 194 180 163 244 80 9 70 222 134 203 172 242 210
                   69 162 186 41 191 248 137 38 164 96 179 238 64 47 226 6 118 90 40 154 241 42
                   208 252 6 143 245 178 28 172 74 26 155 8 73 179 255 216 58 170 70 7 12 82 71))))

;;; **************************************************************************
;;;  Decoder
;;; **************************************************************************

(in-suite :binarium.decoding)

(test basic-types-decoding
  (dolist (number *u1*)
    (is (= (car number) (read-type (cdr number) 'binarium.types:u1))))
  (dolist (number *u2*)
    (is (= (car number) (read-type (cdr number) 'binarium.types:u2))))
  (dolist (number *u4*)
    (is (= (car number) (read-type (cdr number) 'binarium.types:u4))))
  (dolist (number *u8*)
    (is (= (car number) (read-type (cdr number) 'binarium.types:u8))))
  (dolist (number *u16*)
    (is (= (car number) (read-type (cdr number) 'binarium.types:u16)))))

(test (composite-types-decoding :depends-on basic-types-decoding)
  (dolist (type *composite*)
    (destructuring-bind (u8 s8 var-int f4 char str bool uuid byte-array)
        (read-type (cdr type) 'a-bunch-of-stuff)
      (destructuring-bind (o-u8 o-s8 o-var-int o-f4 o-char o-str o-bool o-uuid o-byte-array)
          (car type)
        (is (= o-u8 u8))
        (is (= o-s8 s8))
        (is (= o-var-int var-int))
        (is (= o-f4 f4))
        (is (equal o-char char))
        (is (equal o-str str))
        (is (equal o-bool bool))
        (is (uuid:uuid= o-uuid uuid))
        (is (equalp o-byte-array byte-array))))))

;;; **************************************************************************
;;;  Encoder
;;; **************************************************************************

(in-suite :binarium.encoding)

(test basic-types-encoding
  (dolist (number *u1*)
    (is (equalp (cdr number) (write-type (car number) 'binarium.types:u1))))
  (dolist (number *u2*)
    (is (equalp (cdr number) (write-type (car number) 'binarium.types:u2))))
  (dolist (number *u4*)
    (is (equalp (cdr number) (write-type (car number) 'binarium.types:u4))))
  (dolist (number *u8*)
    (is (equalp (cdr number) (write-type (car number) 'binarium.types:u8))))
  (dolist (number *u16*)
    (is (equalp (cdr number) (write-type (car number) 'binarium.types:u16)))))

(test (composite-types-encoding :depends-on basic-types-encoding)
  (dolist (type *composite*)
    (is (equalp (cdr type) (write-type (car type) 'a-bunch-of-stuff)))))
