(in-package #:binarium.benchmark)

;; It is pointless to test basic type so I'm going for composite ones.

(defun generate-random-array (n &optional (max-value (expt 2 8)))
  (let ((array (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n array)
      (setf (aref array i) (random max-value)))))

(defun generate-integer (&key min max)
  (+ (random (+ (abs min) (abs max))) min))

(defun generate-float ()
  (random 1.0))

(defun generate-ascii-char ()
  (code-char (+ 32 (random (- 128 32)))))

(defun generate-string (&key (max-length 80))
  (make-string (random max-length)
               :initial-element (generate-ascii-char)))

(defun generate-boolean ()
  (when (zerop (random 2))
    t))

(defun generate-uuid ()
  (uuid:make-v4-uuid))

(binarium:define-composite-type a-bunch-of-stuff
  (binarium.types:u8 unsigned-long)
  (binarium.types:s8 signed-long)
  (binarium.types:var-int varint)
  (binarium.types:f4 float)
  (binarium.types:char character)
  (binarium.types:string string)
  (binarium.types:bool boolean)
  (binarium.types:uuid uuid)
  (binarium.types:byte-array byte-array))

(defun generate-sample-composite-data ()
  (list (generate-integer :min 0 :max 255)
        (generate-integer :min -128 :max 127)
        (generate-integer :min 0 :max 65535)
        (generate-float)
        (generate-ascii-char)
        (generate-string)
        (generate-boolean)
        (generate-uuid)
        (generate-random-array 1024)))

(defun benchmark-composite-types ()
  (let ((encoding-timer (benchmark:make-timer))
        (decoding-timer (benchmark:make-timer)))
    (loop repeat 10000
          for data = (generate-sample-composite-data)
          for encoded = (binarium:with-binary-output (buffer)
                          (benchmark:with-sampling (encoding-timer)
                            (binarium:write-binary-type 'a-bunch-of-stuff data buffer)))
          do (binarium:with-binary-input (buffer encoded)
               (benchmark:with-sampling (decoding-timer)
                 (binarium:read-binary-type 'a-bunch-of-stuff buffer))))
    (format t "Encoding:")
    (benchmark:report encoding-timer)
    (format t "Decoding:")
    (benchmark:report decoding-timer)))

(defun run-benchmarks ()
  (benchmark-composite-types))
