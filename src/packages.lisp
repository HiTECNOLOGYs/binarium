(defpackage #:binarium
  (:documentation "Core package. Contains all the necessary functions to work
with types and/or define your own.")
  (:use #:cl
        #:alexandria)

  ;; Base classes
  (:export #:binary-type      ; Class
           #:name             ; Class slot
           #:size             ; Class slot
           #:binary-type-name ; Class slot accessor
           #:binary-type-size ; Class slot accessor
           #:binary-array     ; Class
           )

  ;; Base generic functions
  (:export #:read-binary-type  ; Generic function
           #:write-binary-type ; Read generic function
           )

  ;; Base macros
  (:export #:define-binary-type  ; Macro
           #:define-binary-array ; Macro
           )

  ;; Base types
  (:export #:basic-type            ; Class
           #:composite-type        ; Class
           #:invalid-structure     ; Condition
           #:define-composite-type ; Macro
           )

  ;; Convenience functions/macros
  (:export #:with-binary-input  ; Macros
           #:with-binary-output ; Macros
           )
  )

(defpackage #:binarium.types
  (:nicknames #:bintype)
  (:documentation "Holds basic binarium types. This package is necessary due to
collisions with some symbols in CL package and for the sake of consistency.")
  (:use)

  ;; Classes
  (:export #:unsigned-integer ; Class
           #:signed-integer   ; Class
           #:var-num          ; Class
           #:float            ; Class
           #:character        ; Class
           #:string           ; Class
           #:boolean          ; Class
           #:uuid             ; Class
           #:byte-array       ; Class
           #:symbol-map       ; Class
           )

  ;; Unsigned integers
  (:export #:u1  ; Symbol
           #:u2  ; Symbol
           #:u4  ; Symbol
           #:u8  ; Symbol
           #:u16 ; Symbol
           )

  ;; Signed integers
  (:export #:s1  ; Symbol
           #:s2  ; Symbol
           #:s4  ; Symbol
           #:s8  ; Symbol
           #:s16 ; Symbol
           )

  ;; VarNums (https://developers.google.com/protocol-buffers/docs/encoding#varints)
  (:export #:var-int  ; Symbol
           #:var-long ; Symbol
           )

  ;; Floating point numbers
  (:export #:f4 ; Symbol
           #:f8 ; Symbol
           )

  ;; Characters
  (:export #:char ; Symbol
           )

  ;; Strings
  (:export #:string ; Symbol
           )

  ;; Booleans
  (:export #:bool ; Symbol
           )

  ;; UUIDs
  (:export #:uuid ; Symbol
           )

  ;; Byte arrays
  (:export #:byte-array ; Symbol
           )

  ;; Enums
  (:export #:enum        ; Symbol
           #:undefined   ; Symbol
           #:define-enum ; Macro
           )
  )
