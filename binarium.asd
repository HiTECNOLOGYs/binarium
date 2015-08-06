(defsystem #:binarium
  :author "Mark Fedurin <hitecnologys@gmail.com>"
  :description "Network binary types made easy."
  :depends-on (#:alexandria  ; General toolkit
               #:fast-io     ; I/O
               #:babel       ; Strings
               #:ieee-floats ; IEEE floating point numbers
               #:uuid        ; UUIDs
               )
  :serial t
  :pathname "src/"
  :components ((:file "packages")
               (:file "binarium")
               (:file "basic-types")))

(defsystem #:binarium/tests
  :author "Mark Fedurin <hitecnologys@gmail.com>"
  :description "Unit-tests for binarium."
  :depends-on (#:binarium ; Target
               #:fiveam   ; Unit-testing
               )
  :serial t
  :pathname "tests/"
  :components ((:file "packages")
               (:file "suites")
               (:file "tests")))
