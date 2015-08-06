(in-package #:binarium.tests)

(def-suite :binarium
           :description "All binarium tests.")

(def-suite :binarium.encoding
           :description "Types encoder tests."
           :in :binarium)

(def-suite :binarium.decoding
           :description "Types decoder tests."
           :in :binarium)
