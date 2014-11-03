;;;; GR-PARSEVAL system definition
;;;; (c) 2014 Grammarly Inc.

(asdf:defsystem #:gr-parseval
  :version "1.0.0"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "MIT"
  :description "Different variants of Parseval constituency parser evaluation metric."
  :depends-on (#:rutilsx #+dev #:should-test)
  :serial t
  :components ((:file "parseval")
               #+dev (:static-file "test-cases.sexp")
               #+dev (:static-file "measurements.txt")
               #+dev (:file "test")))
