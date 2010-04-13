(defsystem tiny-http
  :name "tiny-http"
  :version "0.2.1"
  :author "Takeru Ohta"
  :description "A HTTP client"
  
  :depends-on (:sb-bsd-sockets :cl+ssl :common-utils :puri)

  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "tiny-http-internal")
               (:file "tiny-http")))
