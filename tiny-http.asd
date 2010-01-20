(defpackage :tiny-http-asd
  (:use :common-lisp :asdf))
(in-package :tiny-http-asd)

(defsystem tiny-http
  :name "tiny-http"
  :version "0.1.8"
  :author "Takeru Ohta"
  :description "a tiny HTTP client"
  
  :depends-on (:sb-bsd-sockets :cl-ppcre :cl+ssl :guess :common-utils :puri)

  :components ((:file "package")
	       (:file "tiny-http"
		      :depends-on ("package" "tiny-http-internal"))
	       (:file "tiny-http-internal"
		      :depends-on ("package"))))

