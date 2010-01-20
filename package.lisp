(defpackage :tiny-http
  (:use :common-lisp 
	:common-utils
	:puri
	:cl-ppcre 
	:cl+ssl
	:sb-ext
	:sb-bsd-sockets)
  (:shadow "GET")
  (:export :get
	   :post
	   :head
	   :request
	   :*default-unknown-character*))
(in-package :tiny-http)

;;; type, special variable, constant
(deftype octet  ()  '(unsigned-byte 8))
(defvar *default-unknown-character* #\?)
(define-symbol-macro CRLF #.(s #\Return #\Newline))
(defconstant CR-code (char-code #\Return))
(defconstant LF-code (char-code #\Newline))


