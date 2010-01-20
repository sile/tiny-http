(in-package :tiny-http)

;;; auxiliary
(defun assocdr (key alist) (cdr (assoc key alist)))
(defun kwd (name)          (intern (string-upcase name) :keyword))
(defun header-uniq (headers &optional uniqs)
  (if (endp headers)
      uniqs
    (header-uniq (cddr headers) 
		 (if (getf uniqs (car headers)) 
		     uniqs
		   (append (subseq headers 0 2) uniqs)))))
  
  
(defmacro read-until (octet-stream str &aux (len (length str)))
  `(do ((acc (list #1=(read-byte ,octet-stream)) (cons #1# acc)))
       ((every #'= acc ,(reverse (string-to-octets str)))
	(map 'string #'code-char (nreverse (nthcdr ,len acc))))))

(defun read-octets (input-stream size)
  (let ((buf (make-array size :element-type 'octet)))
    (progn (read-sequence buf input-stream) buf)))

;;; connection and http-stream
(defun http-connect (host &optional (port 80))
  (let ((socket (make-instance 'inet-socket :protocol :tcp :type :stream))
	(addr (host-ent-address (get-host-by-name host)))) 
    (socket-connect socket addr port)
    socket))

(defmacro with-http-stream ((stream host &optional (port 80)) &body body)
  `(let ((,stream (socket-make-stream (http-connect ,host ,port) 
				      :input t :output t :element-type 'octet)))
     (unwind-protect
	 (progn ,@body)
       (close ,stream))))

;;; HTTP header
(defun HTTP-header (method host path &optional header-fields)
  (let ((hs `(,@header-fields :host ,host :user-agent "tiny-http/0.1.5")))
    (s method" "path" HTTP/1.1"CRLF
       (format nil #.(s "~{~A: ~A"CRLF"~}"CRLF) (header-uniq hs)))))

;;; read header
(defun read-header-string (input-stream)
  (read-until input-stream #.(s CRLF CRLF)))

(defun read-header (input-stream)
  (destructuring-bind (first . rest) (split CRLF (read-header-string input-stream))
    (values
     ;; status code
     (parse-integer first :start (position #\Space first) :junk-allowed t)

     ;; header fields
     (mapcar (lm (register-groups-bind (key val) ("^(?s)(.+?): ?(.*)$" $) 
                   (cons (kwd key) val)))
	     rest))))

;;; read body
(defun transfer-encoding (header-fields)
  (cond ((assoc :content-length header-fields)
	 :fixsize)
	((equalp "chunked" (assocdr :transfer-encoding header-fields))
	 :chunked)
	((equalp "close" (assocdr :connection header-fields))
	 :all)
	(t
	 :unknown)))

(defmacro te-case (header-fields &body cases)
  `(ecase (transfer-encoding ,header-fields)
     ,@cases))

;; for chunk1
(defun read-next-chunck-len (input-stream)
  (parse-integer
   (read-until input-stream #.CRLF)
   :radix 16 :junk-allowed t))

;; for chunk2
(defun read-chuncked-content (input-stream)
  (apply #'concatenate '(vector octet)
	 (loop FOR     len = (read-next-chunck-len input-stream)
	       UNTIL   (zerop len)
	       COLLECT (prog1 (read-octets input-stream len)
			 #1=(read-byte input-stream)#|CR|# #1# #|LF|#))))

;; for header no specied content-length and transfer-encoding
;; XXX: slow
(defun read-all (input-stream)
  (coerce
   (loop FOR it = (read-byte input-stream nil nil)
	 WHILE it 
	 COLLECT it)
   '(vector octet)))

(defun read-body (input-stream header-fields)
  (te-case header-fields
    (:fixsize (read-octets input-stream 
                (parse-integer (assocdr :content-length header-fields))))
    (:chunked (read-chuncked-content input-stream))
    (:all     (read-all input-stream))))

;;; decode octets(HTTP body)
(defun octets-to-ascii (octets &optional (unknown-char *default-unknown-character*))
  (map 'string (lm (if (< $ 128) (code-char $) unknown-char)) octets))

;; guess1: from header fields
(defun guess1 (header-fields)
  (a.when (assocdr :content-type header-fields)
    (register-groups-bind (charset) ("(?i)charset=['\" ]*(.+?)(?:[ \"';]|$)" it)
      (kwd charset))))

;; guess2: from meta-tag in html
(defun guess2 (octets &aux (len (length octets)))
  (let ((top500 (octets-to-ascii (subseq octets 0 (min 1024 len)))))
    (register-groups-bind (charset) ("(?i)(?:charset|encoding)=['\" ]*(.+?)[ \"';>]" top500)
      (kwd charset))))

;; guess3: from octets pattern
(defun guess3 (octets)
  (guess:ces-guess-from-vector octets :jp))

(defun guess (octets header-fields)
  (or (guess1 header-fields) (guess2 octets) (guess3 octets) sb-impl::*default-external-format*))

(defun decode (octets header-fields external-format force-decode)
  (if (not force-decode)
      #1=(case external-format
           (:octets octets)
	   (:guess  (octets-to-string octets :external-format (guess octets header-fields)))
	   (t       (octets-to-string octets :external-format external-format)))
      (handler-bind ((sb-impl::octet-decoding-error
		      (lambda (c)
			(use-value *default-unknown-character* c))))
        #1#)))