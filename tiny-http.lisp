(in-package :tiny-http)

(macrolet ((with-http-or-https-stream ((io host port ssl) &body body)
             `(with-http-stream (,io ,host ,port)
                (when ,ssl (setf ,io (cl+ssl:make-ssl-client-stream ,io)))
		,@body))
	   
	   (send-block (method &body body)
	     `(progn
		(write-sequence (string-to-octets (HTTP-header ,method host path header)) io)
		,@body
		(finish-output io))))

  (defun get (host path &key ssl (port (if ssl 443 80)) header (external-format :guess) (force-decode t))
    (with-http-or-https-stream (io host port ssl)
      (send-block "GET")                             ; send

      #1=(multiple-value-bind (status header-fields) ; recv
			      (read-header io)
           (values (decode (read-body io header-fields) header-fields external-format force-decode)
		   status
		   header-fields))))

  (defun post (host path body &key ssl (port (if ssl 443 80)) header (external-format :guess) (force-decode t))
    (with-http-or-https-stream (io host port ssl)
      (let ((body (etypecase body 
                    (string         (string-to-octets body))
		    ((vector octet) body))))
	(setf header `(:content-length ,(length body) ,@header))
	(send-block "POST"                           ; send
	  (write-sequence body io))         
	#1#)))                                       ; recv

  (defun head (host path &key ssl (port (if ssl 443 80)) header)
    (with-http-or-https-stream (io host port ssl)
      (send-block "HEAD")                            ; send
      (read-header io))))                            ; recv

(defun request (uri &key header (external-format :guess) (force-decode t))
  (let* ((o (uri uri))
	 (ssl (ecase (uri-scheme o) (:http nil) (:https t)))
	 (port (or (uri-port o) (if ssl 443 80))))
      (get (uri-host o) (format nil "~:[/~;~:*~A~]~@[?~A~]" (uri-path o) (uri-query o))
	   :port port :ssl ssl :header header 
           :external-format external-format :force-decode force-decode)))