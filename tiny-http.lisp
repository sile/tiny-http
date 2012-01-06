(in-package :tiny-http)

(macrolet ((send-block (method &body body)
	     `(progn
		(write-sequence (string-to-octets (HTTP-header ,method host path header)) io)
		,@body
		(finish-output io))))

  (defun get (host path &key (port 80) header (external-format :guess) (force-decode t))
    (with-http-stream (io host port )
      (send-block "GET")                             ; send

      #1=(multiple-value-bind (status header-fields) ; recv
			      (read-header io)
           (values (decode (read-body io header-fields) header-fields external-format force-decode)
		   status
		   header-fields))))

  (defun post (host path body &key (port 80) header (external-format :guess) (force-decode t))
    (with-http-stream (io host port)
      (let ((body (etypecase body 
                    (list           (string-to-octets
				     (with-output-to-string (out)
				       (format out "~{~A~^&~}" 
					       (loop FOR (key value) IN body COLLECT
						     (format nil "~A=~A" key (url-encode value)))))))
                    (string         (string-to-octets body))
		    ((vector octet) body))))
	(setf header `(:content-length ,(length body) ,@header))
	(unless (getf header :content-type)
	  (setf (getf header :content-type) "application/x-www-form-urlencoded"))
	(send-block "POST"                           ; send
	  (write-sequence body io))         
	#1#)))                                       ; recv

  (defun head (host path &key (port 80) header)
    (with-http-stream (io host port)
      (send-block "HEAD")                            ; send
      (read-header io))))                            ; recv

(defun request (uri &key header (external-format :guess) (force-decode t))
  (let* ((o (uri uri))
	 (port (or (uri-port o) 80)))
      (get (uri-host o) (format nil "~:[/~;~:*~A~]~@[?~A~]" (uri-path o) (uri-query o))
	   :port port :header header 
           :external-format external-format :force-decode force-decode)))
