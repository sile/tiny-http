(in-package :tiny-http)

(defun split (delim seq &key (start 0) end limit
                        &aux (len (length delim)))
  (when (and (numberp limit)
             (not (plusp limit)))
    (return-from split '()))

  (loop FOR beg~ = start THEN (+ end~ len)
        FOR end~ = (if (and limit (zerop (decf limit)))
		       nil
		     (search delim seq :start2 beg~ :end2 end))
	UNLESS (= beg~ (or end (length seq)))
          COLLECT (subseq seq beg~ end~)
        WHILE end~))

(defun get-charset (header-value)
  (a.when (search #1="charset=" (string-downcase header-value))
    (read-from-string 
     (string-left-trim "\"' " (first (split ";" header-value :start (+ it (length #1#))))))))

(defun get-encoding (header-value)
  (a.when (search #1="encoding=" (string-downcase header-value))
    (second (split-by-chars "\"' " (subseq header-value (+ it (length #1#)))))))

(deftype quartet () '(unsigned-byte 4))
(deftype octet   () '(unsigned-byte 8))
(deftype simple-octets () '(simple-array octet))

(defun safe-code-p (code)
  (declare (octet code))
  (or (<= (char-code #\a) code (char-code #\z))
      (<= (char-code #\A) code (char-code #\Z))
      (<= (char-code #\0) code (char-code #\9))
      (case code 
        (#.(map 'list #'char-code ".-*_") t))))

;; 速度を除いて、(digit-char digit 16)と等価
(defun digit-hexchar (digit)
  (declare (quartet digit))
  (schar "0123456789ABCDEF" digit))

;; 速度を除いて、(format out "%~2,'0X" code)と等価
(defun write-hex (code out)
  (declare (octet code))
  (write-char #\% out)
  (write-char (digit-hexchar (ldb (byte 4 4) code)) out)
  (write-char (digit-hexchar (ldb (byte 4 0) code)) out))

(defun url-encode (url &optional (#1=external-format :utf8))
  (declare (string url)
	   (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0)))
  (with-output-to-string (out)
    (loop FOR code ACROSS (the simple-octets (sb-ext:string-to-octets url :external-format #1#)) DO
      (cond ((safe-code-p code)           (write-char (code-char code) out))     
            ((= code (char-code #\Space)) (write-char #\+              out))     
            (t                            (write-hex  code             out))))))
