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
