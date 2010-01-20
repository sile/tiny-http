(in-package :tiny-http)

(defun split (delim seq &aux (len (length delim)))
  (declare (simple-string delim seq))
  (loop FOR beg = 0 THEN (+ end len)
	FOR end = (search delim seq :start2 beg)
	COLLECT (subseq seq beg end)
	WHILE end))
