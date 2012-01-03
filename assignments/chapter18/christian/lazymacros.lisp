(defmacro lazy (&body body)
	(let ((forced (gensym))
		  (value (gensym)))
		`(let ((,forced nil)
			   (,value nil))
			(lambda ()
				(unless ,forced
					(setf ,value (progn ,@body))
					(setf ,forced t))
				,value))))

(defun force (lazy-value)
	(funcall lazy-value))				