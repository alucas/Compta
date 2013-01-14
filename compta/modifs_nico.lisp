(defmacro accept-or-abort (&body body)
   `(handler-case
	 (accept ,@body)
       (error ()
	 (prog1 nil
	   (format t "Bad value; command aborted~%")))))

(defun lessp-by-accessor (obj1 obj2 &rest accessors)
  (loop for f in accessors
	do (progn (when (< (funcall f obj1) (funcall f obj2))
		    (return t))
		  (when (> (funcall f obj1) (funcall f obj2))
		    (return nil)))
	finally (return t)))

(defun date-lessp (date1 date2)
  (lessp-by-accessor date1 date2 #'year #'month #'day #'hour #'minute)  
)