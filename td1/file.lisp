(defclass myfile ()
  ((file :initform nil
	 :initarg :setfile
	 :accessor getfile))
  (:documentation "le type file"))

(defgeneric enfiler(l e))
(defgeneric defiler(l))
(defgeneric filevide(l))
(defgeneric afficher(l))

(defmethod enfiler((l myfile) e)
  (if (endp (getfile l))
      (setf (getfile l) (list e))
      (nconc (getfile l) (list e))))

(defmethod defiler((l myfile))
  (pop (getfile l)))

(defmethod filevide((l myfile)) (endp (getfile l)))

(defmethod afficher((l myfile))
  (print (getfile l)))

(defun creerfile(l)
  (make-instance 'myfile :setfile l))


