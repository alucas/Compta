(defgeneric date-naissance(p))

(defclass personne()
   ((age :accessor age)))

(defun creepersonne(d)
  (make-instance 'personne :date-naissance d))

(defmethod initialize-instance :after ((p personne) &rest initargs &key date-naissance &allow-other-keys)
  (setf (age p) (- (get-universal-time) date-naissance)))

(defmethod date-naissance((p personne))
  (- (get-universal-time) (age p)))
  