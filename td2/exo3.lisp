(defclass participant()
    ((nom :accessor nom
	  :initform nil
	  :initarg :nom)))

(defclass personnage(participant)
  ((age :accessor age
	:initform 0
	:initarg :age)))

(defclass heros(personnage)
  ((force :accessor force
	  :initform 0
	  :initarg :force)))

(defgeneric taille(p)
  (:method-combination +))

(defmethod taille + ((p participant))
  (length (nom p)))
      
(defmethod taille + ((p personnage))
  1)

(defmethod taille + ((p heros))
  1)


