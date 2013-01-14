(in-package :compta-gui)

(defparameter *var* nil)

(define-application-frame compta ()
  ((%organizations :initform (list (make-instance 'organization :name "Default"))
		   :accessor list-organizations))
  
  (:panes (main :application
		:width  (gethash "main-v" *var*)
		:height (gethash "main-h" *var*)
                :display-function 'display-main)
	  (accounts :application
                    :width  (gethash "account-v" *var*)
                    :height (gethash "account-h" *var*)
                    :display-function 'display-accounts)
          (transactions :application
                        :width  (gethash "transaction-v" *var*)
                        :height (gethash "transaction-h" *var*)
                        :display-function 'display-transactions)
	  (organisation :application
                        :width  (gethash "organization-v" *var*)
                        :height (gethash "organization-h" *var*)
                        :display-function 'display-organisation)
	  (inter :interactor
		 :width  (gethash "command-v" *var*)
		 :height (gethash "command-h" *var*)))
  (:layouts (default
		(vertically ()
		  (horizontally () main transactions accounts)
		  (horizontally () inter organisation)))))

(define-presentation-type amount () :inherit-from 'integer)

(define-presentation-method present
    (object (type account) stream (view textual-view) &key)
  (format stream "~a" (name object)))

(define-presentation-method present
    (object (type transaction) stream (view textual-view) &key)
  (format stream "~a" (name object)))


(defclass account-view (view)
  ((%account :initarg :account :reader account)))

(defclass transaction-view (view)
  ((%transaction :initarg :transaction :accessor transaction)))


(defun current-organization (frame)
  (car (list-organizations frame)))

(defun display-inter (form &rest format-arguments)
  (apply #'format (find-pane-named *application-frame* 'inter) form format-arguments))

(defgeneric display-main-with-view (frame pane view))

(defmethod display-main-with-view (frame pane view)
  (declare (ignore frame pane view))
  nil)

(defun format-amount (pane amount format-val)
  (multiple-value-bind (euros cents) (truncate amount 100)
    (let ((medium (sheet-medium pane)))
      (with-text-family (medium :fixed)
	(with-output-as-presentation (pane amount 'amount)
	  (format pane format-val euros (abs cents)))))))

(defclass entry-adder ()
  ((%adder :initarg :adder :reader adder)))

(defun display-main (frame pane)
  (display-main-with-view frame pane (stream-default-view pane)))

(defun compta ()
  (setf *var* (read-param))
  (run-frame-top-level (make-application-frame 'compta)))

; ajout perso
(defun confirm()
  (let ((s (handler-case
	       (accept 'string :prompt "(O ou N) "))))
    (equal s "O")))

(define-compta-command (com-quit :name t :menu "Quit") ()
    (format (find-pane-named *application-frame* 'inter) "Voulez vous vraiment quitter ?~%")
    (when (confirm)
      (frame-exit *application-frame*)))

; exo 2 (facile)
(defun ask-date (pane name min max &optional default)
  (when (null default)
    (setf default 0))
  (let ((val 0))
    (loop 
      do (setf val (accept 'integer :prompt name :default default :insert-default t))
      until (and (<= val max) (>= val min))
      do (format pane "~a must be between ~d and ~d~%" name min max)
      finally (return val))))

;exo 2 (facile) + exo 2 (moyen)
;(defun edit-date (date)
;  (let ((pane (find-pane-named *application-frame* 'inter)))
;    (let ((year   (ask-int pane "Année " 0 (compta-model::current-year) (year date)))
;	  (month  (ask-int pane "Mois " 1 12   (month  date)))
;	  (day    (ask-int pane "Jour " 1 31   (day    date)))
;	  (hour   (ask-int pane "Heure " 0 23  (hour   date)))
;	  (minute (ask-int pane "Minute " 0 59 (minute date))))
;      (setf (year   date) year)
;      (setf (month  date) month)
;      (setf (day    date) day)
;      (setf (hour   date) hour)
;      (setf (minute date) minute))))

(defun edit-date (date)
  (let ((pane (find-pane-named *application-frame* 'inter)))
  (setf *select-date* nil)
  (run-frame-top-level (make-application-frame 'calendar))
  (when (typep *select-date* 'date)
    (let ((year  (year  *select-date*))
	  (month (month *select-date*))
	  (day   (day   *select-date*))
	  (hour  (handler-case
		     (ask-date pane "Heure "  0 23 (hour   date))
		   (error ()
		     (prog1 
			 (display-inter "Bad value; command aborted~%")
		       (return-from edit-date)))))
	  (minute (handler-case
		      (ask-date pane "Minute " 0 59 (minute date))
		    (error ()
		      (prog1 
			  (display-inter "Bad value; command aborted~%")
			(return-from edit-date))))))
      (setf (year   date) year)
      (setf (month  date) month)
      (setf (day    date) day)
      (setf (hour   date) hour)
      (setf (minute date) minute)))))

; exo 2 (moyen)
(define-compta-command (com-edit-date :name t)
    ((date 'date :gesture :nothing))
  (edit-date date))


(define-compta-command (display-graph :name t)
    ()
  (prog1
      (graph-pdf (current-organization *application-frame*) #P"/tmp/graph_compta_fkdeder.pdf")
    (sb-ext:run-program "acroread" '("/tmp/graph_compta_fkdeder.pdf")
		      :search t :wait nil)))

(defmacro accept-or-abort (fonction-name &body body)
   `(handler-case
	 (accept ,@body)
       (error ()
	 (prog1 
	     (display-inter "Bad value; command aborted~%")
	   (return-from ,fonction-name)))))