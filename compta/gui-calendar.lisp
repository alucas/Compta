(in-package :compta-gui)

(define-application-frame calendar ()
  ()
  (:panes (main :application
		:width 300
		:height 220
		:display-function 'display-calendar))
  (:layouts (default main)))

(defparameter *day-string* '#("Sun" "Mon" "Tue" "Wed"
			      "Thu" "Fri" "Sat"))

(defparameter *month-string* '#("December"  "January" "February" 
				"March"     "April"   "May"
				"June"      "July"    "August"
				"September" "October" "November"
				"December"  "January"))

(defvar *month-length* '#(0 31 28 31 30 31 30 31 31 30 31 30 31))

(defun leap-year-p (year)
  (cond ((and (integerp (/ year 100))  ; <= O_o
	      (integerp (/ year 400))) ; 
	 t)
	((and (not (integerp (/ year 100)))
	      (integerp (/ year 4)))
	 t)
	(t nil)))

(defun month-length (month year)
  (let ((days (aref *month-length* month)))
    (when (and (eql month 2)
	       (leap-year-p year))
      (incf days))
    days))

(defclass select-year()
  ((year :initarg :select-year :reader select-year)))

(defclass select-month()
  ((month :initarg :select-month :reader select-month)))

(defun calendar-month (month year &key (stream *standard-output*))
  (let ((days-in-month (month-length month year)))
    (format stream "~a ~D :~%~%" (aref *month-string* month) year)
    (multiple-value-bind (sec min hour date month2 year2 start-day)
	(decode-universal-time (encode-universal-time
				0 0 0 1 month year))
      (declare (ignore sec min hour date month2 year2))
      (setq start-day (mod (+ start-day 1) 7))
      (formatting-table (stream)
	(formatting-row (stream)
	  (dotimes (d 7)
	    (formatting-cell (stream :align-x :center)
	      (format stream (aref *day-string* d)))))
	(do ((date 1)
	     (first-week t nil))
	    ((> date days-in-month))
	  (formatting-row (stream)
	    (dotimes (d 7)
	      (formatting-cell (stream :align-x :right)
		(when (and (<= date days-in-month)
			   (or (not first-week)
			       (>= d start-day)))
		  (let ((inst-date (make-instance 'date :year year :month month :day date)))
		    (with-output-as-presentation (stream inst-date 'date)
		      (format stream "~D" date)))
		  (incf date)))))))

      (let ((year-1 (make-instance 'select-year :select-year (1- year)))
	    (year+1 (make-instance 'select-year :select-year (1+ year)))
	    (month-1 (make-instance 'select-month :select-month (1- month)))
	    (month+1 (make-instance 'select-month :select-month (1+ month))))
	(with-output-as-presentation (stream month-1 'select-month)
	  (format stream "~%<<"))
	(format stream " ~a / ~a "
		(aref *month-string* (1- month))
		(aref *month-string* (1+ month)))
	(with-output-as-presentation (stream month+1 'select-month)
	  (format stream ">>~%"))
	(with-output-as-presentation (stream year-1 'select-year)
	  (format stream "<<"))
	(format stream " ~a / ~a "
		(1- year)
		(1+ year))
	(with-output-as-presentation (stream year+1 'select-year)
	  (format stream ">>"))
	))))

(defparameter *select-date* nil)
(defparameter *select-year* nil)
(defparameter *select-month* nil)

(defun display-calendar (frame pane)
  (declare (ignore frame))
  (when (or
	 (null *select-month*)
	 (null *select-year*))
    (multiple-value-bind (sec min hour date month year start-day)
	(decode-universal-time (get-universal-time))
      (declare (ignore sec min hour date start-day))
      (setf *select-month* month)
      (setf *select-year*  year)))
  (calendar-month *select-month* *select-year* :stream pane))

;(define-compta-command (com-open-calendar :name t :menu "Calendar")()
;  (setf *select-date* nil)
;  (run-frame-top-level (make-application-frame 'calendar))
;  (format (find-pane-named *application-frame* 'inter) "(~a)~%" *select-date*))

(define-calendar-command (com-get-date :name t)
    ((date 'date :gesture :select))
  (setf *select-date* date)
  (frame-exit *application-frame*))

(define-calendar-command (com-change-month :name t)
    ((month 'select-month :gesture :select))
  (let ((m (select-month month)))
    (when (zerop m) (decf m))
    (setf m (mod m 13))
    (when (zerop m) (incf m))
    (setf *select-month* m)))

(define-calendar-command (com-change-year :name t)
    ((year 'select-year :gesture :select))
  (setf *select-year* (select-year year)))