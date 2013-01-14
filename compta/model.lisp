(in-package #:compta-model)

(defclass name-mixin ()
  ((%name :initarg :name :accessor name)))

(defclass account (name-mixin)
  ())

(defclass account-r (account)
  ())

(defclass group (account)
  ((%accounts :initform '()  :initarg :accounts :accessor accounts)))

(defmacro make-time-function (name n)
  `(defun ,name () (nth-value ,n (get-decoded-time))))

(make-time-function current-year 5)
(make-time-function current-month 4)
(make-time-function current-day 3)
(make-time-function current-hour 2)
(make-time-function current-minute 1)

(defclass date ()
  ((%year   :initarg :year   :initform (current-year)   :accessor year)
   (%month  :initarg :month  :initform (current-month)  :accessor month)
   (%day    :initarg :day    :initform (current-day)    :accessor day)
   (%hour   :initarg :hour   :initform (current-hour)   :accessor hour)
   (%minute :initarg :minute :initform (current-minute) :accessor minute)))

(defun iso-date-string (date)
  (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d"
          (year date) (month date) (day date)
          (hour date) (minute date)))

(defun encode-time(date)
  (encode-universal-time 0 (minute date)
			 (hour date) (day date)
			 (month date) (year date)))

(defclass entry ()
  ((%account :initarg :account :accessor account)
   (%amount  :initarg :amount  :accessor amount)))

(defclass transaction (name-mixin)
  ((%date    :initarg :date    :initform (make-instance 'date) :accessor date)
   (%creator :initarg :creator :initform "-" :reader creator)
   (%valid   :initarg :valid   :initform nil :accessor valid)
   (%debits  :initarg :debits  :initform '() :accessor debits)
   (%credits :initarg :credits :initform '() :accessor credits)))

(defclass period ()
  ((%end    :initarg :end    :initform (make-instance 'date) :accessor end)
   (%start  :initarg :start  :initform (make-instance 'date) :accessor start)
   (%enable :initarg :enable :initform nil :accessor enable)))

(defclass organization (name-mixin)
  ((%period   :initarg :period       :initform (make-instance 'period)  :accessor period)
   (%accounts :initarg :accounts     :initform '() :accessor accounts)
   (%transact :initarg :transactions :initform '() :accessor transactions)))