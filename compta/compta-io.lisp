(in-package :compta-io)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Save information for various model classes

(define-save-info period
  (:end end) (:start start) (:enable enable))

(define-save-info organization
  (:name name) (:accounts accounts)
  (:transactions transactions) (:period period))

(define-save-info date
  (:year year) (:month month) (:day day)
  (:hour hour) (:minute minute))

(define-save-info group
  (:accounts accounts))

(define-save-info account
  (:name name))

(define-save-info entry
  (:account account) (:amount amount))1

(define-save-info transaction
  (:name name) (:date date) (:creator creator)
  (:debits debits) (:credits credits))

(defparameter *compta-allowed-version-names* '("ComptaV1" "ComptaV1.1"))
(defparameter *compta-current-version-name*  "ComptaV1.1")

(defun read-param ()
  (let ((var (make-hash-table :test #'equal))
	(filename ".compta")
	(list-int (list "main-v" "main-h"
			"account-v" "account-h"
			"transaction-v" "transaction-h"
			"organization-v" "organization-h"
			"command-v" "command-h"))
	(list-str (list "user"))
	(val 0))

    ; default values
    (setf (gethash "main-v" var) 800)
    (setf (gethash "main-h" var) 400)
    (setf (gethash "account-v" var) 200)
    (setf (gethash "account-h" var) 400)
    (setf (gethash "transaction-v" var) 300)
    (setf (gethash "transaction-h" var) 400)
    (setf (gethash "organization-v" var) 500)
    (setf (gethash "organization-h" var) 200)
    (setf (gethash "command-v" var) 800)
    (setf (gethash "command-h" var) 200)
    (setf (gethash "user" var) "Your father !")
    
    ; update values
    (when (probe-file filename)
	(with-open-file (stream filename :direction :input :if-does-not-exist nil)
	  (do ((list-tmp (read-line-to-list stream) (read-line-to-list stream)))
	      ((eq list-tmp 'eof) var)
	    (when (> (length list-tmp) 1)
	      (when (member (car list-tmp) list-int :test #'equal)
		  (setf val (read-from-string (cadr list-tmp)))
		  (when (typep val 'integer)
		    (setf (gethash (car list-tmp) var) val)
		    ;(format t "(int) ~a = ~d~%" (car list-tmp) val)
		    ))
	      (when (member (car list-tmp) list-str :test #'equal)
		  (setf (gethash (car list-tmp) var) (cadr list-tmp))
		  ;(format t "(str) ~a = ~a~%" (car list-tmp) (cadr list-tmp))
		  )
	      ))))

    var))