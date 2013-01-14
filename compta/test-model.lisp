(require :unit-test)

(load "package-compta-model.lisp")
(load "model.lisp")
(load "my-model.lisp")

(use-package :unit-test)
(use-package :compta-model)

(defparameter *account1* (make-instance 'account :name "account 1"))
(defparameter *account2* (make-instance 'account :name "account 2"))
(defparameter *account3* (make-instance 'account :name "account 3"))
(defparameter *account4* (make-instance 'account :name "account 4"))
(defparameter *account5* (make-instance 'account :name "account 5"))
(defparameter *account6* (make-instance 'account :name "account 6"))

(defparameter *group1* (make-instance 'group :name "group 1" :accounts (list *account1*)))
(defparameter *group2* (make-instance 'group :name "group 2" :accounts (list *group1* *account2*)))
(defparameter *group3* (make-instance 'group :name "group 3" :accounts nil))

(defparameter *entry1* (make-instance 'entry :account *account1* :amount 1000))
(defparameter *entry2* (make-instance 'entry :account *account2* :amount 2000))
(defparameter *entry3* (make-instance 'entry :account *account3* :amount 3000))
(defparameter *entry4* (make-instance 'entry :account *account3* :amount 4000))

(defparameter *date1* (make-instance 'date :year 2000 :month 1 :day 1 :hour 0 :minute 1))
(defparameter *date2* (make-instance 'date :year 2000 :month 1 :day 1 :hour 0 :minute 2))
(defparameter *date3* (make-instance 'date :year 2000 :month 1 :day 1 :hour 0 :minute 3))

(defparameter *tr1* (make-instance 'transaction :name "tr1" :date *date1* :debits (list *entry1* *entry3*) :credits (list *entry2* *entry4*)))
(defparameter *tr2* (make-instance 'transaction :name "tr2" :date *date2* :debits (list *entry4* *entry2*) :credits (list *entry3* *entry1*)))
(defparameter *tr3* (make-instance 'transaction :name "tr3" :date *date2* :debits (list *entry4* *entry1*) :credits (list *entry3* *entry2*)))

(defparameter *org1* (make-instance 'organization :name "org1" :accounts (list *account3* *group2* *account4* *group3* *account5*) :transactions (list *tr1* *tr2*)))

(deftest :test-model "get-group-account"
  (test-assert (and
		(eql
		 (get-group-account *org1* *account6*)
		 nil)
		(eql
		 (get-group-account *org1* *account3*)
		 *org1*)
		(eql
		 (get-group-account *org1* *account2*)
		 *group2*)
		)))

(deftest :test-model "get-group-account 'account"
  (test-assert (let ((list-tmp (list-accounts *org1* 'account)))
		 (and
		  (= (length list-tmp) 5) 
		  (member *account1* list-tmp)
		  (member *account2* list-tmp)
		  (member *account3* list-tmp)
		  (member *account4* list-tmp)
		  (member *account5* list-tmp)
		  ))))

(deftest :test-model "get-group-account 'group"
  (test-assert (let ((list-tmp (list-accounts *org1* 'group)))
		 (and
		  (= (length list-tmp) 3) 
		  (member *group1* list-tmp)
		  (member *group2* list-tmp)
		  (member *group3* list-tmp)
		  ))))

(deftest :test-model "date-in-period"
  (test-assert (and (null (date-in-period
			   (encode-time *date1*)
			   (encode-time *date2*)
			   *date2*))
		    (null (date-in-period
			   (encode-time *date2*)
			   (encode-time *date3*)
			   *date1*))
		    (date-in-period
			   (encode-time *date1*)
			   (encode-time *date2*)
			   *date1*)
		    )))

(deftest :test-model "value-account"
  (test-assert (and
		(= (value-account *tr1* *account1* #'debits)  1000)
		(= (value-account *tr1* *account2* #'credits) 2000)
		(= (value-account *tr1* *account1* #'credits) 0)
		)))

(deftest :test-model "sum-values-account"
  (test-assert (and
		(= (sum-values-account (transactions *org1*) *account3* #'debits) 7000)
		(= (sum-values-account (transactions *org1*) *account2* #'credits) 2000)
		(= (sum-values-account (transactions *org1*) *account4* #'credits) 0)
		)))

(deftest :test-model "date-lessp"
  (test-assert (and
		(     date-lessp *date1* *date2*)
		(not (date-lessp *date3* *date2*))
		)))

(deftest :test-model "sum-values-transaction"
  (test-assert (and
		(= (sum-values-transaction *tr1* #'debits)  4000)
		(= (sum-values-transaction *tr2* #'debits)  6000)
		(= (sum-values-transaction *tr2* #'credits) 4000)
		)))

(deftest :test-model "valid-transaction"
  (test-assert (and
		(not (valid-transaction *tr1*))
		(valid-transaction *tr3*)
		)))

(run-all-tests :unit :test-model)