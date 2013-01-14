(defpackage #:compta-model
  (:use #:common-lisp)
  (:export #:organization
           #:name
           #:accounts #:transactions #:group
           #:account #:account-r
           #:debits #:credits #:valid
           #:date #:iso-date-string #:encode-time
           #:year #:month #:day #:hour #:minute
           #:transaction
           #:creator
           #:entry
           #:amount
	   #:period
	   #:end #:start #:enable
           #:*operator*
	   #:value-account
	   #:sum-values-account
	   #:sum-values-transaction
	   #:date-lessp
	   #:get-group-account
	   #:list-accounts
	   #:valid-transaction
	   #:valid-transactions
	   #:date-in-period
	   #:graph-pdf))