(defpackage #:compta-io
  (:use #:common-lisp #:compta-model #:io)
  (:export #:*compta-allowed-version-names*
	   #:*compta-current-version-name*
	   #:read-param))