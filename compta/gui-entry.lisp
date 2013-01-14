(in-package :compta-gui)

(defun display-entry (pane transaction entry format-val)
  (unless (null entry)
    (with-output-as-presentation
	(pane transaction 'transaction)
      (format pane "~a" (iso-date-string (date transaction))))
    (format-amount pane (amount entry) format-val)
    (with-output-as-presentation
	(pane transaction 'transaction)
      (format pane "~a~%" (name transaction)))))

(defun display-entry-adder (pane area-name push-entry entries)
  (flet ((show-entry (entry)
	   (format pane "~4t")
	   (with-output-as-presentation (pane entry 'entry)
	     (format pane "[opts]"))
	   (format-amount pane (amount entry) "[~10d.~2,'0d]")
	   (format pane " ")
	   (with-output-as-presentation (pane (account entry) 'account-r)
	     (format pane "~a~%" (name (account entry))))))
	     
    (format pane "~a: " area-name)
    (let ((adder (make-instance 'entry-adder
				:adder push-entry)))
      (with-output-as-presentation (pane adder 'entry-adder)
	(format pane "[add]~%")))
    (loop for entry in (reverse entries)
       do (show-entry entry))))

(define-compta-command (com-add-entry :name t)
    ((adder 'entry-adder :gesture :select))
  (let ((tr (transaction (stream-default-view (find-pane-named *application-frame* 'main))))
	(amount  (accept-or-abort com-add-entry 'amount    :prompt "Valeur "))
	(account (accept-or-abort com-add-entry 'account-r :prompt "Compte ")))
    (if (not (or
	      (member account (debits  tr) :test #'eql :key #'account)
	      (member account (credits tr) :test #'eql :key #'account)))
	(progn
	  (funcall (adder adder) 
		   (make-instance 'entry
				  :account account
				  :amount  amount))
	  (setf (valid tr) (valid-transaction tr)))
	(display-inter "Compte '~a' déjà utilisé dans la transaction" (name account)))))

;exo 4 (facile) 
(define-compta-command (com-delete-entry :name t)
    ((entry 'entry :gesture :nothing))
  (let ((pane-in (find-pane-named *application-frame* 'inter))
	(tr      (transaction (stream-default-view (find-pane-named *application-frame* 'main)))))
    (format pane-in "Voulez-vous vraiment supprimer l'entrée ")
    (format-amount pane-in (amount entry)  "[~10d.~2,'0d]")
    (display-oneline-account-summary pane-in " ~a~%" (account entry))
    (when (confirm)
      (setf (debits tr)  (delete entry (debits  tr)))
      (setf (credits tr) (delete entry (credits tr)))
      (setf (valid tr) (valid-transaction tr))
      (format pane-in "Entrée supprimée"))))

;exo 5 (facile)
(define-compta-command (com-edit-entry :name t)
    ((entry 'entry :gesture :nothing))
  (let ((tr (transaction (stream-default-view (find-pane-named *application-frame* 'main))))
	(amount  (accept-or-abort com-edit-entry 'amount    :prompt "Valeur "))
	(account (accept-or-abort com-edit-entry 'account-r :prompt "Compte ")))
    (if (or
	 (eql account (account entry)) ; new account = old account
	 (not (or                      ; new account not use
	       (member account (debits  tr) :test #'eql :key #'account)
	       (member account (credits tr) :test #'eql :key #'account))))
	(progn
	  (setf (amount  entry) amount)
	  (setf (account entry) account)
	  (setf (valid tr) (valid-transaction tr)))
	(display-inter "Compte '~a' déjà utilisé dans la transaction" (name account)))))