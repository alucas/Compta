(in-package :compta-gui)

(defun display-oneline-account-summary (pane format-val account)
  (with-output-as-presentation (pane account 'account-r)
    (format pane format-val (name account))))

;exo 1
(defun display-account-infos (pane transactions account start end enable)
  (let ((trs transactions))
    (when enable
      (setf trs (remove-if (lambda (x) (not (date-in-period start end (date x)))) transactions)))
    (let ((format-val "[~10d.~2,'0d]~%")
	  (sum-debits  (sum-values-account trs account #'debits))
	  (sum-credits (sum-values-account trs account #'credits)))
      (format pane "Somme des débits :~%~10t")
      (format-amount pane sum-debits format-val)
      (format pane "Somme des crédits :~%~10t")
      (format-amount pane sum-credits format-val)
      (if (not enable)
	  (progn
	    (format pane "Solde du compte :~%~10t")
	    (format-amount pane (- sum-credits sum-debits) format-val))
	  (let ((trs (remove-if (lambda (x) (not (date-in-period 0 start (date x)))) transactions)))
	    (let ((sum-debits-before  (sum-values-account trs account #'debits))
		  (sum-credits-before (sum-values-account trs account #'credits)))
	      (format pane "Solde du compte (debut de période) :~%~10t")
	   
   (format-amount pane (- sum-credits-before sum-debits-before) format-val)
	      (format pane "Solde du compte (fin de période) :~%~10t")
	      (format-amount pane (+ (- sum-credits-before sum-debits-before) (- sum-credits sum-debits)) format-val)
	      ))))))

(defmethod display-main-with-view (frame pane (view account-view))
  (let ((account (account view))
	(org (current-organization *application-frame*)))
    
    ; display account name
    (display-oneline-account-summary pane "~a~%" account)
    
    ; display accounts
    (format pane "~%")
    (let ((period (period org))
	  (transactions (transactions org)))
      (let ((start (encode-time (start period)))
	    (end   (encode-time (end   period)))
	    (liste (if (typep account 'group)
		       (list-accounts account 'account)
		       (cons account nil))))
	(loop 
	   for tr in (reverse transactions)
	   do (when (or (not (enable period))
			(date-in-period start end (date tr)))
		(loop 
		   for acc in liste
		   do (display-entry pane tr
				     (find acc (debits tr) :key #'account)
				     "~10d.~2,'0d~50t")
		   do (display-entry pane tr
				     (find acc (credits tr) :key #'account)
				     "~30d.~2,'0d~50t"))))
	(format pane "~%")
    
        ; display account infos, exo 1
	(display-account-infos pane transactions account start end (enable period))
	))))

(defun display-accounts-list(pane accounts prefix)
  (loop 
    for acc in accounts
    do (format pane prefix)
    do (if (not (typep acc 'group))
	   (display-oneline-account-summary pane "~a~%" acc)
	   (progn
	     (with-text-face (pane :bold)
	       (with-output-as-presentation (pane acc 'group)
		 (format pane "~a~%" (name acc))))
	     (display-accounts-list pane (accounts acc) (concatenate 'string prefix "  "))))))

(defun display-accounts (frame pane)
  (format pane "Accounts :")
  (format pane "~%~%")
  (display-accounts-list pane (reverse (accounts (current-organization frame))) ""))

(define-compta-command (com-edit-account :name t)
    ((account 'account :gesture :select))
  (setf (stream-default-view (find-pane-named *application-frame* 'main))
        (make-instance 'account-view :account account)))

(defun add-account (type)
  (let ((org   (current-organization *application-frame*))
	(group (accept-or-abort add-account 'group  :prompt "Groupe "))
	(name  (accept-or-abort add-account 'string :prompt "Nom ")))
    (when (eql group 'aucun)
      (setf group org))
    (if (not (equal name ""))
	(if (not (member name (list-accounts org type) :test #'equal :key #'name))
	    (push (make-instance type :name name)
		  (accounts group))
	    (display-inter "Nom déjà utilisé~%"))
	(display-inter "Il faut entrer un nom~%"))))

(define-compta-command (com-add-account :name t :menu "Add-account") ()
  (add-account 'account-r))

; ajout perso
(define-compta-command (com-add-group :name t :menu "Add-group") ()
  (add-account 'group))



(define-compta-command (com-delete-account :name t)
    ((account 'account :gesture :nothing))
  (let ((organization (current-organization *application-frame*))
	(pane-in (find-pane-named *application-frame* 'inter)))
    (format pane-in "Voulez-vous vraiment supprimer le ~a "
	    (if (typep account 'group)
		"groupe (et ses sous-comptes)"
		"compte"))
    (display-oneline-account-summary pane-in "~a~%" account)
      (when (confirm)
	(setf (accounts (get-group-account organization account))
	      (delete account (accounts (get-group-account organization account)))))))

; ajout perso
(defun edit-account-name (object)
  (let ((name (accept-or-abort edit-account-name 'string :prompt "Entrer un nom" :default (name object) :insert-default t))
	(list (list-accounts (current-organization *application-frame*) (if (typep object 'group) 'group 'account))))
    (when (not (equal name (name object)))
      (if (not (equal name ""))
	  (if (not (member name list :test #'equal :key #'name))
	      (setf (name object) name)
	      (display-inter "Nom '~a' déjà utilisé" name))
	  (display-inter "Il faut entrer un nom")))))

; ajout perso
(define-compta-command (com-edit-account-name :name t)
    ((account 'account :gesture :nothing))
  (edit-account-name account))