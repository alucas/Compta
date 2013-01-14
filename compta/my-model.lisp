(in-package #:compta-model)

;;;;;;;;;;;;;;;;;;;
;;               ;;
;;  Ajout perso  ;;
;;               ;;
;;;;;;;;;;;;;;;;;;;

(defun get-group-account (group object)
  (when (not (null group))
    (loop
       for acc in (accounts group)
       do (if (eql acc object)
	      (return-from get-group-account group)
	      (when (typep acc 'group)
		(let ((group-tmp (get-group-account acc object)))
		  (when group-tmp
		    (return-from get-group-account group-tmp)))))
       finally (return nil))))

; group : group or organization
; type : 'group or 'account
(defun list-accounts(group type)
  (let ((liste nil))
    (loop
       for acc in (accounts group)
       do (if (typep acc 'group)
	      (progn
		(when (eql type 'group)
		  (push acc liste))
		(setf liste (append liste (list-accounts acc type))))
	      (unless (eql type 'group)
		(push acc liste)))
       finally (return-from list-accounts liste))))

; start : (encode-time (start period))
; end   : (            (end         ))
(defun date-in-period (start end date)
    (let ((time (encode-time date)))
      (when (and (>= time start)
		 (<  time end))
	(return-from date-in-period t)))
    nil)

;;;;;;;;;;;;;
;;         ;;
;; Facile  ;;
;;         ;;
;;;;;;;;;;;;;

; exo 1
(defun value-account (transaction account f)
  (let ((tr (find-if (lambda (x) (eq account (account x))) (funcall f transaction))))
    (if (null tr)
	0
	(amount tr))))

(defun sum-values-account (transactions account func)
  (let ((liste (if (typep account 'group)
		   (list-accounts account 'account)
		   (cons account nil))))
    (loop
       for tr in transactions
       sum (loop
	      for acc in liste
	      sum (value-account tr acc func))
	 )))

; exo 2
(defun lessp-by-accessor (obj1 obj2 &rest accessors)
  (loop for f in accessors
	do (progn (when (< (funcall f obj1) (funcall f obj2))
		    (return t))
		  (when (> (funcall f obj1) (funcall f obj2))
		    (return nil)))
	finally (return t)))

(defun date-lessp (date1 date2)
  (lessp-by-accessor date1 date2 #'year #'month #'day #'hour #'minute)  
)


; exo 3
(defun sum-values-transaction (transaction func)
  (loop for val in (funcall func transaction)
	sum (amount val)))

; exo 4
; exo 5
; exo 6
(defun valid-transaction(transaction)
  (zerop (- (sum-values-transaction transaction #'credits) (sum-values-transaction transaction #'debits))))

(defun valid-transactions(organizations)
  (let ((trs (transactions organizations)))
    (loop
      for tr in trs
      do (setf (valid tr) (valid-transaction tr)))))

;;;;;;;;;;;;;
;;         ;;
;;  Moyen  ;;
;;         ;;
;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;
;;             ;;
;;  Difficile  ;;
;;             ;;
;;;;;;;;;;;;;;;;;


(setf pdf:*compress-streams* nil)
(defun graph-pdf (organization &optional (file #P"./graph.pdf"))
  (let* ((accounts (list-accounts organization 'account))
	 (nombre-comptes (length accounts))
	 (max 0)
	 (montants-comptes (list)))
    (pdf:with-document ()
      (pdf:with-page ()
	(pdf:with-outline-level ("Main" (pdf:register-page-reference))
	  (let ((helvetica (pdf:get-font "Helvetica")))
	     (pdf:in-text-mode
	       (pdf:set-font helvetica 24.0)
	       (pdf:move-text 100 800)
	       (pdf:draw-text (format nil "Graph of the organization ~a~%" (name organization))))
	    (pdf:translate 230 500)
	    
	    (let* ((transactions (transactions organization))
		  (period (period organization))
		  (start (encode-time (start period)))
		  (end   (encode-time (end   period))))
	      (when (enable period)
		    (setf transactions (remove-if (lambda (x) (not (date-in-period start end (date x)))) transactions)))

	    (setf montants-comptes
		  (loop
		    for compte in accounts
		    collect (- (sum-values-account transactions compte #'credits)
			       (sum-values-account transactions compte #'debits)))))
	    (setf max (apply #'max montants-comptes))

	    (let ((min (apply #'min montants-comptes)))
	      (when (minusp (+ max min))
		(setf max (- min)))

	      
	      
	      (loop 
		;repeat nombre-comptes
		;for compte in (list-accounts organization 'account)
		for montant in montants-comptes
		for x from -200 by (/ 512 nombre-comptes)
		do (pdf:set-rgb-fill (/ (random 255) 255.0)(/ (random 255) 255.0)(/ (random 255) 255.0))
		   (pdf:set-rgb-stroke 0.1 0.1 0.1)
		   
		   (let ((height (* 300 (/ montant max))))
		     (when (not (zerop montant))
		       (if (> montant 0)
			   (pdf:rectangle x -100 (/ 300 nombre-comptes) height :radius 1)
			   (pdf:rectangle x -102 (/ 300 nombre-comptes) height :radius 1))
		       (pdf:close-fill-and-stroke))
		     
		     (pdf:in-text-mode
		       (pdf:set-font helvetica 10.0)
		       
		       (if (> montant 0)
			   (pdf:move-text x (- height 97))
			   (pdf:move-text x -97))
		       (pdf:draw-text (format nil "~a" (coerce (/ montant 100) 'float))))))
		   
	      (loop 
		for compte in (list-accounts organization 'account)
		for x from -200 by (/ 512 nombre-comptes)
		do (pdf:set-rgb-fill (/ (random 255) 255.0)(/ (random 255) 255.0)(/ (random 255) 255.0))
		   (pdf:set-rgb-stroke 0.1 0.1 0.1)
		   (pdf:in-text-mode
		     (pdf:set-font helvetica 21.0)
		     (pdf:rotate -45)
		     (pdf:set-rgb-fill 0 0 0)
		     (pdf:move-text (+ (/ x 1.4) 85) (- (/ x 1.4) 80))
		     (pdf:draw-text (name compte))
		     (pdf:rotate 45)))
	      
	      (pdf:set-rgb-fill 0 0 255)
	      (pdf:set-rgb-stroke 0.05 0.05 0.05)
	      (pdf:rectangle -204 -102 (+ 8 (* nombre-comptes (/ 512 nombre-comptes))) 2 :radius 1) ; axe horizontal
	      (pdf:close-fill-and-stroke)
	      
	      (pdf:set-rgb-stroke 0.05 0.05 0.05)
	      (pdf:rectangle -204 -102 2 300  :radius 1) ; axe vertical
	      (pdf:close-fill-and-stroke)
	      
	      (pdf:set-rgb-stroke 0.05 0.05 0.05) ; Fleche haut
	      (pdf:regular-polygon -203 201 6 3)
	      (pdf:close-fill-and-stroke)
	      
	      (pdf:rotate -90)
	      (pdf:set-rgb-stroke 0.05 0.05 0.05) ; Fleche droite
	      (pdf:regular-polygon 101 (+ -196 (* nombre-comptes (/ 512 nombre-comptes))) 6 3)
	      (pdf:close-fill-and-stroke)
	      (pdf:rotate 90)))))
      (pdf:write-document file))))