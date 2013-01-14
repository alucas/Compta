(in-package :compta-gui)

(defun add-organization (organization)
  (if (not (member (name organization) (list-organizations *application-frame*) :test #'equal :key #'name))
      (push organization (list-organizations *application-frame*))
      (display-inter "Une organisation de nom '~a' est déjà ouverte~%" (name organization))))
	 
(defun read-organization (filename)
  (when (not (probe-file filename))
      (display-inter "Le fichier '~a' n'existe pas~%" filename)
      (return-from read-organization nil))
  
  (with-open-file (stream filename :direction :input)
    (when (not (member (read-line stream) *compta-allowed-version-names* :test #'string=))
      (display-inter "Version du fichier '~a' inconue~%" filename)
      (return-from read-organization nil)))
  
  (add-organization (read-model filename *compta-allowed-version-names*))
  (valid-transactions (car (list-organizations *application-frame*))))
	
       
	
(define-compta-command (com-write-organization :name t :menu "Save") ()
  (let ((filename (accept 'string :prompt "Adresse ")))
    (handler-case
	(write-model filename *compta-current-version-name*
		     (current-organization *application-frame*))
      (error ()
	(prog1 
	    (display-inter "You can't specifie a folder~%")
	  (return-from com-write-organization))))))

(define-compta-command (com-read-organization :name t :menu "Open") ()
  (let ((filename (accept 'pathname :prompt "Adresse ")))
    (handler-case
	(read-organization filename)
      (error ()
	(prog1 
	    (display-inter "You can't specifie a folder~%")
	  (return-from com-read-organization))))))

(define-compta-command (com-read-organization-default :name t) ()
  (read-organization "home3"))

(define-compta-command (com-creat-organization :name t :menu "New") ()
  (let ((filename (accept 'string :prompt "Nom ")))
    (add-organization (make-instance 'organization :name filename))))

;exo 3 (moyen)
(defun sort-organizations (organizations)
  (stable-sort organizations
	       (lambda (x y) (string< (name x) (name y)))))

;exo 3 (moyen)
(defun display-oneline-organization-summary (pane organization)
      (with-output-as-presentation (pane organization 'organization)
	(format pane "~a~%" (name organization))))

;exo 3 (moyen)
(defun display-organisation (frame pane)
  (format pane "Organizations :")
  ;(with-output-as-presentation (pane nil 'add-organization)
    ;(format pane "[add]"))
  (format pane "~%~%")
  (loop 
    for or in (sort-organizations (copy-list (list-organizations frame)))
    do (display-oneline-organization-summary pane or)))

;exo 3 (moyen)
(define-compta-command (com-change-organization :name t)
    ((organization 'organization :gesture :select))
  (setf (list-organizations *application-frame*) (delete organization (list-organizations *application-frame*)))
  (push organization (list-organizations *application-frame*)))

(define-compta-command (com-delete-organization :name t)
    ((organization 'organization :gesture :nothing))
  (let ((pane-in (find-pane-named *application-frame* 'inter)))
    (format pane-in "Voulez-vous vraiment supprimer l'organization ~a~%" (name organization))
    (when (confirm)
      (setf (list-organizations *application-frame*) (delete organization (list-organizations *application-frame*))))))
  
(define-compta-command (com-edit-organization-name :name t)
    ((organization 'organization :gesture :nothing))
  (let ((filename (accept 'string :prompt "Nom ")))
    (when (not (equal filename (name organization)))
      (if (not (member filename (list-organizations *application-frame*) :test #'equal :key #'name))
	  (setf (name organization) filename)
	  (display-inter "Une organisation de nom '~a' existe déjà~%" filename)))))