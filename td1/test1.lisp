(inpakage

(defun example1 (&optional (file #P"/tmp/ex1.pdf"))
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
	(let ((helvetica (pdf:get-font "Helvetica")))
	  (pdf:in-text-mode
	   (pdf:set-font helvetica 36.0)
	   (pdf:move-text 100 800)
	   (pdf:draw-text "cl-pdf: Example 1"))
	  (pdf:translate 230 500)
	  (loop repeat 150
	 for i = 0.67 then (* i 1.045)
	 do (pdf:in-text-mode
	     (pdf:set-font helvetica i)
	     (pdf:set-rgb-fill (/ (random 255) 255.0)(/ (random 255) 255.0)(/ (random 255) 255.0))
	     (pdf:move-text (* i 3) 0)
	     (pdf:show-text "cl-typesetting"))
	   (pdf:rotate 13)))))
    (pdf:write-document file)))
