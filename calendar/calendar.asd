(in-package #:cl-user)

(asdf:defsystem :calendar
  :depends-on (:mcclim)
  :components
  ((:file "package" :depends-on ())
   (:file "calendar" :depends-on ("package"))))
