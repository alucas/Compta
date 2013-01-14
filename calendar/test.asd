(in-package #:cl-user)

(asdf:defsystem :calendar
  :depends-on (:mcclim)
  :components
  ((:file "calendar" :depends-on ())))
