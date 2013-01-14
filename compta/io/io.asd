(in-package #:cl-user)

(asdf:defsystem :io
  :depends-on (:split-sequence)
  :components
  ((:file "package-io" :depends-on ())
   (:file "io" :depends-on ("package-io"))))