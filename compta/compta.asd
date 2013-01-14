(in-package #:cl-user)

(asdf:defsystem :compta
  :depends-on (:mcclim :io :cl-pdf)
  :components
  ((:file "package-compta-model" :depends-on ())
   (:file "package-compta-io"    :depends-on ("package-compta-model"))
   (:file "package-compta-gui"   :depends-on ("package-compta-io"))
   (:file "model"                :depends-on ("package-compta-gui"))
   (:file "my-model"  :depends-on ("model"))
   (:file "compta-io" :depends-on ("my-model"))
   (:file "gui-calendar"     :depends-on ("my-model" "compta-io"))
   (:file "gui-compta"       :depends-on ("gui-calendar"))
   (:file "gui-account"      :depends-on ("gui-compta"))
   (:file "gui-entry"        :depends-on ("gui-compta"))
   (:file "gui-transaction"  :depends-on ("gui-entry"))
   (:file "gui-organization" :depends-on ("gui-compta"))
   ))
