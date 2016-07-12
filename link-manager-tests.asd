(in-package :cl-user)

(asdf:defsystem #:link-manager-test
  :depends-on (:link-manager
               :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:file "test/packages")
               (:test-file "test/xmllib"
                :depends-on ("test/packages"))
               (:test-file "test/xml"
                :depends-on ("test/packages"))
               (:test-file "test/html"
                :depends-on ("test/packages"))
               (:test-file "test/template"
                :depends-on ("test/packages"))
               (:test-file "test/macro"
                :depends-on ("test/packages")))
  :perform (test-op :after (op c)
(funcall (intern #.(string :run) :prove) c)))
