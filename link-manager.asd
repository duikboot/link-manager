;;;; link-manager.asd

(asdf:defsystem #:link-manager
  :description "Describe link-manager here"
  :author "Arjen Dijkstra <dijkstra.arjen@gmail.com>"
  :license "Specify license here"
  :serial t
  :depends-on (:hunchentoot)
  :components ((:file "package")
               (:file "link-manager")))

