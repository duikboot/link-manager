(in-package :link-manager)


(defclass bookmark-bkrn (bknr.datastore:store-object)
  ((title
     :accessor title
     :index-type bknr.indices:string-unique-index
     :index-reader bookmark-by-title
     :initarg :title)
   (link
     :accessor link
     :index-reader bookmark-by-link
     :index-type bknr.indices:string-unique-index
     :initarg :link)
   (summary
     :accessor summary
     :initarg :summary)
   (date-added
     :initarg :date-added
     :accessor date-added)
   (tags
     :accessor tags
     :index-reader bookmark-by-tags
     :initarg :tags))
  (:metaclass bknr.datastore:persistent-class))


(let ((object-subsystem
        (make-instance 'bknr.datastore:store-object-subsystem)))
  (make-instance 'bknr.datastore:mp-store
                 :directory "/tmp/store/"
                 :subsystems (list object-subsystem)))

(make-instance 'bookmark-bkrn
               :title "lisp programming"
               :link "http://lisp.org"
               :summary "test"
               :tags '(lisp python))  

(make-instance 'bookmark-bkrn
               :title "python programming"
               :link "http://python.org"
               :summary "test"
               :tags '(lisp python))  

(defmethod print-object ((object bookmark-bkrn) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (title link summary) object
      (format stream  "title: ~s link: ~s summary: ~s"
              (string-trim '(#\space) title)
              (string-trim '(#\space) link)
              (string-trim '(#\space) summary)))))

