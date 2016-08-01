(in-package :link-manager)


(clsql:def-view-class tag ()
                      ((tagsid
                         :db-kind :key
                         :db-constraints (:not-null :auto-increment)
                         :type integer)
                       (name
                         :type (string 50)
                         :accessor name
                         :initarg :name)
                       (bookmark
                         :accessor bookmark
                         :db-kind :join
                         :db-info (:join-class bookmark
                                               :home-key tagsid
                                               :foreign-key bookmarkid
                                               :set nil)
                         ) ))

(clsql:def-view-class bookmark ()
                      ((bookmarkid
                         :db-kind :key
                         :db-constraints (:not-null :auto-increment)
                         :type integer)
                       (title
                         :accessor title
                         :type (string)
                         :initarg :title)
                       (link
                         :accessor link
                         :type (string)
                         :initarg :link)
                       (summary
                         :accessor summary
                         :type (string)
                         :initarg :summary)
                       (date-added
                         :accessor date-added)
                       (tags
                         :accessor tags
                         :initarg :tags
                         :db-kind :join
                         :db-info (:join-class tags
                                               :home-key bookmarkid
                                               :foreign-key tagid
                                               :set t)
                         )))

(clsql:file-enable-sql-reader-syntax)
(clsql:start-sql-recording)

(ignore-errors
  (clsql:drop-view-from-class 'tag)
  (clsql:drop-view-from-class 'bookmark))

(ignore-errors
  (clsql:create-view-from-class 'tag)
  (clsql:create-view-from-class 'bookmark))


(let ((clsql:*db-auto-sync* t))
  (make-instance 'tag :name "lisp")
  (make-instance 'bookmark :title "lisp programming" :link
                 "http://lisp.org" :summary "test"
                 :tags '(lisp python))) 

(clsql:select 'bookmark)


(defmethod print-object ((object bookmark) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (bookmarkid title link summary) object
      (format stream  "id: ~d title: ~s link: ~s summary: ~s"
              bookmarkid
              (string-trim '(#\space) title)
              (string-trim '(#\space) link)
              (string-trim '(#\space) summary)))))

(defmethod print-object ((object tag) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (tagid name) object
      (format stream  "id: ~d name: ~s"
              tagid
              (string-trim '(#\space) name)))))
