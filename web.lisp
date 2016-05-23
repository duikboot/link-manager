
(in-package :link-manager)

(defun start-server ()
  (if *web-acceptor*
    (format t "~%Webserver already started~%")
    (setf *web-acceptor*
          (hunchentoot:start
            (make-instance 'hunchentoot:easy-acceptor :port *web-port*)))))

(defun stop-server ()
  (if (not *web-acceptor*)
    (format t "~%Webserver not running~%")
    (progn
      (hunchentoot:stop *web-acceptor*)
      (setf *web-acceptor* nil))))

(setq *dispatch-table*
      (list
       (create-regex-dispatcher "^/$" 'index)
       (create-regex-dispatcher "^/bookmarks/$" 'index)
       (create-regex-dispatcher "^/bookmarks/[0-9]+$" 'get-bookmark)
       (create-regex-dispatcher "^/bookmarks/[0-9]+/edit$" 'edit-bookmark)))


(defun index ()
  (let ((url-part (query-string*)))
    (format t "~a~%" url-part)
    (with-html-output-to-string (*standard-output* nil :prologue t)
          (:html
            (:head (:title "Show all bookmarks"))
            (:body
              (:div
                (mapcar #'(lambda (row)
                            (htm
                              (:a :href
                                  (format nil "/bookmarks/~A" (id row)) "details"))
                            (htm
                              (:div (fmt "~A" (title row)))
                              (:div (fmt "~A" (link row)))
                              (:div (fmt "~A" (summary row)))
                              (:div (fmt "~A" (tags row)))
                              :br
                              )) (sort-database *db*))))))))

(defun edit-bookmark ()
  (let ((bookmark-id
          (first (reverse (split-sequence:split-sequence #\/ (request-uri*)))))))
     (with-html-output-to-string
       (*standard-output* nil :prologue t)
       :html
       (:head (:title "Bookmark details"))
       (:body
         (htm (:a :href "/row" "test")))))

(defun get-bookmark ()
  (let ((bookmark-id
          (first (reverse (split-sequence:split-sequence #\/ (request-uri*))))))
    (with-html-output-to-string
      (*standard-output* nil :prologue t)
        (:html
            (:head (:title "Bookmark details"))
            (:body
            (htm
              (:div
                (fmt "~A"
                     (first
                       (select :fn
                               (where 'id (parse-integer bookmark-id))))))))))))

(defun start-app ()
  (start-server)
  (load-db))

(defun stop-app ()
  (stop-server)
  (save))
