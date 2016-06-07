
(in-package :link-manager)

(setf html-template:*default-template-pathname* #P"templates/")

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
       (create-regex-dispatcher "^/a/$" 'generate-index-page)
       (create-regex-dispatcher "^/bookmarks/$" 'bookmarks)
       (create-regex-dispatcher "^/bookmarks/[0-9]+$" 'get-bookmark)
       (create-regex-dispatcher "^/bookmarks/[0-9]+/edit$" 'edit-bookmark)))

(defun get-order (x)
  (if (equalp "desc" x) #'> #'<))

(defun index ()
  (redirect "/bookmarks/"))

(defun generate-index-page ()
  "Generate the index page showing all the blog posts."
      (with-output-to-string (stream)
        (html-template:fill-and-print-template #P"index.html" '(:page-title "test") :stream stream)))

(defun bookmarks ()
  (let ((order (get-order (get-parameter "order")))
        (key (or
               (intern (string-upcase (get-parameter "sort")))
               'date-added)))
    (render-bookmarks (stable-sort (copy-list *db*) order :key key))))

(defun time-string (str date-time)
  (multiple-value-bind (sec minute hour day month year) (decode-universal-time date-time)
    (format nil "~a ~4d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" str year month day hour minute sec)))

(defun render-bookmarks (database)
  (with-html-output-to-string
    (*standard-output* nil :prologue t)
    (:html
      (:head (:title "Show all bookmarks"))
      (:body
        (:div
          (mapcar #'(lambda (row)
                      (htm
                        (:a :href (format nil "~a" (link row)) (fmt "~{ ~(~a~) ~}" (title row)))
                        :br
                        (:a :href
                            (format nil "/bookmarks/~a" (id row)) "details")
                        ; (:div (fmt "Title: ~A" (title row)))
                        (:div (fmt (time-string "Date added: "(date-added row))))
                        (:div (fmt (time-string "Date modified: " (date-modified row))))
                        (:div (fmt "Summary: ~{ ~(~a~) ~}" (summary row)))
                        (:div (fmt "Tags: ~{ ~(~a~) ~}" (tags row)))
                        :br
                        )) database))))))

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
  (load-db)
  (format t "~%Bookmark application started on port ~A~%" *web-port*))

(defun stop-app ()
  (stop-server)
  (save))
