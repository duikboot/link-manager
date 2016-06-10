
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

(push (create-static-file-dispatcher-and-handler "/images/logo.png" "images/logo.png") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/style.css" "style/style.css") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/bootstrap.css" "bootstrap/css/bootstrap.css") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/bootstrap-js.js" "bootstrap/js/bootstrap.js") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/fonts/glyphicons-halflings-regular.ttf" "bootstrap/fonts/glyphicons-halflings-regular.ttf") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/fonts/glyphicons-halflings-regular.woff" "bootstrap/fonts/glyphicons-halflings-regular.woff") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/fonts/glyphicons-halflings-regular.woff2" "bootstrap/fonts/glyphicons-halflings-regular.woff2") *dispatch-table*)
;;font awesome fonts
(push (create-static-file-dispatcher-and-handler "/font-awesome.css" "font-awesome-4.6.3/css/font-awesome.css") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/fonts/fontawesome-webfont.woff2" "font-awesome-4.6.3/fonts/fontawesome-webfont.woff2") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/fonts/fontawesome-webfont.woff " "font-awesome-4.6.3/fonts/fontawesome-webfont.woff") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/fonts/fontawesome-webfont.ttf" "font-awesome-4.6.3/fonts/fontawesome-webfont.ttf") *dispatch-table*)

(defun get-order (x)
  (if (equalp "desc" x) #'> #'<))

;standard page for all the html pages
(defmacro standard-page ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html
      (:head 
       (:meta :content "text/html" :charset "utf-8")
       (:meta :http-equiv "X-UA-Compatible" :content "IE=edge")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1")
       (:title ,title)
       (:link :href "/style.css" :rel "stylesheet" :type "text/css")
       (:link :href "/bootstrap.css" :rel "stylesheet" :type "text/css")
       (:link :href "/font-awesome.css" :rel "stylesheet" :type "text/css")
       (:script :src "/bootstrap-js.js" ))
      (:body
        (header)
        (:div :style "min-height:450px"
         (:h2 :style "text-align:center" ,title)
         (:br)
         ,@body)
(footer)))))

;; header procedure
(defun header ()
  (with-html-output (*standard-output* nil :indent t)
    (:nav :class "navbar navbar-default"
      (:div :class "container-fluid"
        (:div :class "navbar-header"
            (:span :class "sr-only" "Toggle navigation")
            (:span :class "icon-bar")
            (:span :class "icon-bar")
            (:span :class "icon-bar"))
          ;;(:a :class "navbar-brand" :href "#" "brand"))
        (:div :class "collapse navbar-collapse" :id "bs-example-navbar-collapse-1"
          (:ul :class "nav navbar-nav navbar-right"))))))

 ;; footer content
(defun footer ()
  (with-html-output (*standard-output* nil :indent t)
  (:footer :class "footer-distributed"
    (:div :class "footer-right"
      (:a :href "https://twitter.com/u_boot" (:i :class "fa fa-twitter")))
    (:div :class "footer-left"
    (:a :href "/index" (:img  :class "logo" :src "/images/logo.png" )) 
(:p :style "color: black;" "Duikboot &copy; 2016")))))

(defun index ()
  (redirect "/bookmarks/"))

(defun generate-index-page ()
  "Generate the index page showing all the blog posts."
      (with-output-to-string (stream)
        (html-template:fill-and-print-template #P"index.html" '(:page-title "test") :stream stream)))

(defun create-query-sequence (sequence)
  (if sequence (mapcar #'intern (mapcar #'string-upcase (split-sequence:split-sequence #\, sequence))) '()))

(defun bookmarks ()
  (let* ((order (get-order (get-parameter "order")))
         (key (or
                (intern (string-upcase (get-parameter "sort")))
                'date-added))
         (tags (or (get-parameter "tags") '()))
         (summary (or (get-parameter "summary") '()))
         (database (stable-sort (copy-list *db*) order :key key))
         (database (select-links-with-tags (create-query-sequence tags) database))
         (database (select-links-with-summary (create-query-sequence summary) database)))
    (render-bookmarks database)))

(defun render-bookmarks (database)
  (standard-page
    (:title "Show bookmarks")
    (:div (fmt "Tags: ~{ ~a ~}" (show-all-unique-elements 'tags *db*)))
    (:div
    (htm :br)
    (mapcar #'(lambda (row)
                (htm
                    (:a :href (format nil "~a" (link row)) (fmt "~{ ~(~a~) ~}" (title row)))
                    :br
                    (:a :href (format nil "/bookmarks/~a" (id row)) "details")
                    (:div (fmt (format-time "Date added: "(date-added row))))
                    (:div (fmt (format-time "Date modified: " (date-modified row))))
                    (:div (fmt "Summary: ~{ ~(~a~) ~}" (summary row)))
                    (:div (fmt "Tags: ~{ ~(~a~) ~}" (tags row)))
                    :br)) database))))

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
      (*standard-output* nil :prologue t :indent t)
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
