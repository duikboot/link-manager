
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
        ; (:div :class "navbar-header"
        ;     (:span :class "sr-only" "Toggle navigation")
        ;     (:span :class "icon-bar")
        ;     (:span :class "icon-bar")
        ;     (:span :class "icon-bar"))
          ;;(:a :class "navbar-brand" :href "#" "brand"))
        ; (:div :class "collapse navbar-collapse" :id "bs-example-navbar-collapse-1"
        ;   (:ul :class "nav navbar-nav navbar-right"))
        (:a :class "navbar-brand" :href "/bookmarks/add" "Add bookmark")))))

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

(defun bookmarks ()
  (let* ((order (get-order (get-parameter "order")))
         (key (or
                (intern (string-upcase (get-parameter "sort")))
                'date-added))
         (tags (or (get-parameter "tags") '()))
         (summary (or (get-parameter "summary") '()))
         (database (stable-sort (copy-list *db*) order :key key))
         (database (select-links-with-tags (create-query-sequence tags #\,) database))
         (database (select-links-with-summary (create-query-sequence summary #\,) database)))
    (render-bookmarks database)))

(defun save-bookmark ()
  (let
    ((id (parse-integer (post-parameter "id")))
     (title (create-query-sequence (string-trim '(#\space) (post-parameter "title")) #\space))
     (link (string-trim '(#\space) (post-parameter "link")))
     (summary (create-query-sequence (string-trim '(#\space) (post-parameter "summary")) #\space))
     (tags (create-query-sequence (string-trim '(#\space) (post-parameter "tags")) #\space)))
    (if (not (= id 0))
      (update :fn (where 'id id) :title title :link link
                   :summary summary :tags tags)
      (make-link title link summary tags)))
  (redirect "/bookmarks/"))

(defun bookmark-form (&key id title link summary tags)
    (standard-page
      (:title "Add bookmark")
      (:div :align "center"
            (:form :method "post" :action "/bookmarks/save"
                    (:input :type "hidden" :value (if id id 0) :name "id")
                   (:div :class "form-group"
                         (:label "Title")
                         (:input :type "text" :value
                                 (if title (format nil "~{~(~a~^ ~)~}" title) nil) :name "title"))
                   (:div :class "form-group"
                         (:label "Link")
                         (:input :type "text" :value
                                 (if link link nil) :name "link"))
                   (:div :class "form-group"
                         (:label "summary")
                         (format t "<textarea name=\"summary\" rows= \"10\" cols= \"70\">")
                         (if summary (format t "~{~(~a~^ ~)~}" summary))
                         (format t "</textarea>"))
                   (:div :class "form-group"
                         (:label "tags")
                         (:input :type "text" :value
                                 (if tags (format nil "~{~(~a~^ ~)~}" tags) nil) :name "tags" :size 80))
                   (:div :class "form-group"
                         (:input :type "submit" :class "btn btn-primary" :value "Save"))))))

(defun render-bookmarks (database)
  (standard-page
    (:title "Bookmarks")
    (:div :class "container"
    (:div (fmt "Tags: ~{ ~a ~}" (show-all-unique-elements 'tags *db*)))
    (htm :br)
    (mapcar #'(lambda (row)
                (htm
                  (:div :class "col-md-6 col column-div"
                    (:a :target "_blank" :href
                        (format nil "~a" (link row)) (fmt "~{ ~(~a~) ~}" (title row)))
                    (:a :class "glyphicon glyphicon-pencil" :href (format nil "/bookmarks/edit/~a" (id row)))
                    (:a :class "glyphicon glyphicon-remove" :href (format nil "/bookmarks/delete/~a" (id row)))
                    (:div (fmt (format-time "Date added: "(date-added row))))
                    (:div (fmt (format-time "Date modified: " (date-modified row))))
                    (:div (fmt "Summary: ~{ ~(~a~) ~}" (summary row)))
                    (:div (fmt "Tags: ~{ ~(~a~) ~}" (tags row))))
                    )) database))))

(defun delete-bookmark ()
  (let ((bookmark-id
          (first (reverse (split-sequence:split-sequence #\/ (request-uri*))))))
    (delete-link (parse-integer bookmark-id)))
  (redirect "/"))

(defun edit-bookmark ()
  (let* ((bookmark-id
          (first (reverse (split-sequence:split-sequence #\/ (request-uri*)))))
         (bookmark (first (select :fn (where 'id (parse-integer bookmark-id)))))
         (id (id bookmark))
         (title (title bookmark))
         (link (link bookmark))
         (summary (summary bookmark))
         (tags (tags bookmark)))
    (bookmark-form :id id :title title :link link :summary summary :tags tags)))

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
