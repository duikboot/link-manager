
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

(defun get-order (x)
  (if (equalp "desc" x) #'> #'<))

(defun truncate-string (str len)
  (if (> (length str) len) (subseq str 0 len) str))

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
       (:script :src "/jquery.js")
       (:script :src "/bootstrap-js.js"))
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
    (:nav :class "navbar navbar-inverse navbar-fixed-top"
      (:div :class "container-fluid"
        (:div :class "navbar-header"
          (:a :class "navbar-brand" :href "/"
            (:i :class "glyphicon glyphicon-home"))
          (:ul :class "nav navbar-nav"
            (:li
                (:a :href "/bookmarks/add" "Add bookmark")))
          (:form :method "get" :class "navbar-form navbar-left" :role "search"
            (:div :class "form-group"
              (:input :type "text" :name "search"
                      :class "form-control" :placeholder "Search bookmarks..."))))))))

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
  (save)
  (redirect "/bookmarks/"))

(defun bookmark-form (&key id title link summary tags)
    (standard-page
      (:title "Add bookmark")
      (:div :style "margin: 0 auto; width: 90%"
            (:form :method "post" :class "form-horizontal" :action "/bookmarks/save"
                   (:input :type "hidden" :value (if id id 0) :name "id")
                   (:div :class "form-group"
                         (:label :for "bf_title" :class "col-sm-2 control-label" "Title")
                         (:div :class "col-sm-10"
                             (:input :type "text" :id "bf_title" :placeholder "Bookmark title"
                                :tabindex "1" :class "form-control" :value
                                 (if title (format nil "~{~(~a~^ ~)~}" title) nil) :name "title")))
                   (:div :class "form-group"
                         (:label :for "bf_link" :class "col-sm-2 control-label" "Link")
                         (:div :class "col-sm-10"
                             (:input :type "url" :id "bf_link" :placeholder "https://www.site.com"
                                :tabindex "2" :class "form-control" :value
                                 (if link link nil) :name "link")))
                   (:div :class "form-group"
                         (:label :for "bf_sum" :class "col-sm-2 control-label" "Summary")
                         (:div :class "col-sm-10"
                            (format t "<textarea name=\"summary\" placeholder=\"Descriptive summary of site contents\"
                                tabindex=\"3\" class=\"form-control\"  rows=\"10\" cols=\"70\">")
                            (if summary (format t "~{~(~a~^ ~)~}" summary))
                            (format t "</textarea>")))
                   (:div :class "form-group"
                         (:label :for "bf_tags" :class "col-sm-2 control-label" "Tags")
                         (:div :class "col-sm-10"
                             (:input :type "text" :id "bf_tags" :placeholder "comma, separated, words"
                                :tabindex "4" :class "form-control" :value
                                 (if tags (format nil "~{~(~a~^ ~)~}" tags) nil) :name "tags" :size 80)))
                   (:div :class "form-group"
                        (:div :class "col-sm-offset-2 col-sm-10"
                            (:button :type "submit" :tabindex "5" :class "btn btn-primary" "Save")
                            (:a :tabindex "6" :href "/bookmarks/" :class "btn" "Cancel")))))))


(defun render-tags (tags)
  (with-html-output
    (*standard-output* nil :indent t)
    (htm
      (:div :class "row"
            (:div
              :class "col-md-12"
              (:div
                :class "tags-list well"
                (mapcar #'(lambda (tag)
                            (htm
                              (:a
                                :class "btn btn-primary btn-xs" :role "button"
                                :href "#" (format t "~{ ~a ~}" tag)))) tags)))))))

(defun render-bookmarks (database)
  (standard-page
    (:title "Bookmarks")
    (:div :class "container"
      (render-tags (number-of-occurrences 'tags database))
      (htm :br)
      (mapcar #'(lambda (row)
                  (htm
                    (:div :class "col-md-4 col column-div"
                      (:a :target "_blank" :href
                          (format nil "~a" (link row)) (fmt "~{ ~(~a~) ~}" (title row)))
                      (:a :class "glyphicon glyphicon-pencil" :href (format nil "/bookmarks/edit/~a" (id row)))
                      (:a :class "glyphicon glyphicon-remove" :href (format nil "/bookmarks/delete/~a" (id row)))
                      ; try make link for delete and make it cancable.
                      ; (format t "<textarea name=\"summary\" rows= \"10\" cols= \"70\">")
                      (:div (fmt (format-time "Date added: "(date-added row))))
                      (:div (fmt (format-time "Date modified: " (date-modified row))))
                      (:div (fmt "Tags: <b><em>~{ ~(~a~) ~}</em></b>" (tags row)))
                      (:div (fmt "Summary: ~{ ~(~a~) ~}" (summary row))))
                      )) database))))

(defun delete-bookmark ()
  "Delet bookmark"
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
