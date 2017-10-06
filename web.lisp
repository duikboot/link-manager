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

; (defun truncate-string (str len)
;   (if (> (length str) len) (subseq str 0 len) str))

(defun truncate-string (str len)
  (cond
    ((<= (length str) len) str)
    (t (concatenate 'string (subseq str 0 len) "..."))))

;standard page for all the html pages
(defmacro standard-page ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html
      (:head
       (:meta :content "text/html" :charset "utf-8")
       (:meta :http-equiv "X-UA-Compatible" :content "IE=edge")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1")
       (:title ,title)
       (:link :href "/css/style.css" :rel "stylesheet" :type "text/css")
       (:link :href "/bootstrap/css/bootstrap.css" :rel "stylesheet" :type "text/css")
       (:link :href "/font-awesome-4.6.3/css/font-awesome.css" :rel "stylesheet" :type "text/css")
       (:script :src "/bootstrap/js/jquery-2.2.4-min.js")
       (:script :src "/bootstrap/js/bootstrap.js")
       (:script :src "/js/confirmdelete.js"))
      (:body
        (header)
        (:div :style "min-height:450px"
         (:h2 :style "text-align:center" ,title)
         (:br)
         ,@body)
        (footer)))))

(defun header ()
  (with-html-output (*standard-output* nil :indent t)
    (:nav :class "navbar navbar-inverse navbar-fixed-top"
      (:div :class "container-fluid"
        (:div :class "navbar-header"
          (:a :class "navbar-brand" :href "/bookmarks/"
            (:i :class "glyphicon glyphicon-home"))
          (:ul :class "nav navbar-nav"
            (:li
                (:a :href "/bookmarks/add" "Add bookmark")))
          (:form :method "get" :action "/bookmarks/" :class "navbar-form navbar-left" :role "search"
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
      (:a :href "/index" (:img  :class "logo" :src "/images/logo.png"))
      (:p :style "color: black;" "Duikboot &copy; 2016")))))

(defun index ()
  (redirect "/bookmarks/"))

(defun bookmarks ()
  (let*
    ((order (get-order (get-parameter "order")))
     (key (or (intern (string-upcase (get-parameter "sort"))) 'date-added))
     (tags (create-query-sequence (or (get-parameter "tags") '()) #\space))
     (database (stable-sort (copy-list *db*) order :key key))
     (search-params (or (get-parameter "search") '()))
     (database (select :tags tags :database database))
     (database
       (search-bookmarks (create-query-sequence search-params #\space) database)))
    (render-bookmarks database tags)))

(defun trim (item)
  (string-trim '(#\space) (escape-string (post-parameter item))))

(defmacro create-query-sequence-from-post (item)
  `(create-query-sequence (trim ,item)))

(defun save-bookmark ()
  (let
    ((id (parse-integer (post-parameter "id")))
     (title (create-query-sequence-from-post "title"))
     (link (escape-string (trim "link")))
     (summary (create-query-sequence-from-post "summary"))
     (tags (create-query-sequence-from-post "tags")))
    (if (not (and title link tags))
      (bookmark-form :title title :link link :tags tags :error t)
      (if (not (= id 0))
        (update :fn (where 'id id) :title title :link link
                :summary summary :tags tags)
        (make-link title link summary tags)))
    (save)
    (redirect "/bookmarks/add")))

(defun show-error ()
  (with-html-output
    (*standard-output* nil :indent t)
    (htm
      (:div
        :class "alert alert-danger" "Title, link, and tags are obligated."))))

(defun show-success ()
  (with-html-output
    (*standard-output* nil :indent t)
    (htm
      (:div
        :class "alert alert-success fade in" "Bookmark successfully added."))))

(defun bookmark-form (&key id title link summary tags (error nil))
  (standard-page
   (:title "Add bookmark")
   (:div :style "margin: 0 auto; width: 90%"
         (:form :method "post" :class "form-horizontal" :action "/bookmarks/save"
                (:input :type "hidden" :value (if id id 0) :name "id")
                (when error
                  (show-error))
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
                            (format t "<textarea name=\"summary\" placeholder=\"Descriptive summary of site contents\" tabindex=\"3\" class=\"form-control\" rows=\"10\" cols=\"70\">")
                            (when summary (format t "~{~(~a~^ ~)~}" summary))
                            (format t "</textarea>")))
                (:div :class "form-group"
                      (:label :for "bf_tags" :class "col-sm-2 control-label" "Tags")
                      (:div :class "col-sm-10"
                            (:input :type "text" :id "bf_tags" :placeholder "space separated words"
                                    :tabindex "4" :class "form-control" :value
                                    (if tags (format nil "~{~(~a~^ ~)~}" tags) nil) :name "tags" :size 80)))
                (:div :class "form-group"
                      (:div :class "col-sm-offset-2 col-sm-10"
                            (:button :type "submit" :tabindex "5" :class "btn btn-primary" "Save")
                            (:a :tabindex "6" :href "/bookmarks/" :class "btn" "Cancel")))))))


(defun render-tags (tags tags-list)
  (with-html-output
    (*standard-output* nil :indent t)
    (htm
      (:div :class "row"
            (:div
              :class "col-md-12"
              (:div
                :class "tags-list well"
                (mapc #'(lambda (tag)
                            (htm
                              (:a :class "btn btn-primary btn-xs" :role "button"
                                  :href (format nil "?tags=~{~a~^+~}"
                                                (reverse (cons (first tag) tags-list)))
                                  (format t "~{ ~a ~}" tag)))) tags)))))))

(defun render-bookmarks (database tags-list)
  (standard-page
    (:title "Bookmarks")
    (:div :class "container"
      (render-tags (number-of-occurrences 'tags database) tags-list)
      (htm :br)
      (mapc #'(lambda (row)
                  (htm
                    (:div :class "col-md-4 col column-div"
                      (:a :target "_blank" :href
                          (format nil "~a" (link row)) (fmt "~@(~{ ~(~a~)~}~)." (title row)))
                      (:a :class "glyphicon glyphicon-pencil" :href (format nil "/bookmarks/edit/~a" (id row)))
                      (:a :class "glyphicon glyphicon-remove confirm-delete" :data-title (format nil "~a" (title row)) :href (format nil "/bookmarks/delete/~a" (id row))) (:div (fmt (format-time "Date added: "(date-added row))))
                      (:div (fmt (format-time "Date modified: " (date-modified row)))) (:div (fmt "Tags: <b><em>~{ ~(~a~) ~}</em></b>" (tags row))))))
                      ; (:div :class "block-with-text" (fmt "Summary: ~{ ~(~a~) ~}" (summary row)))
              database))))

(defun last-element (lst)
  "Return last element in list."
  (first (last lst)))

(defun get-last-element-from-uri (uri)
  (last-element (split-sequence:split-sequence #\/ uri)))

(defun delete-bookmark ()
  "Delet bookmark"
  (let ((bookmark-id
          (get-last-element-from-uri (request-uri*))))
    (delete-link (parse-integer bookmark-id)))
  (save)
  (redirect "/bookmarks/"))



(defmethod handle-request :before ((acceptor hunchentoot:acceptor)
                                   (request hunchentoot:request))
  ; (get-user)
  (multiple-value-bind (user password)(hunchentoot:authorization)
    (cond ((and (equal user *user*)
                (equal password *pass*)
            (hunchentoot:header-in* :authorization)(hunchentoot:start-session)))
          (t (hunchentoot:require-authorization)))))

; (defmethod hunchentoot:handle-request :before ((acceptor hunchentoot:acceptor)
;                                                (request hunchentoot:request))
;   (hunchentoot:start-session))


(defun edit-bookmark ()
  (let* ((bookmark-id (get-last-element-from-uri (request-uri*)))
         (bookmark (first (select :fn (where 'id (parse-integer bookmark-id))))))
   (bookmark-form :id (id bookmark)
                  :title (title bookmark)
                  :link (link bookmark)
                  :summary (summary bookmark)
                  :tags (tags bookmark))))

(defun get-bookmark ()
  (let ((bookmark-id (get-last-element-from-uri (request-uri*))))
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
