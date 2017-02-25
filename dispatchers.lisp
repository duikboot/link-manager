(in-package :link-manager)

(setq *dispatch-table* (list (create-regex-dispatcher "^/$" 'index)
                             (create-regex-dispatcher "^/bookmarks/$"
                                                      'bookmarks)
                             (create-regex-dispatcher "^/bookmarks/add$"
                                                      'bookmark-form)
                             (create-regex-dispatcher
                               "^/bookmarks/delete/[0-9]+" 'delete-bookmark)
                             (create-regex-dispatcher "^/bookmarks/save"
                                                      'save-bookmark)
                             (create-regex-dispatcher "^/bookmarks/[0-9]+$"
                                                      'get-bookmark)
                             (create-regex-dispatcher
                               "^/bookmarks/edit/[0-9]+$" 'edit-bookmark)))

(push (create-static-file-dispatcher-and-handler "/images/logo.png" "images/logo.png") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/style.css" "style/style.css") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/bootstrap.css" "bootstrap/css/bootstrap.css") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/bootstrap-js.js" "bootstrap/js/bootstrap.js") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/jquery.js" "bootstrap/js/jquery-2.2.4-min.js") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/confirmdelete.js" "javascript/confirmdelete.js") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/fonts/glyphicons-halflings-regular.ttf" "bootstrap/fonts/glyphicons-halflings-regular.ttf") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/fonts/glyphicons-halflings-regular.woff" "bootstrap/fonts/glyphicons-halflings-regular.woff") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/fonts/glyphicons-halflings-regular.woff2" "bootstrap/fonts/glyphicons-halflings-regular.woff2") *dispatch-table*)

;;font awesome fonts
(push (create-static-file-dispatcher-and-handler
        "/font-awesome.css"
        "font-awesome-4.6.3/css/font-awesome.css")
      *dispatch-table*)
(push (create-static-file-dispatcher-and-handler
        "/fonts/fontawesome-webfont.woff2"
        "font-awesome-4.6.3/fonts/fontawesome-webfont.woff2")
      *dispatch-table*)
(push (create-static-file-dispatcher-and-handler
        "/fonts/fontawesome-webfont.woff "
        "font-awesome-4.6.3/fonts/fontawesome-webfont.woff")
      *dispatch-table*)
(push (create-static-file-dispatcher-and-handler
        "/fonts/fontawesome-webfont.ttf"
        "font-awesome-4.6.3/fonts/fontawesome-webfont.ttf")
      *dispatch-table*)
