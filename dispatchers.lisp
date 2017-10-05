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
(push (create-static-file-dispatcher-and-handler "/css/style.css" "css/style.css") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/bootstrap/css/bootstrap.css" "bootstrap/css/bootstrap.css") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/bootstrap/js/bootstrap.js" "bootstrap/js/bootstrap.js") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/bootstrap/js/jquery-2.2.4-min.js" "bootstrap/js/jquery-2.2.4-min.js") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/js/confirmdelete.js" "js/confirmdelete.js") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/bootstrap/fonts/glyphicons-halflings-regular.ttf" "bootstrap/fonts/glyphicons-halflings-regular.ttf") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/bootstrap/fonts/glyphicons-halflings-regular.woff" "bootstrap/fonts/glyphicons-halflings-regular.woff") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/bootstrap/fonts/glyphicons-halflings-regular.woff2" "bootstrap/fonts/glyphicons-halflings-regular.woff2") *dispatch-table*)

;;font awesome fonts
(push (create-static-file-dispatcher-and-handler
        "/font-awesome-4.6.3/css/font-awesome.css"
        "font-awesome-4.6.3/css/font-awesome.css")
      *dispatch-table*)
(push (create-static-file-dispatcher-and-handler
        "/font-awesome-4.6.3/fonts/fontawesome-webfont.woff2"
        "font-awesome-4.6.3/fonts/fontawesome-webfont.woff2")
      *dispatch-table*)
(push (create-static-file-dispatcher-and-handler
        "/font-awesome-4.6.3/fonts/fontawesome-webfont.woff"
        "font-awesome-4.6.3/fonts/fontawesome-webfont.woff")
      *dispatch-table*)
(push (create-static-file-dispatcher-and-handler
        "/font-awesome-4.6.3/fonts/fontawesome-webfont.ttf"
        "font-awesome-4.6.3/fonts/fontawesome-webfont.ttf")
      *dispatch-table*)
