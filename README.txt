# PREREQUISITES

* sbcl (version 1.2.7+)
* [quicklisp](https://www.quicklisp.org/beta/)
* [vlime](https://github.com/l04m33/vlime)
** cd ~/quicklisp/local-projects/
** git clone git@github.com:l04m33/vlime.git

# RUN
sbcl --eval "(ql:quickload :link-manager)" --eval "(in-package :link-manager)" --eval "(start-app)"

# Password-protection
The bookmarks web-app is password protected. To let Lisp load your credentials on startups, rename `passwd.sample` to `passwd` and edit the credentials inside.