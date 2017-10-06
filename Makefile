run:
	lisp --eval "(ql:quickload :link-manager)" --eval "(in-package :link-manager)" --eval "(in-package :link-manager)" --eval "(start-app)"

run-sbcl:
	sbcl --eval "(ql:quickload :link-manager)" --eval "(in-package :link-manager)" --eval "(in-package :link-manager)" --eval "(start-app)"
