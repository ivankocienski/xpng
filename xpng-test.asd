;;;; -*- Mode: Lisp; -*-

(asdf:defsystem :xpng-test
  :components ((:file "test" :depends-on ("lisp-unit"))
	       (:file "lisp-unit")
	       )
  :depends-on (#:xpng))

