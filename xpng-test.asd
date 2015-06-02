;;;; -*- Mode: Lisp; -*-

(asdf:defsystem :xpng-test
  :components ((:file "test/test"))
  :depends-on (#:xpng :lisp-unit))

