;;;; -*- Mode: Lisp; -*-

(in-package #:cl-user)

(eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op '#:cffi-grovel))

(asdf:defsystem #:xpng
  :description "Read and write PNG (Portable Network Graphics) files."
  :depends-on (#:cffi)
  :serial T
  :components ((:file "src/package")
	       (cffi-grovel:grovel-file "src/grovel")
	       (:file "src/ffi")	       
	       (:file "src/compat")
	       (:file "src/common")
	       (:file "src/image")
	       (:file "src/decode")))

