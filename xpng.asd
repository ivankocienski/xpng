;;;; -*- Mode: Lisp; -*-

(in-package #:cl-user)

(eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op '#:cffi-grovel))

#+cffi-features:darwin
(push #p"/usr/X11/lib/" cffi:*foreign-library-directories*)

#+cffi-features:darwin
(push #p"/opt/local/lib/" cffi:*foreign-library-directories*)

(asdf:defsystem #:xpng
  :description "Read and write PNG (Portable Network Graphics) files."
  :perform (asdf:load-op :after (op xpng)
			 (pushnew :xpng *features*))
  :components ((:file "src/package")
  (:file "src/ffi")
  (:file "src/common")
  (:file "src/encode")
  (:file "src/decode")
	       (:file "src/compat" :depends-on ("src/package"))
	       (:file "src/image" :depends-on ("src/package" "src/compat"))
	       (cffi-grovel:grovel-file "src/grovel" :depends-on ("src/package")))
  :depends-on (#:cffi))

