;;;; -*- Mode: Lisp; -*-

(in-package #:cl-user)

(eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op '#:cffi-grovel))

(asdf:defsystem #:xpng
  :description "Read PNG (Portable Network Graphics) files."
  :depends-on (#:cffi)
  :serial T
  :components ((:file "src/package")
	       (cffi-grovel:grovel-file "src/grovel")
	       (:file "src/ffi")	       
	       (:file "src/compat")
	       (:file "src/common")
	       (:file "src/image")
	       (:file "src/decode")))


(asdf:defsystem #:xpng-gl
  :description "Upload PNGs to GL textures"
  :depends-on (#:xpng #:cl-opengl)
  :serial t
  :components ((:file "src/gl")))

(asdf:defsystem #:xpng-gl-demo
  :description "Demo app for XPNG-GL"
  :depends-on (#:xpng-gl #:cl-glfw3)
  :serial t
  :components ((:file "demo/gl-demo")))
