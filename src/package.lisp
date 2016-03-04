;;;; -*- Mode: Lisp;  -*-

(defpackage #:xpng
  (:documentation "Read and write PNG (Portable Network Graphics) files.")
  (:use #:common-lisp #:cffi)
  (:shadow #:make-shareable-byte-vector
	   #+(or allegro clisp) #:with-pointer-to-vector-data)
  
  (:export #:image
	   #:image-type
	   #:image-width
	   #:image-height
	   #:image-palette
	   #:image-pixels

	   #:color-red
	   #:color-green
	   #:color-blue
	   #:color-alpha
	   
	   #:decode
	   #:decode-file))


