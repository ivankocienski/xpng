
#|
This is another way of testing the decoder for various types of
file format 

To run
  ; [load xpng first if it is not already in asdf load path]
  (load "test-decode")
  (test-decode:run)
|#

(defpackage :test-decode
  (:use :cl :xpng))

(in-package :test-decode)

(defun test-load-channels (from should-have-channels)
    (let ((image 
	 (with-open-file (input ( merge-pathnames filename) :element-type '(unsigned-byte 8))
	   (xpng:decode input))))

    (format t "png.width=~d~%" (xpng:image-width image))
    (format t "png.height=~d~%" (xpng:image-height image))
    (format t "png.channels=~d~%" (xpng:image-channels image))
    (format t "png.bits=~d~%" (xpng:image-bit-depth image))

    (let ((pixels (make-array
		   (*
		    (xpng:image-width image)
		    (xpng:image-height image)
		    (xpng:image-channels image)))))
  )

(defun run ()
  
  ;; paletted source, no alpha
  (test-load-channels "image-1-bit" 3)
  (test-load-channels "image-2-bit" 3)
  (test-load-channels "image-4-bit" 3)
  (test-load-channels "image-8-bit" 3)
  
  ;; palette with alpha channel
  (test-load-channels "image-1-bit-trns" 4)
  (test-load-channels "image-2-bit-trns" 4)
  (test-load-channels "image-4-bit-trns" 4)
  (test-load-channels "image-8-bit-trns" 4)
  
  
  (test-load-channels "image-gray" 1)
  (test-load-channels "image-gray-trans" 2)

  
  (test-load-channels "image-rgb" 3)
  (test-load-channels "image-rgba" 4)
  
  )


