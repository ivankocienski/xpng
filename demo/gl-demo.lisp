
(defpackage :xpng-gl-demo
  (:use :cl :glfw3 :opengl :xpng)
  (:export :main))

(in-package :xpng-gl-demo)

(defconstant +XRES+ 800)
(defconstant +YRES+ 600)

(defparameter *letters* nil)

(defun load-png-to-texture (filename)
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
      
      (let ((p 0))
	(dotimes (y (xpng:image-height image))
	  (dotimes (x (xpng:image-width image))
	    
	    (setf ( aref pixels p) (aref image y x 0) )
	    (setf ( aref pixels (+ p 1)) (aref image y x 1) )
	    (setf ( aref pixels (+ p 2)) (aref image y x 2) )
	    (setf ( aref pixels (+ p 3)) (aref image y x 3) )
	    
	    (incf p 4))))

	  ;;; pixels now has the array filled with image data

      (gl:tex-parameter :TEXTURE-2D :TEXTURE-MIN-FILTER :NEAREST )
      (gl:tex-parameter :TEXTURE-2D :TEXTURE-MAG-FILTER :NEAREST )
      (gl:tex-parameter :TEXTURE-2D :TEXTURE-WRAP-S :CLAMP)
      (gl:tex-parameter :TEXTURE-2D :TEXTURE-WRAP-T :CLAMP)
      
      (gl:tex-image-2d 
       :TEXTURE-2D
       0
       :RGBA
       (xpng:image-width image)
       (xpng:image-height image)
       0 ;; border
       :RGBA
       :UNSIGNED-BYTE
       pixels)

      )))

(defun init ()
  
  (gl:viewport 0 0 +XRES+ +YRES+)
  (gl:matrix-mode :PROJECTION)
  (gl:load-identity)

  (gl:ortho 0.0 +XRES+ +YRES+ 0.0 -1.0 1.0)

  (gl:matrix-Mode :MODELVIEW)
  (gl:load-identity)

  (gl:enable :TEXTURE-2D)
  (gl:enable :BLEND)
  (gl:blend-func :SRC-ALPHA :ONE-MINUS-SRC-ALPHA)

  (gl:clear-color 0.2 0.2 0.2 0.0)
  (gl:color 1 1 1 )

  (setf *letters* (gl:gen-textures 3))
  (gl:bind-texture :TEXTURE-2D (first *letters*))
  (load-png-to-texture "letter-p.png")
  
  (gl:bind-texture :TEXTURE-2D (second *letters*))
  (load-png-to-texture "letter-n.png")
  
  (gl:bind-texture :TEXTURE-2D (third *letters*))
  (load-png-to-texture "letter-g.png")
  )

(defun render ()
  (gl:clear :color-buffer)

  (gl:bind-texture (first *letters*))
  (gl:with-primitive :quads
    (gl:tex-coord 0 0)
    (gl:vertex 0 0)

    (gl:tex-coord 1 0)
    (gl:vertex 32 0)

    (gl:tex-coord 1 1)
    (gl:vertex 32 40)

    (gl:tex-coord 0 1)
    (gl:vertex 0 40))
    
  )

(defun main ()
  (with-init-window (:title "GL Window" :width +XRES+ :height +YRES+)
    
    (init)
    
    (loop until (window-should-close-p)
       do (render)
       do (swap-buffers)
       do (poll-events))))
