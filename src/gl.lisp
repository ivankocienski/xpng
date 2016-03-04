(in-package :cl-user)

(ql:quickload :cl-glfw3)

(defpackage :xpng-gl
  (:use :cl :cl-opengl :xpng :cl-glfw3))

(in-package :xpng-gl)

;;
;; core library
;;

(defun convert-to-rgb (image)
  (let ((it (image-type image)))
    (if (eq it :rgb)
	image

	;; conversion must happen
	(let* ((pixel-count (* (image-width image) (image-height image)))
	       (pixels (make-array (* pixel-count 3))))
	  
	  (cond
	    ((eq it :grey)
	     (loop for from-pixel from 0 to pixel-count
		and to-pixel from 0 by 3
		do (let ((grey (aref (image-pixels image) from-pixel)))
		     (setf (aref pixels    to-pixel)    grey
			   (aref pixels (+ to-pixel 1)) grey
			   (aref pixels (+ to-pixel 2)) grey))))
	    
	    ((eq it :indexed)
	     (loop for from-pixel from 0 to pixel-count
		and to-pixel from 0 by 3
		do (let ((color (aref (image-palette image)
				      (aref (image-pixels image) from-pixel))))
		     (setf (aref pixels    to-pixel)    (color-red   color)
			   (aref pixels (+ to-pixel 1)) (color-green color)
			   (aref pixels (+ to-pixel 2)) (color-blue  color)))))
	    
	    ((eq it :grey-alpha)
	     (loop for from-pixel from 0 to (* 2 pixel-count) by 2
		and to-pixel from 0 by 3
		do (let ((grey (aref (image-pixels image) from-pixel)))
		     (setf (aref pixels    to-pixel)    grey
			   (aref pixels (+ to-pixel 1)) grey
			   (aref pixels (+ to-pixel 2)) grey))))
	    
	    ((eq it :rgba))
	     (loop for from-pixel from 0 to (* 4 pixel-count) by 4
		and to-pixel from 0 by 3
		do (setf (aref pixels    to-pixel)    (aref (image-pixels image) from-pixel)
			 (aref pixels (+ to-pixel 1)) (aref (image-pixels image) (+ from-pixel 1))
			 (aref pixels (+ to-pixel 2)) (aref (image-pixels image) (+ from-pixel 2)))))
	    

	  (setf (image-type   image) :rgb
		(image-pixels image) pixels)

	  image))))


(defun convert-to-rgba (image)
  (let ((it (image-type image)))
    (if (eq it :rgba)
	image

	;; conversion must happen
	(let* ((pixel-count (* (image-width image) (image-height image)))
	       (pixels (make-array (* pixel-count 3))))
	  
	  (cond
	    ((eq it :grey)
	     (loop for from-pixel from 0 to pixel-count
		and to-pixel from 0 by 4
		do (let ((grey (aref (image-pixels image) from-pixel)))
		     (setf (aref pixels    to-pixel)    grey
			   (aref pixels (+ to-pixel 1)) grey
			   (aref pixels (+ to-pixel 2)) grey
			   (aref pixels (+ to-pixel 3)) 0))))
	    
	    ((eq it :indexed)
	     (loop for from-pixel from 0 to pixel-count
		and to-pixel from 0 by 4
		do (let ((color (aref (image-palette image)
				      (aref (image-pixels image) from-pixel))))
		     (setf (aref pixels    to-pixel)    (color-red   color)
			   (aref pixels (+ to-pixel 1)) (color-green color)
			   (aref pixels (+ to-pixel 2)) (color-blue  color)
			   (aref pixels (+ to-pixel 3)) (color-alpha color)))))
	    
	    ((eq it :grey-alpha)
	     (loop for from-pixel from 0 to (* 2 pixel-count) by 2
		and to-pixel from 0 by 4
		do (let ((grey (aref (image-pixels image) from-pixel)))
		     (setf (aref pixels    to-pixel)    grey
			   (aref pixels (+ to-pixel 1)) grey
			   (aref pixels (+ to-pixel 2)) grey
			   (aref pixels (+ to-pixel 3)) (aref (image-pixels image) (1+ from-pixel))))))
	    
	    ((eq it :rgb)
	     (loop for from-pixel from 0 to (* 3 pixel-count) by 3
		and to-pixel from 0 by 4
		do (setf (aref pixels    to-pixel)    (aref (image-pixels image) from-pixel)
			 (aref pixels (+ to-pixel 1)) (aref (image-pixels image) (+ from-pixel 1))
			 (aref pixels (+ to-pixel 2)) (aref (image-pixels image) (+ from-pixel 2))
			 (aref pixels (+ to-pixel 3)) 0))))
	  

	  (setf (image-type   image) :rgba
		(image-pixels image) pixels)

	  image))))

(defun load-file-to-texture (filename &optional (pixel-format :rgba))
  (let ((image (let ((i (xpng::decode-file filename)))
		 (cond
		   ((eq pixel-format :rgba) (convert-to-rgba i))
		   ((eq pixel-format :rgb)  (convert-to-rgb  i))
		   (t (error "(load-file-to-texture) pixel-format must be :RGB or :RGBA"))))))

    ;;(format t "pixels=~s~%" (image-pixels image))
    
    (gl:tex-image-2d :TEXTURE-2D
		     0
		     pixel-format
		     (xpng:image-width image)
		     (xpng:image-height image)
		     0 ;; border
		     pixel-format
		     :UNSIGNED-BYTE
		     (xpng:image-pixels image))

    image))


;;
;; demo code
;;


(defconstant +TEXTURE-FILE-PATH+ "demo/texture.png")
(defparameter *texture* nil)

(defun render ()
  (gl:clear :color-buffer)
  (gl:load-identity)
  
  (gl:bind-texture :TEXTURE-2D *texture*)
  (gl:color 1 1 1)
  
  (gl:with-primitives :quads
    (gl:tex-coord 0 0) (gl:vertex   0   0)
    (gl:tex-coord 1 0) (gl:vertex 256   0)
    (gl:tex-coord 1 1) (gl:vertex 256 256)
    (gl:tex-coord 0 1) (gl:vertex   0 256)))

(glfw:def-key-callback key-callback (window key scancode action mod-keys)
  (when (and (eq key :escape) (eq action :press))
    (glfw:set-window-should-close)))

(defun demo ()
  (glfw:with-init-window (:title "PNG loading GL demo" :width 640 :height 480)

    ;; init
    (glfw:set-key-callback 'key-callback)

    ;; GL init
    (gl:clear-color 0.4 0.4 1 0)
    (gl:viewport 0 0 640 480)
    (gl:enable :TEXTURE-2D)
    (gl:enable :BLEND)
    (gl:blend-func :SRC-ALPHA :ONE-MINUS-SRC-ALPHA)

    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:ortho 0   640
	      480 0
	      -1  1)
    
    (gl:matrix-mode :modelview)
    (gl:load-identity)

    ;; load texture
    (setf *texture* (first (gl:gen-textures 1)))
    (gl:bind-texture :TEXTURE-2D *texture*)

    (gl:tex-parameter :TEXTURE-2D :TEXTURE-MIN-FILTER :NEAREST )
    (gl:tex-parameter :TEXTURE-2D :TEXTURE-MAG-FILTER :NEAREST )
    (gl:tex-parameter :TEXTURE-2D :TEXTURE-WRAP-S :CLAMP)
    (gl:tex-parameter :TEXTURE-2D :TEXTURE-WRAP-T :CLAMP)

    (let ((path (merge-pathnames +TEXTURE-FILE-PATH+
				  (asdf:system-source-directory :xpng-gl))))

      (format t "loading ~s~%" path)
      
      (load-file-to-texture path :rgb))
    

    ;; main loop
    (loop until (glfw:window-should-close-p)
       do (render)
       do (glfw:swap-buffers)
       do (glfw:poll-events))))


