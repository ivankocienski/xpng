(in-package :cl-user)

(defpackage :xpng-gl-demo
  (:use :cl :cl-opengl :xpng :xpng-gl :cl-glfw3)
  (:export :demo))

(in-package :xpng-gl-demo)

(defparameter *txt-grass* nil)
(defparameter *txt-balls* nil)
(defparameter *balls* nil)

(defstruct ball
  (xpos 0)
  (ypos 0)
  (yinc 0)
  (texture nil))

(defun render ()
  (gl:clear :color-buffer)
  (gl:load-identity)
  
  (gl:color 1 1 1)
  (gl:bind-texture :TEXTURE-2D *txt-grass*)

  (gl:with-primitives :quads

    (loop for x1 from 0 upto 640 by 64
       do (loop for y1 from 0 upto 480 by 64
	     do (let ((x2 (+ x1 64))
		      (y2 (+ y1 64)))
		   
		  (gl:tex-coord 0 0) (gl:vertex x1 y1)
		  (gl:tex-coord 1 0) (gl:vertex x2 y1)
		  (gl:tex-coord 1 1) (gl:vertex x2 y2)
		  (gl:tex-coord 0 1) (gl:vertex x1 y2)))))

  (dolist (ball *balls*)
    (incf (ball-ypos ball) (ball-yinc ball))
    (if (> (ball-ypos ball) 480)
	(setf (ball-xpos ball) (random 608)
	      (ball-ypos ball) -32
	      (ball-yinc ball) (* 4.0 (random 2.0))
	      (ball-texture ball) (nth (random (length *txt-balls*)) *txt-balls*)))

    (gl:bind-texture :TEXTURE-2D (ball-texture ball))
    
    (gl:with-primitives :quads
      (let* ((x1 (ball-xpos ball))
	     (y1 (ball-ypos ball))
	     (x2 (+ x1 32))
	     (y2 (+ y1 32)))
		   
	(gl:tex-coord 0 0) (gl:vertex x1 y1)
	(gl:tex-coord 1 0) (gl:vertex x2 y1)
	(gl:tex-coord 1 1) (gl:vertex x2 y2)
	(gl:tex-coord 0 1) (gl:vertex x1 y2)))))

(glfw:def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (glfw:set-window-should-close)))

(defun load-texture (id file type)
  (format t "loading ~s~%" file)
  
  (gl:bind-texture :TEXTURE-2D id)

  (gl:tex-parameter :TEXTURE-2D :TEXTURE-MIN-FILTER :NEAREST )
  (gl:tex-parameter :TEXTURE-2D :TEXTURE-MAG-FILTER :NEAREST )
  (gl:tex-parameter :TEXTURE-2D :TEXTURE-WRAP-S :CLAMP)
  (gl:tex-parameter :TEXTURE-2D :TEXTURE-WRAP-T :CLAMP)

  (let* ((base-dir (asdf:system-source-directory :xpng-gl-demo))
	 (path     (merge-pathnames file base-dir))
	 (image    (load-file-to-texture path type)))

    (format t "    type = ~s~%" (image-type image))
    (format t "   width = ~d~%" (image-width image))
    (format t "  height = ~d~%" (image-height image))

    image))

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

    (setf *txt-grass* (first (gl:gen-textures 1)))
    (load-texture *txt-grass* "demo/grass.png" :rgb)

    (setf *txt-balls* (gl:gen-textures 3))
    (load-texture (nth 0 *txt-balls*) "demo/ball-red.png"    :rgba)
    (load-texture (nth 1 *txt-balls*) "demo/ball-yellow.png" :rgba)
    (load-texture (nth 2 *txt-balls*) "demo/ball-purple.png" :rgba)

    (labels ((make-ball-instance (c)
	       (if (> c 0)
		   (cons (make-ball :xpos (random 608)
				    :ypos (random 448)
				    :yinc (random 2.0)
				    :texture (nth (random (length *txt-balls*)) *txt-balls*))

			 (make-ball-instance (1- c))))))

      (setf *balls* (make-ball-instance 100)))

    ;; main loop
    (loop until (glfw:window-should-close-p)
       do (render)
       do (glfw:swap-buffers)
       do (glfw:poll-events))))
