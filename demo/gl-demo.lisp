(in-package :cl-user)

(defpackage :xpng-gl-demo
  (:use :cl :cl-opengl :xpng :xpng-gl :cl-glfw3)
  (:export :demo))

(in-package :xpng-gl-demo)

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
  (declare (ignore window scancode mod-keys))
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
