(in-package :cl-user)

(defpackage :xpng-gl
  (:use :cl :cl-opengl :xpng))

(in-package :xpng-gl)

;;
;; core library
;;

(defun convert-to-rgb (image)
  (let ((it (image-format image)))
    (if (eq it +png-color-type-rgb+)
	image

	;; conversion must happen
	(let* ((pixel-count (* (image-width image) (image-height image)))
	       (pixels (make-array (* pixel-count 3))))
	  
	  (cond
	    ((eq it +png-color-type-gray+)
	     (loop for from-pixel from 0 to pixel-count
		and to-pixel from 0 by 3
		do (let ((grey (aref (image-pixels image) from-pixel)))
		     (setf (aref pixels    to-pixel)    grey
			   (aref pixels (+ to-pixel 1)) grey
			   (aref pixels (+ to-pixel 2)) grey))))
	    
	    ((eq it +png-color-type-palette+)
	     (loop for from-pixel from 0 to pixel-count
		and to-pixel from 0 by 3
		do (let ((color (aref (image-palette image)
				      (aref (image-pixels image) from-pixel))))
		     (setf (aref pixels    to-pixel)    (color-red   color)
			   (aref pixels (+ to-pixel 1)) (color-green color)
			   (aref pixels (+ to-pixel 2)) (color-blue  color)))))
	    
	    ((eq it +png-color-type-gray-alpha+)
	     (loop for from-pixel from 0 to (* 2 pixel-count) by 2
		and to-pixel from 0 by 3
		do (let ((grey (aref (image-pixels image) from-pixel)))
		     (setf (aref pixels    to-pixel)    grey
			   (aref pixels (+ to-pixel 1)) grey
			   (aref pixels (+ to-pixel 2)) grey))))
	    
	    ((eq it +png-color-type-rgba+))
	     (loop for from-pixel from 0 to (* 4 pixel-count) by 4
		and to-pixel from 0 by 3
		do (setf (aref pixels    to-pixel)    (aref (image-pixels image) from-pixel)
			 (aref pixels (+ to-pixel 1)) (aref (image-pixels image) (+ from-pixel 1))
			 (aref pixels (+ to-pixel 2)) (aref (image-pixels image) (+ from-pixel 2)))))
	    

	  (setf (image-format +png-color-type-rgb+)
		(image-pixels pixels))

	  image))))


(defun convert-to-rgba (image)
  (let ((it (image-format image)))
    (if (eq it +png-color-type-rgba+)
	image

	;; conversion must happen
	(let* ((pixel-count (* (image-width image) (image-height image)))
	       (pixels (make-array (* pixel-count 3))))
	  
	  (cond
	    ((eq it +png-color-type-gray+)
	     (loop for from-pixel from 0 to pixel-count
		and to-pixel from 0 by 4
		do (let ((grey (aref (image-pixels image) from-pixel)))
		     (setf (aref pixels    to-pixel)    grey
			   (aref pixels (+ to-pixel 1)) grey
			   (aref pixels (+ to-pixel 2)) grey
			   (aref pixels (+ to-pixel 3)) 0))))
	    
	    ((eq it +png-color-type-palette+)
	     (loop for from-pixel from 0 to pixel-count
		and to-pixel from 0 by 4
		do (let ((color (aref (image-palette image)
				      (aref (image-pixels image) from-pixel))))
		     (setf (aref pixels    to-pixel)    (color-red   color)
			   (aref pixels (+ to-pixel 1)) (color-green color)
			   (aref pixels (+ to-pixel 2)) (color-blue  color)
			   (aref pixels (+ to-pixel 3)) (color-alpha color)))))
	    
	    ((eq it +png-color-type-gray-alpha+)
	     (loop for from-pixel from 0 to (* 2 pixel-count) by 2
		and to-pixel from 0 by 4
		do (let ((grey (aref (image-pixels image) from-pixel)))
		     (setf (aref pixels    to-pixel)    grey
			   (aref pixels (+ to-pixel 1)) grey
			   (aref pixels (+ to-pixel 2)) grey
			   (aref pixels (+ to-pixel 3)) (aref (image-pixels image) (1+ from-pixel))))))
	    
	    ((eq it +png-color-type-rgb+))
	     (loop for from-pixel from 0 to (* 3 pixel-count) by 3
		and to-pixel from 0 by 4
		do (setf (aref pixels    to-pixel)    (aref (image-pixels image) from-pixel)
			 (aref pixels (+ to-pixel 1)) (aref (image-pixels image) (+ from-pixel 1))
			 (aref pixels (+ to-pixel 2)) (aref (image-pixels image) (+ from-pixel 2))
			 (aref pixels (+ to-pixel 3)) 0)))
	    

	  (setf (image-format +png-color-type-rgb+)
		(image-pixels pixels))

	  image))))

(defun load-file-to-texture (filename &optional (pixel-format :rgba))
  (let ((image (let ((i (xpng:decode-file filename)))
		 (cond
		   ((eq pixel-format :rgba) (convert-to-rgba i))
		   ((eq pixel-format :rgb)  (convert-to-rgb  i))
		   (t (error "(load-file-to-texture) pixel-format must be :RGB or :RGBA"))))))

    (gl:tex-image-2d :TEXTURE-2D
		     0
		     pixel-format
		     (xpng:image-width image)
		     (xpng:image-height image)
		     0 ;; border
		     pixel-format
		     :UNSIGNED-BYTE
		     (xpng:image-pixels image))))


;;
;; demo code
;;

#|
;;(with-open-file (input ( merge-pathnames filename) :element-type '(unsigned-byte 8))
;;  (xpng:decode input))))

      (gl:tex-parameter :TEXTURE-2D :TEXTURE-MIN-FILTER :NEAREST )
      (gl:tex-parameter :TEXTURE-2D :TEXTURE-MAG-FILTER :NEAREST )
      (gl:tex-parameter :TEXTURE-2D :TEXTURE-WRAP-S :CLAMP)
      (gl:tex-parameter :TEXTURE-2D :TEXTURE-WRAP-T :CLAMP)
|#
