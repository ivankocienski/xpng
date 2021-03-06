
(in-package :xpng)

(defun read-image-header (png info image)
  
  (with-foreign-pointer       (width      (foreign-type-size :uint32))
    (with-foreign-pointer     (height     (foreign-type-size :uint32))
      (with-foreign-pointer   (bit-depth  (foreign-type-size :int))
	(with-foreign-pointer (color-type (foreign-type-size :int))
		  
	  (png-read-info png info)

	  (png-get-ihdr png
			info
			
			width
			height
			bit-depth
			color-type
			
			(null-pointer)  ; interlaced
			(null-pointer)  ; compression
			(null-pointer)) ; filter

	  ;; downsample 16bit to 8bit
	  (png-set-strip-16 png)

	  ;; read 1,2,4 bit fields as 8bit
	  (png-set-packing png)
	  
	  (setf (image-width  image) (mem-ref width      :uint32)
		(image-height image) (mem-ref height     :uint32)
		(image-type   image) (let ((it (mem-ref color-type :int)))
				       (cond
					 ((eq it +png-color-type-palette+)    :indexed)
					 ((eq it +png-color-type-gray+)       :grey)
					 ((eq it +png-color-type-gray-alpha+) :grey-alpha)
					 ((eq it +png-color-type-rgb+)        :rgb)
					 ((eq it +png-color-type-rgba+)       :rgba))))
	  ))))
  image)

(defun read-image-palette (png info image)
  (if (eq (image-type image) :indexed)
      (progn
	;; ... read pallet (only 8 bit)
	(with-foreign-pointer (num-colors (foreign-type-size :int))
	  (with-foreign-pointer (palette-ptr-ptr (foreign-type-size :pointer))

	    (png-get-plte png info palette-ptr-ptr num-colors)
	    
	    (let* ((palette-ptr    (mem-ref palette-ptr-ptr :pointer))
		   (palette-offset 0)
		   (num-colors     (mem-ref num-colors :int))
		   (image-colors   (make-array num-colors)))
	      
	      (dotimes (i num-colors)
		
		(let ((red        (mem-aref palette-ptr :uchar    palette-offset )) 
		      (green      (mem-aref palette-ptr :uchar (+ palette-offset 1))) 
		      (blue       (mem-aref palette-ptr :uchar (+ palette-offset 2))))

		  (setf (aref image-colors i)
			(make-color :red   red
				    :green green
				    :blue  blue
				    :alpha 0))
		  
		  (incf palette-offset 3)))

	      (setf (image-palette image) image-colors))))))

  image)

(defun read-image-pixels (png info image)

  (setf (image-pixels image) (make-array (* (image-width image)
					    (image-height image)
					    (image-pixel-size image))))
    
  (with-row-pointers (row-pointers image)
      
    (png-set-rows   png info row-pointers)
    (png-read-image png row-pointers))

  image)

(defun decode (file-stream)
  (let ((image (make-image)))

    (with-png-struct (png)

      (setf *stream* file-stream)
      
      (png-set-read-fn png
		       (null-pointer)
		       (callback user-read-data))
      
      (with-png-info-struct (info png)
	
	(read-image-header png info image)
      
	(read-image-pixels png info image)

	(png-read-end png info)
	
	(read-image-palette png info image)))
    
    image))

(defun decode-file (pathname)
  (with-open-file (input pathname :element-type '(unsigned-byte 8))
    (decode input)))
