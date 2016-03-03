
(in-package :xpng)

#|
(defun decode (input)
  "Reads an image in PNG format from input and returns an array of
   type IMAGE.  If the bit depth of the PNG file is less than or equal to
   8, an 8-BIT-IMAGE will be returned; otherwise, a 16-BIT-IMAGE will be
   returned.

   Applications that would like to receive images of consistent bit
   depth (rather than 8 or 16 depending on the PNG file) can apply the
   function 8-BIT-IMAGE or the function 16-BIT-IMAGE to the result of
   DECODE.

   Bit depths less than 8 will be converted to 8 bits when read, and bit
   depths between 8 and 16 bits will be converted to 16 bits.  As an
   example, 2-bit PNG files contain only the pixel values 0, 1, 2, and 3.
   These will be converted to 0, 85, 170, and 255, respectively, in order
   to fill the dynamic range of the 8-bit image that is returned.

   Signals an error if reading the image fails."
  (with-png-struct (png-ptr :direction :input)
    (with-png-info-struct (info-ptr png-ptr (png-create-info-struct png-ptr))
      (with-png-info-struct (end-ptr png-ptr (png-create-info-struct png-ptr))

	(let ((*stream* input) (read-channels 0))

	  (png-set-read-fn png-ptr (null-pointer) (callback user-read-data))

	  (png-read-info png-ptr info-ptr)

	  (multiple-value-bind (width height bit-depth color-type) (get-ihdr png-ptr info-ptr) 

      (cond
        ((= color-type +png-color-type-palette+) 
         (progn
           (png-set-palette-to-rgb png-ptr)
           (if (png-get-valid png-ptr info-ptr +png-info-trns+)
             (progn
               (setf read-channels 4)
               (png-set-trns-to-alpha png-ptr))
             (setf read-channels 3)
           )))

        ((= color-type +png-color-type-gray+) 
         (progn
           (png-set-expand-gray-1-2-4-to-8 png-ptr)
           (if (png-get-valid png-ptr info-ptr +png-info-trns+)
             (progn
               (setf read-channels 2)
               (png-set-trns-to-alpha png-ptr))
             (setf read-channels 1))))

        ((= color-type +png-color-type-gray-alpha+)
         (progn
           (setf read-channels 2)
           (png-set-palette-to-rgb png-ptr)))

        ((= color-type +png-color-type-rgb+)
         (setf read-channels 3))
        
        ((= color-type +png-color-type-rgba+)
         (progn
           (setf read-channels 4)
           (png-set-expand png-ptr)))
        )

      #+little-endian
      (if (= bit-depth 16) 
	  (png-set-swap png-ptr))

      (let ((image (make-image height width read-channels bit-depth)))
	(with-row-pointers (row-pointers image)
	  
	  (png-set-rows png-ptr info-ptr row-pointers)
	  (png-read-image png-ptr row-pointers))


	image)))))))

(defun decode-file (pathname)
  (with-open-file (input pathname :element-type '(unsigned-byte 8))
    (decode input)))

|#

















;; yeah so- eh?

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
		(image-depth  image) (mem-ref bit-depth  :int)
		(image-type   image) (mem-ref color-type :int))

	  (format t "image-header:~%")
	  (format t "  width: ~d~%" (image-width  image))
	  (format t " height: ~d~%" (image-height image))
	  (format t "  depth: ~d~%" (image-depth  image))
	  (format t " colort: ~d~%" (image-type   image))
	  
	  ))))
  image)

#|
(defun read-image-palette (png info image)
  (if (= (image-type image) +png-color-type-palette+)
      (progn
	;; ... read pallet (only 8 bit)
	(with-foreign-pointer (num-colors (foreign-type-size :int))
;;	  (with-foreign-pointer (palette-buffer (* 256 3 (foreign-type-size :uchar)))
	  (with-foreign-pointer (palette-buffer-ptr (foreign-type-size :pointer))

	    (png-get-plte png info (pointer-address palette-buffer-ptr) num-colors)
	    
	    ;;(png-get-plte png info palette-buffer num-colors)

	    (let ((pp (make-pointer palette-buffer-ptr)))
	    
	    (labels ((gather-colors (byte-offset)
		       (if (> byte-offset -1)
			   (let* ((red        (mem-aref palette-buffer-ptr :uchar    byte-offset  )) 
				  (green      (mem-aref palette-buffer-ptr :uchar (+ byte-offset 1))) 
				  (blue       (mem-aref palette-buffer-ptr :uchar (+ byte-offset 2))))
			     
			     (cons (list red green blue) (gather-colors (- byte-offset 3)))))))

	      (let* ((pal-size (mem-ref num-colors :int))
		     (palette  (gather-colors (* 3 (1- pal-size)))))
		
		(format t "palette:~%")
		(format t "  num-colors=~d~%" num-colors)
		(format t " palette=~s~%" palette)
		
		(setf (image-palette image) palette)))))))
      image)
  |#

(defun read-image-palette (png info image)
  (if (= (image-type image) +png-color-type-palette+)
      (progn
	;; ... read pallet (only 8 bit)
	(with-foreign-pointer (num-colors (foreign-type-size :int))
	  (with-foreign-pointer (palette-ptr-ptr (foreign-type-size :pointer))

	    (png-get-plte png info palette-ptr-ptr num-colors)
	    
	    (let ((palette-ptr (mem-ref palette-ptr-ptr :pointer)))
	      
	      (labels ((gather-colors (byte-offset)
			 (if (> byte-offset -1)
			     (let* ((red        (mem-aref palette-ptr :uchar    byte-offset  )) 
				    (green      (mem-aref palette-ptr :uchar (+ byte-offset 1))) 
				    (blue       (mem-aref palette-ptr :uchar (+ byte-offset 2))))
			       
			       (cons (list red green blue) (gather-colors (- byte-offset 3)))))))

		(let* ((pal-size (mem-ref num-colors :int))
		       (palette  (gather-colors (* 3 (1- pal-size)))))
		  
		  (format t "palette:~%")
		  (format t "  num-colors=~d~%" num-colors)
		  (format t " palette=~s~%" palette)
		  
		  (setf (image-palette image) palette))))))))
  image)

(defun read-image-pixels (png info image)

  (setf (image-pixels image) (make-array (* (image-width image)
					    (image-height image)
					    (image-pixel-size image))))
    
  (with-row-pointers (row-pointers image)
      
    (png-set-rows   png info row-pointers)
    (png-read-image png row-pointers))

  image)

(defun ik-decode (file-stream)
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

(defun ik-decode-file (pathname)
  (with-open-file (input pathname :element-type '(unsigned-byte 8))
    (ik-decode input)))
