(in-package #:xpng)

#|
(defmacro with-png-struct ((var &key (direction :input)) &body body)
  (let ((pointer (gensym "POINTER")))
    `(let ((,var (,(ecase direction
			  (:input 'png-create-read-struct)
			  (:output 'png-create-write-struct))
                   +png-libpng-ver-string+ (null-pointer)
                   (callback error-fn) (callback warn-fn)))
           (*buffer* (make-shareable-byte-vector 1024)))
       
       (when (null-pointer-p ,var)
         (error "Failed to allocate PNG write struct."))
       
       (unwind-protect (progn ,@body)
         (with-foreign-pointer (,pointer (foreign-type-size :pointer))
	   (setf (mem-ref ,pointer :pointer) ,var)
	   
	   ,(ecase direction
		   (:input `(png-destroy-read-struct ,pointer (null-pointer) 
						     (null-pointer)))
		   
		   (:output `(png-destroy-write-struct ,pointer 
						       (null-pointer)))))))))
|#

(defmacro with-png-struct ((var) &body body)
  "allocate space for PNG data structure"
  (let ((pointer (gensym "POINTER")))
    `(let ((,var (png-create-read-struct +png-libpng-ver-string+
					 (null-pointer)
					 (callback error-fn)
					 (callback warn-fn)))
           (*buffer* (make-shareable-byte-vector 1024)))
       
       (when (null-pointer-p ,var)
         (error "Failed to allocate PNG write struct."))
       
       (unwind-protect
	    (progn ,@body)
	 
         (with-foreign-pointer (,pointer (foreign-type-size :pointer))
	   (setf (mem-ref ,pointer :pointer) ,var)
	   (png-destroy-read-struct ,pointer (null-pointer) 
						     (null-pointer)))))))

(defmacro with-png-info-struct ((var png-struct) &body body)
  (let ((pointer (gensym "POINTER")))
    `(let ((,var (png-create-info-struct ,png-struct)))
       (when (null-pointer-p ,var)
         (error "Failed to allocate PNG info struct."))
       
       (unwind-protect
	    (progn ,@body)
	 
         (with-foreign-pointer (,pointer (foreign-type-size :pointer))
	   (setf (mem-ref ,pointer :pointer) ,var)
	   (png-destroy-info-struct ,png-struct ,pointer))))))

(defun get-ihdr (png-ptr info-ptr)
  (with-foreign-pointer (width (foreign-type-size :uint32))
    (with-foreign-pointer (height (foreign-type-size :uint32))
      (with-foreign-pointer (bit-depth (foreign-type-size :int))
	(with-foreign-pointer (color-type (foreign-type-size :int))
	  
	  (png-get-ihdr png-ptr
			info-ptr
			width
			height
			bit-depth
			color-type
			(null-pointer)
			(null-pointer)
			(null-pointer))
	  
	  (values (mem-ref width :uint32)
		  (mem-ref height :uint32)
		  (mem-ref bit-depth :int)
		  (mem-ref color-type :int)))))))

(defun bytes-per-pixel (image)
  (ecase (image-bit-depth image)
    (16 2)
    (8 1)))

#|
(defmacro with-row-pointers ((rows-ptr image) &body body)
  
  (let ((row-pointers (gensym "ROW-POINTERS"))
        (raw-data     (gensym "RAW-DATA"))
        (row-pos      (gensym "I"))
        (buffer       (gensym "BUFFER"))
	(pixel-size   (gensym "PIXEL-SIZE")))
    
    
    `(let ((,row-pointers (make-shareable-byte-vector
			   (* (image-height ,image)
			      (foreign-type-size :pointer))))
           (,buffer (array-displacement ,image))
	   (,pixel-size (image-pixel-size ,image)))
       
       (with-pointer-to-vector-data (,rows-ptr ,row-pointers)
	 (with-pointer-to-vector-data (,raw-data ,buffer)
	   
	   (dotimes (,row-pos (image-height ,image))
	     
	     (setf (mem-aref ,rows-ptr :pointer ,row-pos) 
		   (inc-pointer ,raw-data (* ,row-pos
					     (image-width ,image)
					     ,pixel-size))))
	   ,@body)))))
|#

(defmacro with-row-pointers ((rows-ptr image) &body body)
  
  (let ((row-pointers (gensym "ROW-POINTERS"))
        (raw-data     (gensym "RAW-DATA"))
        (row-pos      (gensym "I"))
        (buffer       (gensym "BUFFER"))
	(ptr-inc      (gensym "PTR-INC")))
    
    
    `(let ((,row-pointers (make-shareable-byte-vector (* (image-height ,image)
							 (foreign-type-size :pointer))))
           (,buffer     (image-pixels ,image))
	   (,ptr-inc    (* (image-width ,image)
			   (image-pixel-size ,image))))
       
       (with-pointer-to-vector-data   (,rows-ptr ,row-pointers)
	 (with-pointer-to-vector-data (,raw-data ,buffer)
	   
	   (dotimes (,row-pos (image-height ,image))
	     
	     (setf (mem-aref ,rows-ptr :pointer ,row-pos) 
		   (inc-pointer ,raw-data (* ,row-pos ,ptr-inc))))
	   
	   ,@body)))))

;;(defun grayp (color-type)
;;  (zerop (logand color-type (lognot +png-color-mask-alpha+))))



