
(in-package :xpng)

(defun encode (image output)
  "Writes IMAGE in PNG format to OUTPUT.  The current version always
   writes an 8-bit PNG file if image is an 8-BIT-IMAGE and a 16-bit PNG
   file if image is an 16-BIT-IMAGE.  Future versions may write PNG files
   of lower bit depths than IMAGE when the least significant bits may be
   trimmed without loss of precision.

   Signals an error if writing the image fails."
  (check-type image (or grayscale-image rgb-image))
  (with-png-struct (png-ptr :direction :output)
    (with-png-info-struct (info-ptr png-ptr (png-create-info-struct png-ptr))
      (let ((*stream* output))
	(png-set-write-fn png-ptr (null-pointer) (callback user-write-data)
			  (callback user-flush-data))
	(png-set-ihdr png-ptr info-ptr (image-width image) (image-height image)
		      (image-bit-depth image) 
		      (if (= (image-channels image) 1)
			  +png-color-type-gray+
			  +png-color-type-rgb+)
		      +png-interlace-none+ +png-compression-type-default+
		      +png-filter-type-default+)
	(with-row-pointers (row-pointers image)
	  (png-set-rows png-ptr info-ptr row-pointers)
	  (png-write-png png-ptr info-ptr 
			 #+little-endian +png-transform-swap-endian+ 
			 #-little-endian +png-transform-identity+
			 (null-pointer))))))
  t)

(defun encode-file (image pathname)
  (with-open-file (output pathname :element-type '(unsigned-byte 8)
                          :direction :output :if-exists :supersede)
    (encode image output)))
