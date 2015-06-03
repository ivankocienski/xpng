
(in-package :xpng)

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

	(let ((*stream* input) (has-alpha nil))

	  (png-set-read-fn png-ptr (null-pointer) (callback user-read-data))

	  (png-read-info png-ptr info-ptr)

	  (multiple-value-bind (width height bit-depth color-type) (get-ihdr png-ptr info-ptr) 

      (cond
        ((= color-type +png-color-type-palette+) 
         (progn
           (png-set-palette-to-rgb png-ptr)
           (if (png-get-valid png-ptr info-ptr +png-info-trns+)
             (progn
               (setf has-alpha T)
               (png_set_tRNS_to_alpha png_ptr))
           )))

        ((= color-type +png-color-type-gray+) 
         (progn
           (png-set-expand-gray-1-2-4-to-8 png-ptr)
           (if (png-get-valid png-ptr info-ptr +png-info-trns+)
             (progn
               (setf has-alpha T)
               (png_set_tRNS_to_alpha png_ptr)))))

        ((= color-type +png-color-type-gray-alpha+)
         (progn
           (setf has-alpha T)
           (png-set-palette-to-rgb png-ptr)))

;        ((= color-type +png-color-type-rgb+)
;         (setf read-channels 4))
        
        ((= color-type +png-color-type-rgba+)
         (progn
           (setf has-alpha T)
           (png-set-expand png-ptr)))
        )

	    #+little-endian
	    (if (= bit-depth 16) 
	      (png-set-swap png-ptr))

      (if has-alpha 
        (incf channel-count))

	    (let ((image (make-image height width read-channels read-depth)))
	      (with-row-pointers (row-pointers image)
                           
                           (png-set-rows png-ptr info-ptr row-pointers)
                           (png-read-image png-ptr row-pointers))


	      image)))))))

(defun decode-file (pathname)
  (with-open-file (input pathname :element-type '(unsigned-byte 8))
    (decode input)))
