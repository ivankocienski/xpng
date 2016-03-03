(in-package :xpng)

(define-foreign-library libpng
                        ;;  (:unix (:or "libpng12.0.dylib" "libpng12.dylib" "libpng12.so.0"))
                        (t (:default "libpng16")))

(use-foreign-library libpng)

(defconstant +png-libpng-ver-string+ (symbol-name '|1.6.16|))

;;; Foreign function definitions.

(defcfun "png_access_version_number" :uint32)

(defcfun "png_create_read_struct" :pointer
         (user-png-ver :string)
         (error-ptr :pointer)
         (error-fn :pointer)
         (warn-fn :pointer))

(defcfun "png_destroy_read_struct" :void
         (png-ptr-ptr :pointer)
         (info-ptr-ptr :pointer)
         (end-info-ptr-ptr :pointer))

(defcfun "png_create_write_struct" :pointer
         (user-png-ver :string)
         (error-ptr :pointer)
         (error-fn :pointer)
         (warn-fn :pointer))

(defcfun "png_destroy_write_struct" :void
  (png-ptr-ptr :pointer)
  (info-ptr-ptr :pointer))

(defcfun "png_create_info_struct" :pointer
         (png-ptr :pointer))

(defcfun "png_destroy_info_struct" :void
         (png-ptr :pointer)
         (info-ptr-ptr :pointer))

(defcfun "png_init_io" :void
         (png-ptr :pointer)
         (file :pointer))

(defcfun "png_set_read_fn" :void
         (png-ptr :pointer)
         (io-ptr :pointer)
         (read-data-fn :pointer))

(defcfun "png_set_write_fn" :void
         (png-ptr :pointer)
         (io-ptr :pointer)
         (write-data-fn :pointer)
         (output-flush-fn :pointer))

(defcfun "png_get_io_ptr" :pointer
         (png-ptr :pointer))

(defcfun "png_read_info" :void
         (png-ptr :pointer)
         (info-ptr :pointer))

(defcfun "png_read_png" :void
         (png-ptr :pointer)
         (info-ptr :pointer)
         (png-transforms :int)
         (params :pointer))

(defcfun "png_get_IHDR" :uint32
         (png-ptr :pointer)
         (info-ptr :pointer)
         (width-uint32-ptr :pointer)
         (height-uint32-ptr :pointer)
         (bit-depth-int-ptr :pointer)
         (color-type-int-ptr :pointer)
         (interlace-type-int-ptr :pointer)
         (compression-type-int-ptr :pointer)
         (filter-type-int-ptr :pointer))

(defcfun "png_set_IHDR" :void
         (png-ptr :pointer)
         (info-ptr :pointer)
         (width :uint32)
         (height :uint32)
         (bit-depth :int)
         (color-type :int)
         (interlace-type :int)
         (compression-type :int)
         (filter-type :int))

(defcfun "png_get_PLTE" :int32
  (png-ptr :pointer)
  (png-info :pointer)
  (palette :pointer)
  (num-colors :pointer))

(defcfun "png_set_palette_to_rgb" :void
         (png-ptr :pointer))

(defcfun "png_set_expand_gray_1_2_4_to_8" :void
         (png-ptr :pointer))

(defcfun "png_set_expand" :void
         (png-ptr :pointer))

(defcfun "png_get_valid" :uint32
         (png-ptr :pointer)
         (info-ptr :pointer)
         (flag :uint32))

(defcfun "png_set_tRNS_to_alpha" :void
         (png-ptr :pointer))

(defcfun "png_set_strip_16" :void
         (png-ptr :pointer))

(defcfun "png_set_packing" :void
         (png-ptr :pointer))

(defcfun "png_set_strip_alpha" :void
         (png-ptr :pointer))

(defcfun "png_set_swap" :void
         (png-ptr :pointer))

(defcfun "png_get_rows" :pointer
         (png-ptr :pointer)
         (info-ptr :pointer))

(defcfun "png_set_rows" :void
         (png-ptr :pointer)
         (info-ptr :pointer)
         (row-pointers :pointer))

(defcfun "png_read_image" :void
         (png-ptr :pointer)
         (row-pointers :pointer))

(defcfun "png_read_end" :void
         (png-ptr :pointer)
         (info-ptr :pointer))
  
(defcfun "png_write_png" :void
         (png-ptr :pointer)
         (info-ptr :pointer)
         (transforms :int)
         (params :pointer))

(defcfun "memcpy" :pointer
         (dest :pointer)
         (source :pointer)
         (n :uint))


(defvar *stream*)

(defvar *buffer*)

(defun ensure-buffer-sufficient (needed)
  (when (< (length *buffer*) needed)
    (let ((new-length (length *buffer*)))
      (loop while (< new-length needed)
	 do (setf new-length (* 2 new-length)))
      (setf *buffer* (make-shareable-byte-vector new-length)))))

(defcallback user-read-data :void ((png-ptr :pointer) (data :pointer)
				   (length png-size))
  
             (declare (ignore png-ptr))
             (ensure-buffer-sufficient length)

	     (let ((bytes-read (read-sequence *buffer* *stream* :start 0 :end length)))
               (unless (= bytes-read length)
                 (error "Expected to read ~D bytes, but only read ~D." length 
                        bytes-read)))
	     
             (with-pointer-to-vector-data (buffer-ptr *buffer*)
	       (memcpy data buffer-ptr length)))


(defcallback user-flush-data :void ((png-ptr :pointer))
             (declare (ignore png-ptr)))


;;; Error handling.

(defcallback error-fn :void ((png-structp :pointer) (message :string))
             (declare (ignore png-structp))
             (error message))

(defcallback warn-fn :void ((png-structp :pointer) (message :string))
             (declare (ignore png-structp))
             (error message))
 
