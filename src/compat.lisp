(in-package #:xpng)

#-(or lispworks allegro)
(defun make-shareable-byte-vector (size &optional (byte-size 8))
   (make-array size :element-type (list 'unsigned-byte byte-size)))

(defmacro with-pointer-to-vector-data ((ptr-var vector) &body body)
  "Bind PTR-VAR to a foreign pointer to the data in VECTOR."
  (let ((vector-var     (gensym))
	(nbytes         (gensym)))
    
    `(let* ((,vector-var ,vector))
              
       (with-foreign-pointer (,ptr-var (length ,vector-var) ,nbytes)
         ;; copy-in
         (loop
	    for word from 0 
	    and byte below ,nbytes 
	    do (cffi-sys:%mem-set (aref ,vector-var word) ,ptr-var :unsigned-char byte))
	 
         (unwind-protect (progn ,@body)
           ;; copy-out
           (loop 
	      for word from 0
	      and byte below ,nbytes
	      do (setf (aref ,vector-var word)
		       (cffi-sys:%mem-ref ,ptr-var :unsigned-char byte))))))))
