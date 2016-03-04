(in-package #:xpng)

(defstruct color
  (red   0)
  (green 0)
  (blue  0)
  (alpha 0))

(defstruct image
  type      ; identifies type for user
  width     ; pixels
  height    ; pixels
  depth     ; 8 or 16 bit
  palette   ; ((R G B) (R G B) ...)
  pixels)

(defun image-pixel-size (image)
  "Number of bytes in a pixel for a given image type"
  (let ((it (image-type image)))

    (cond
      ((= it +png-color-type-gray+)       1)   ; grey
      ((= it +png-color-type-palette+)    1)   ; indexed
      ((= it +png-color-type-gray-alpha+) 2)   ; grey + alpha
      ((= it +png-color-type-rgb+)        3)   ; truecolor
      ((= it +png-color-type-rgba+)       4))) ; truecolor + alpha  

;;  ((= it 0) 1)   ; grey
;;      ((= it 2) 3)   ; truecolor
;;      ((= it 3) 1)   ; indexed
;;      ((= it 4) 2)   ; grey + alpha
;;      ((= it 6) 4))) ; truecolor + alpha  
  )


