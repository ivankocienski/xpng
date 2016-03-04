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
  ;;depth     ; 8 or 16 bit
  palette   ; [(RGBA) (RGBA) ...]
  pixels)

(defun image-pixel-size (image)
  "Number of bytes in a pixel for a given image type"
  (let ((it (image-type image)))

    (cond
      ((eq it :grey)       1) 
      ((eq it :indexed)    1)  
      ((eq it :grey-alpha) 2) 
      ((eq it :rgb)        3)  
      ((eq it :rgba)       4))))


