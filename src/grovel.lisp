(flag "-I/opt/local/include")
(include "png.h")

(in-package #:xpng)

(constant (+png-color-mask-palette+ "PNG_COLOR_MASK_PALETTE"))
(constant (+png-color-mask-color+   "PNG_COLOR_MASK_COLOR"))
(constant (+png-color-mask-alpha+   "PNG_COLOR_MASK_ALPHA"))

(constant (+png-color-type-palette+    "PNG_COLOR_TYPE_PALETTE"))
(constant (+png-color-type-gray+       "PNG_COLOR_TYPE_GRAY"))
(constant (+png-color-type-gray-alpha+ "PNG_COLOR_TYPE_GRAY_ALPHA"))
(constant (+png-color-type-rgb+        "PNG_COLOR_TYPE_RGB"))
(constant (+png-color-type-rgba+       "PNG_COLOR_TYPE_RGB_ALPHA"))

(constant (+png-transform-strip-16+    "PNG_TRANSFORM_STRIP_16"))
(constant (+png-transform-strip-alpha+ "PNG_TRANSFORM_STRIP_ALPHA"))
(constant (+png-transform-swap-endian+ "PNG_TRANSFORM_SWAP_ENDIAN"))
(constant (+png-transform-identity+    "PNG_TRANSFORM_IDENTITY"))

(constant (+png-compression-type-base+    "PNG_COMPRESSION_TYPE_BASE"))
(constant (+png-compression-type-default+ "PNG_COMPRESSION_TYPE_DEFAULT"))

(constant (+png-filter-type-base+    "PNG_FILTER_TYPE_BASE"))
(constant (+png-filter-type-default+ "PNG_FILTER_TYPE_DEFAULT"))

;; misc

(constant (+png-interlace-none+ "PNG_INTERLACE_NONE"))
(constant (+png-info-trns+ "PNG_INFO_tRNS"))

(ctype ssize "ssize_t")
(ctype size "size_t")
(ctype png-size "png_size_t")
