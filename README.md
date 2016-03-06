
#XPNG

Load PNG files in Common Lisp. 

## Acknowledgements

This is a somewhat heavy handed re-write of CL-PNG
(http://www.ljosa.com/~ljosa/software/cl-png/) by
Harald Musum and Vebjorn Ljosa. 

## Installation

To install

    git clone https://github.com/ivankocienski/xpng.git ~/quicklisp/local-projects

    ;; in lisp
    (ql:register-local-projects)


## Requirements

- CFFI / Grovel
- OpenGL (for OpenGL part only)
- SBCL

## Features

- Update for libpng 1.16.
- Handles transparency.
- Handles 8 bit indexed color schemes.
- Handles greyscale.
- Utility functions for uploading to GL

## Limitations

- Read only capable
- Downconverts 16 bit images to 8 bits
- No support for tRNS (transparency on indexed images)
- Only tested on SBCL on Gentoo Linux :(

## Base API

`(ql:quickload :xpng)`

The main structure is `image` which defines the following
fields

    type
    width
    height
    pixels
    palette

`type` (keyword) one of :indexed, :grey, :grey-alpha, :rgb, :rgba

`width` and `height` (integer) size of image in pixels

`palette` (array of `color` structs) colors for indexed images

`pixels` (array of bytes) 'raw' image data


    (decode stream)

Given an open stream pointing to a valid PNG data stream will return
an `image`. Will raise an `error` condition if something went wrong.

    (decode-file file-path)

Helper wrapper so you only pass in a file name.

## GL API

`(ql:quickload :xpng-gl)`

    (load-file-to-texture filename format)

Given a GL texture has been created and bound, load an image from disk
and send it to GL returning the image. Images are converted for you to
the correct GL format.

`filename` (string) path to file

`format` (keyword) optional flag for format. Must be either `:RGB` or
`:RGBA`. Defaults to `:RGBA`

## Demo 

See `xpng/demo/` for example code of how to use the API in a GLFW
application. Requires GLFW3.

## Copyright

By Ivan Kocienski. 2016. Released under the GPL as per CL-PNGs license.