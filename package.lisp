(defpackage #:opengl-text
  (:use #:cl #:iterate #:alexandria #:ffa #:array-operations)
  (:shadowing-import-from #:alexandria #:rotate)
  (:export #:draw-gl-string  #:ensure-characters
           #:textured-opengl-text #:mipmap-opengl-text
           #:emsquare-of #:*auto-extend-buffers*))

(in-package :opengl-text)

(defvar *opengl-active* t "Disable opengl operations for testing.")