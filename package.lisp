(defpackage #:opengl-text
  (:use #:cl #:iterate #:alexandria #:ffa #:array-operations)
  (:shadowing-import-from #:alexandria #:rotate)
  (:export #:draw-gl-string #:opengl-text #:color-of #:font-loader-of #:ensure-characters
           #:emsquare-of #:*auto-extend-buffers*))
