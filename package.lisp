(defpackage #:opengl-text
  (:use #:cl #:iterate #:alexandria #:ffa)
  (:shadowing-import-from #:alexandria #:rotate)
  (:export #:draw-gl-string #:opengl-text #:color-of #:font-loader-of #:ensure-characters
           #:emsquare-of #:*auto-extend-buffers*))
