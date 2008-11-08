(defpackage #:opengl-text
  (:use #:cl #:iterate #:alexandria #:ffa #:array-operations)
  (:shadowing-import-from #:alexandria #:rotate)
  (:export #:draw-gl-string #:opengl-text #:mipmap-opengl-text #:font-loader-of #:ensure-characters
           #:emsquare-of #:*auto-extend-buffers*
           #:polygonal-opengl-text
           #:bezier-distance-tolerance-of
           #:bezier-angle-tolerance-of
           #:line-width-of))
