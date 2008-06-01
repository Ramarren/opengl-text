(defpackage #:opengl-text
  (:use #:cl #:iterate #:alexandria #:ffa #:affi)
  (:shadowing-import-from #:alexandria #:rotate)
  (:export #:draw-gl-string #:opengl-text #:color-of #:font-loader-of #:ensure-characters
	   #:emsquare-of))

