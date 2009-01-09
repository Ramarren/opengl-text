(in-package :opengl-text)

(defclass outline-opengl-text (polygonal-opengl-text)
  ((line-width :accessor line-width-of :initarg :line-width :initform 1)))

(defun loop-to-lines (loop)
  (iter (for (x y z) on loop by #'cdddr)
        (for px previous x initially nil)
        (for py previous y initially nil)
        (for pz previous z initially nil)
        (when (and x y z px py pz)
          (nconcing (list px py pz x y z)))))

(defmethod tesselate-character (char (gl-text outline-opengl-text))
  (let ((font (font-loader-of gl-text)))
   (let ((glyph (zpb-ttf:find-glyph char font)))
     (let ((paths (paths-ttf:paths-from-glyph glyph
                                              :scale-x (/ (zpb-ttf:units/em font))
                                              :scale-y (/ (zpb-ttf:units/em font)))))
       (let ((loops (iter (for path in paths)
                          (for iterator = (paths:path-iterator-segmented path))
                          (collecting
                            (iter (for (values nil (x . y) endp) = (paths:path-iterator-next iterator))
                                  (collect (float x 0d0))
                                  (collect (float y 0d0))
                                  (collect 0d0)
                                  (until endp))))))
         (let ((lines (mapcan #'loop-to-lines loops)))
           (make-ffa (length lines) :double :initial-contents lines)))))))

(defmethod draw-gl-string ((string string) (gl-text outline-opengl-text) &key (kerning t) (depth-shift 0.0))
  (ensure-characters (remove-duplicates string) gl-text)
  (gl:disable :texture-2d)
  (let* ((vertices (vertices-of gl-text))
         (font (font-loader-of gl-text))
         (chash (character-hash-of gl-text))
         (scaler (zpb-ttf:units/em font)))
    (with-pointer-to-array (vertices vertex-pointer :double (length vertices) :copy-in)
      (%gl:vertex-pointer 3 :double 0 vertex-pointer)
      (gl:enable :line-smooth)       ;this doesn't seem to work
      (gl:hint :line-smooth-hint :nicest)
      (gl:enable :blend)
      (gl:disable :depth-test)
      (gl:blend-func :src-alpha :one)
      (gl:line-width (line-width-of gl-text))
      (gl:with-pushed-matrix
       ;; so that string begins at 0,0,0
       (gl:translate 0
                     (- (/ (zpb-ttf:descender font) scaler))
                     0)
       (labels ((draw-string ()
                  (gl:with-pushed-matrix
                    (iter
                      (for c in-string string)
                      (for polyglyph = (gethash c chash))
                      (for g = (glyph-of polyglyph))
                      (for gp previous g initially nil)
                      (when (and gp kerning)
                        (gl:translate (/ (zpb-ttf:kerning-offset gp g font) scaler) 0 0))
                      (gl:draw-arrays :lines (start-of polyglyph) (count-of polyglyph))
                      (gl:translate (/ (+ (zpb-ttf:advance-width g)) scaler) 0 depth-shift)))))
         (draw-string))))))
