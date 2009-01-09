(in-package :opengl-text-vector)

(defun get-font-loader (font-designator)
  (etypecase font-designator
    ((or string pathname) (zpb-ttf:open-font-loader font-designator))
    (zpb-ttf::font-loader font-designator)))

(defun make-draw-glyph-function (font)
  #'(lambda (char em)
      (values (draw-unscaled-character char em font)
              (compute-actual-slice char font))))

(defun make-kerning-function (font scaler)
  #'(lambda (c1 c2)
      (when (and c1 c2)
        (/ (zpb-ttf:kerning-offset (zpb-ttf:find-glyph c1 font) (zpb-ttf:find-glyph c2 font) font)
           scaler))))

(defun make-advance-function (font scaler)
  #'(lambda (c)
      (/ (zpb-ttf:advance-width (zpb-ttf:find-glyph c font))
         scaler)))

(defun make-placement-function (font scaler)
  #'(lambda (c)
      (/ (- (zpb-ttf:xmin (zpb-ttf:bounding-box font))
            (zpb-ttf:xmin (zpb-ttf:bounding-box (zpb-ttf:find-glyph c font))))
         scaler)
      (/ (zpb-ttf:descender font) scaler)))

(defun make-vector-gl-text (font &key (mipmapped nil) (internal-size 32) (length 20))
  (let ((font (get-font-loader font)))
    (let ((scaler (zpb-ttf:units/em font)))
      (make-instance (if (not mipmapped)
                         'textured-opengl-text
                         'mipmap-opengl-text)
                     :emsquare internal-size
                     :length length
                     :draw-glyph-function (make-draw-glyph-function font)
                     :kerning-function (make-kerning-function font scaler)
                     :advance-function (make-advance-function font scaler)
                     :placement-function (make-placement-function font scaler)))))
