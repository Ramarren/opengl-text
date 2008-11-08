(in-package :opengl-text)

;;; pack glyphs as rectangles to avoid scaling issues

(defclass packed-mipmap-opengl-text (base-opengl-text)
  ((celled-textures :accessor celled-textures-of
                    :initarg :celled-textures
                    :initform nil
                    :documentation "Cell-textures, first primary, then mipmaps from largest to smallest")))

(defun create-unscaled-char-path (char font em)
  (let ((scale (/ em (zpb-ttf:units/em font)))
        (glyph (zpb-ttf:find-glyph char font)))
    ;; whitespace has no extent
    (paths-ttf:paths-from-glyph glyph
                                :offset (paths:make-point (* scale (- (zpb-ttf:xmin glyph)))
                                                          (* scale (- (zpb-ttf:ymin glyph))))
                                :scale-x scale
                                :scale-y scale)))

(defun glyph-size (char font em)
  (let ((scale (/ em (zpb-ttf:units/em font)))
        (glyph (zpb-ttf:find-glyph char font)))
    (list (ceiling (max 1 (1+ (* scale (- (zpb-ttf:xmax glyph) (zpb-ttf:xmin glyph))))))
          (ceiling (max 1 (1+ (* scale (- (zpb-ttf:ymax glyph) (zpb-ttf:ymin glyph)))))))))

(defun draw-unscaled-character (char em font)
  (let* ((char-path (create-unscaled-char-path char font em))
         (aa-state (aa:make-state))
         (out-array (make-array (glyph-size char-path) :initial-element 0)))
    (flet ((draw-function (x y alpha)
             (if (array-in-bounds-p out-array y x)
                 (setf (aref out-array y x) (clamp alpha 0 255))
                 (warn "Out of bounds: ~a ~a" (- em y) x))))
      (aa:cells-sweep (vectors:update-state aa-state char-path) #'draw-function)
      out-array)))

(defmethod draw-char (char (gl-text packed-mipmap-opengl-text))
  (iter (for em initially (emsquare-of gl-text) then (ash em -1))
        (with font = (font-loader-of gl-text))
        (while (plusp em))
        (collect (draw-unscaled-character char em font))))