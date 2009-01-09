(in-package :opengl-text-vector)

(defun create-unscaled-char-path (char font em)
  (let ((scale (/ em (zpb-ttf:units/em font)))
        (glyph (zpb-ttf:find-glyph char font)))
    ;; whitespace has no extent
    (paths-ttf:paths-from-glyph glyph
                                :offset (paths:make-point (* scale (- (zpb-ttf:xmin glyph)))
                                                          (* scale (- (zpb-ttf:ymin glyph))))
                                :scale-x scale
                                :scale-y scale)))

(defun glyph-size (char-path)
  (let ((max-x 0)
        (max-y 0)
        (aa-state (aa:make-state)))
    (aa:cells-sweep (vectors:update-state aa-state char-path)
                    #'(lambda (x y alpha)
                        (declare (ignore alpha))
                        (assert (not (minusp x)))
                        (assert (not (minusp y)))
                        (maxf max-x max-x x)
                        (maxf max-y max-y y)))
    ;; add one for 0-indexing and one for one pixel boundary, to avoid sampling errors
    (list (+ max-x 2) (+ max-y 2))))

(defun draw-unscaled-character (char em font)
  (let* ((char-path (create-unscaled-char-path char font em))
         (aa-state (aa:make-state))
         (out-array (make-array (glyph-size char-path) :initial-element 0)))
    (flet ((draw-function (x y alpha)
             (if (array-in-bounds-p out-array x y)
                 (setf (aref out-array x y) (clamp alpha 0 255))
                 (warn "Out of bounds: ~a ~a" x y))))
      (aa:cells-sweep (vectors:update-state aa-state char-path) #'draw-function)
      out-array)))

(defun compute-actual-slice (char font)
  "Which part of em-square glyph actually covers."
  (let ((glyph (zpb-ttf:find-glyph char font)))
    (let ((base (zpb-ttf:units/em font))
          (bb-glyph (zpb-ttf:bounding-box glyph)))
      (mapcar #'float
              (list (/ (zpb-ttf:xmin bb-glyph) base)
                    (/ (zpb-ttf:ymin bb-glyph) base)
                    (/ (zpb-ttf:xmax bb-glyph) base)
                    (/ (zpb-ttf:ymax bb-glyph) base))))))
