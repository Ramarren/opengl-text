(in-package :opengl-text)

;; each character in character-hash refers to a glyph object
(defclass glyph ()
  ((tex-coord :initarg :tex-coord :accessor tex-coord-of)
   (cell :initarg :cell :accessor cell-of)
   (actual-slice :initarg :actual-slice :accessor actual-slice-of)))

(defun create-char-path (char font em)
  (let ((bb-glyph (zpb-ttf:bounding-box (zpb-ttf:find-glyph char font)))
        (em (1- em)))
    (let ((scale-x (- (zpb-ttf:xmax bb-glyph) (zpb-ttf:xmin bb-glyph)))
          (scale-y (- (zpb-ttf:ymax bb-glyph) (zpb-ttf:ymin bb-glyph))))
      ;; whitespace has no extent
      (when (zerop scale-x) (setf scale-x 1))
      (when (zerop scale-y) (setf scale-y 1))
      (paths-ttf:paths-from-glyph (zpb-ttf:find-glyph char font)
                                  :offset (paths:make-point (* em
                                                               (- (/ (zpb-ttf:xmin bb-glyph) scale-x)))
                                                            (+ (1+ em)
                                                               (* em
                                                                  (/ (zpb-ttf:ymin bb-glyph) scale-y))))
                                  :scale-x (/ em scale-x)
                                  :scale-y (- (/ em scale-y))))))

(defun draw-character (char em font)
  (let ((char-path (create-char-path char font em))
        (aa-state (aa:make-state))
        (out-array (make-array (list em em) :initial-element 0)))
    (flet ((draw-function (x y alpha)
             (if (array-in-bounds-p out-array (- em y) x)
                 (setf (aref out-array (- em y) x) (clamp alpha 0 255))
                 (warn "Out of bounds: ~a ~a" (- em y) x))))
      (aa:cells-sweep (vectors:update-state aa-state char-path) #'draw-function)
      out-array)))

(defgeneric draw-char (char gl-text)
  (:method (char (gl-text opengl-text))
    (draw-character char (emsquare-of gl-text) (font-loader-of gl-text))))

(defun compute-actual-slice (char font)
  (let ((glyph (zpb-ttf:find-glyph char font))
        (bb (zpb-ttf:bounding-box font)))
    (let ((base (max (- (zpb-ttf:xmax bb)
                        (zpb-ttf:xmin bb))
                     (- (zpb-ttf:ymax bb)
                        (zpb-ttf:ymin bb))))
          (x-edge (zpb-ttf:xmin bb))
          (y-edge (zpb-ttf:ymin bb))
          (bb-glyph (zpb-ttf:bounding-box glyph)))
      (mapcar #'float
              (list (/ (- (zpb-ttf:xmin bb-glyph) x-edge) base)
                    (/ (- (zpb-ttf:ymin bb-glyph) y-edge) base)
                    (/ (- (zpb-ttf:xmax bb-glyph) x-edge) base)
                    (/ (- (zpb-ttf:ymax bb-glyph) y-edge) base))))))

(defun transform-cell (cell array em)
  "Transform cell to relative coordinates (OpenGL TexCoords)."
  (destructuring-bind ((ymin ymax) (xmin xmax)) (cell-range cell em array)
    (destructuring-bind (h w) (array-dimensions array)
      (let ((xmax (1+ xmax))
            (ymax (1+ ymax)))
       (make-array '(4 2)
                   :element-type 'single-float
                   :initial-contents
                   (mapcar (curry #'mapcar #'float)
                           (list (list (/ xmin w) (/ ymin h))
                                 (list (/ xmax w) (/ ymin h))
                                 (list (/ xmax w) (/ ymax h))
                                 (list (/ xmin w) (/ ymax h)))))))))

(defun chars-recoordinate (old-chars array em)
  (iter (for (old-char glyph) in-hashtable old-chars)
        (setf (tex-coord-of glyph) (transform-cell (cell-of glyph) array em))))

(defun add-new-character (new-char gl-text cell array em)
  (setf (gethash new-char (character-hash-of gl-text))
        (make-instance 'glyph
                       :tex-coord (transform-cell cell array em)
                       :cell cell
                       :actual-slice (compute-actual-slice
                                      new-char (font-loader-of gl-text)))))

(defgeneric add-char (char gl-text &optional send-texture)
  (:method ((char character) (gl-text opengl-text) &optional (send-texture t))
    (let ((em (emsquare-of gl-text)))
      (let ((new-count (1+ (hash-table-count (character-hash-of gl-text)))))
        (size-texture gl-text new-count :preserve t)
        (let ((new-texture (texture-of gl-text)))
          (copy-character (draw-char char gl-text) 0 new-texture (1- new-count) em)
          (add-new-character char gl-text (1- new-count) new-texture em)
          (setf (texture-of gl-text) new-texture)
          (when send-texture
            (send-texture gl-text))
          (gethash char (character-hash-of gl-text)))))))

(defgeneric ensure-characters (characters gl-text)
  (:method ((characters sequence) (gl-text opengl-text))
    (let ((chars-loaded (hash-table-keys (character-hash-of gl-text)))
          (texture (texture-of gl-text)))
      (let ((more-chars (set-difference (coerce characters 'list) chars-loaded)))
        (when more-chars
          (let ((chars (append chars-loaded more-chars)))
            (when chars-loaded (assert texture))
            (size-texture gl-text (length chars) :preserve t)
            (map nil (rcurry #'add-char gl-text nil) more-chars)
            (send-texture gl-text)))))))
