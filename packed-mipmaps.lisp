(in-package :opengl-text)

;;; pack glyphs as rectangles to avoid scaling issues

(defclass packed-mipmap-opengl-text (base-opengl-text)
  ((celled-texture :accessor celled-texture-of
                   :initarg :celled-texture
                   :initform (make-instance 'simple-cell-texture)
                   :documentation "Cell-texture. Generate mipmaps automatically for now, since I am not certain how to maintain scaling without forcing all rectangles to have power of two sizes.")))

(defmethod (setf emsquare-of) :after (new-value (object mipmap-opengl-text))
  (flush-texture object :new-texture-array t))

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

(defmethod draw-char (char (gl-text packed-mipmap-opengl-text))
  (draw-unscaled-character char (emsquare-of gl-text) (font-loader-of gl-text)))

(defmethod add-char ((char character) (gl-text packed-mipmap-opengl-text) &optional (send-texture t))
  (let ((new-cell (pack-rectangle (celled-texture-of gl-text) (draw-char char gl-text))))
    (setf (gethash char (character-hash-of gl-text))
          (make-instance 'glyph
                         :tex-coord (transform-cell-coords new-cell (celled-texture-of gl-text))
                         :cell new-cell
                         :actual-slice (compute-actual-slice char (font-loader-of gl-text))))
    (when send-texture
      (send-texture gl-text))
    (gethash char (character-hash-of gl-text))))

(defmethod send-texture ((gl-text packed-mipmap-opengl-text))
  (when *opengl-active*
    (if (texture-number-of gl-text)
        (cl-opengl:bind-texture :texture-2d (texture-number-of gl-text))
        (let ((new-number (car (cl-opengl:gen-textures 1))))
          (setf (texture-number-of gl-text) new-number)
          (cl-opengl:bind-texture :texture-2d new-number)
          (trivial-garbage:finalize gl-text #'(lambda ()
                                                (gl:delete-textures (list new-number))))))
    (let ((texture (texture-array-of (celled-texture-of gl-text))))
      (with-pointer-to-array (texture
                              tex-pointer
                              :uint8
                              (reduce #'* (array-dimensions texture))
                              :copy-in)
        (glu:build-2d-mipmaps :texture-2d :intensity (array-dimension texture 1)
                              (array-dimension texture 0) :luminance :unsigned-byte tex-pointer)))))

(defmethod ensure-characters ((characters sequence) (gl-text packed-mipmap-opengl-text))
  (let ((chars-loaded (hash-table-keys (character-hash-of gl-text)))
        (texture (celled-texture-of gl-text)))
    (let ((more-chars (set-difference (coerce characters 'list) chars-loaded)))
      (when more-chars
        (when chars-loaded (assert texture))
        (map nil (rcurry #'add-char gl-text nil) more-chars)
        (send-texture gl-text)))))

(defmethod :before ((string string) (gl-text packed-mipmap-opengl-text) &key (kerning t) (depth-shift 0.0))
  (declare (ignore kerning string depth-shift))
  (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
  (gl:tex-parameter :texture-2d :texture-mag-filter :linear-mipmap-linear))

(defmethod flush-texture ((gl-text packed-mipmap-opengl-text) &key (new-texture-array nil))
    ;; not the most efficient method
    (let ((chars (hash-table-keys (character-hash-of gl-text)))
          (old-cell-tex (celled-texture-of gl-text)))
      (when chars
        (setf (character-hash-of gl-text) (make-hash-table))
        (setf (celled-texture-of gl-text)
              (make-instance 'simple-cell-texture
                             :size (size-of old-cell-tex)
                             :texture-array (if new-texture-array
                                                (make-ffa (list (texture-width-of old-cell-tex)
                                                                (texture-height-of old-cell-tex))
                                                          (kind-of old-cell-tex))
                                                (texture-array-of old-cell-tex))
                             :texture-width (texture-width-of old-cell-tex)
                             :texture-height (texture-height-of old-cell-tex)
                             :kind (kind-of old-cell-tex)))
        (ensure-characters chars gl-text))))