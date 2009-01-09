(in-package :opengl-text)

;;; pack glyphs as rectangles to avoid scaling issues

(defclass packed-mipmap-opengl-text (base-opengl-text)
  ((celled-texture :accessor celled-texture-of
                   :initarg :celled-texture
                   :initform (make-instance 'simple-cell-texture)
                   :documentation "Cell-texture. Generate mipmaps automatically for now, since I am not certain how to maintain scaling without forcing all rectangles to have power of two sizes.")))

(defmethod (setf emsquare-of) :after (new-value (object mipmap-opengl-text))
  (flush-texture object :new-texture-array t))

(defmethod draw-char (char (gl-text packed-mipmap-opengl-text))
  (draw-unscaled-character char (emsquare-of gl-text) (font-loader-of gl-text)))

(defmethod add-char ((char character) (gl-text packed-mipmap-opengl-text) &optional (send-texture t))
  (let ((new-cell (pack-rectangle (celled-texture-of gl-text) (draw-char char gl-text))))
    (setf (gethash char (character-hash-of gl-text))
          (make-instance 'glyph
                         :tex-coord (transform-cell-coords new-cell (celled-texture-of gl-text))
                         :cell new-cell
                         :actual-slice (compute-actual-slice char (font-loader-of gl-text))))
    (iter (for (char glyph) in-hashtable (character-hash-of gl-text))
          (with cell-tex = (celled-texture-of gl-text))
          (setf (tex-coord-of glyph)
                (transform-cell-coords (cell-of glyph) cell-tex)))
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