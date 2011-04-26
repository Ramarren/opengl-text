(in-package :opengl-text)

(defmethod flush-texture ((gl-text textured-opengl-text) &key (new-texture-array nil))
    ;; not the most efficient method
    (let ((chars (hash-table-keys (character-hash-of gl-text)))
          (old-cell-tex (texture-of gl-text)))
      (when chars
        (setf (character-hash-of gl-text) (make-hash-table))
        (setf (texture-of gl-text)
              (make-instance 'simple-cell-texture
                             :size (size-of old-cell-tex)
                             :texture-array (if new-texture-array
                                                (make-array (list (texture-width-of old-cell-tex)
                                                                  (texture-height-of old-cell-tex))
                                                            :element-type (kind-of old-cell-tex))
                                                (texture-array-of old-cell-tex))
                             :texture-width (texture-width-of old-cell-tex)
                             :texture-height (texture-height-of old-cell-tex)
                             :type (kind-of old-cell-tex)))
        (ensure-characters chars gl-text))))

(defmethod send-texture ((gl-text textured-opengl-text))
  (when *opengl-active*
    (unless (texture-number-of gl-text)
      (let ((new-number (car (cl-opengl:gen-textures 1))))
        (setf (texture-number-of gl-text) new-number)
        (trivial-garbage:finalize gl-text #'(lambda ()
                                              (gl:delete-textures (list new-number))))))
    (cl-opengl:bind-texture :texture-2d (texture-number-of gl-text))
    (let ((new-texture (texture-array-of (texture-of gl-text))))
      (with-pointer-to-array (new-texture tex-pointer
                                          :uint8
                                          (array-total-size new-texture)
                                          :copy-in)
        (cl-opengl:tex-image-2d :texture-2d 0 :intensity (array-dimension new-texture 1)
                                (array-dimension new-texture 0) 0 :luminance :unsigned-byte tex-pointer)))))

(defmethod add-char ((char character) (gl-text textured-opengl-text) &optional (send-texture t))
  (multiple-value-bind (glyph-array actual-slice) (draw-char char gl-text)
    (let ((new-cell (pack-rectangle (texture-of gl-text) glyph-array)))
      (setf (gethash char (character-hash-of gl-text))
            (make-instance 'glyph
                           :tex-coord (transform-cell-coords new-cell (texture-of gl-text))
                           :cell new-cell
                           :actual-slice actual-slice))
      (iter (for (char glyph) in-hashtable (character-hash-of gl-text))
            (with cell-tex = (texture-of gl-text))
            (setf (tex-coord-of glyph)
                  (transform-cell-coords (cell-of glyph) cell-tex)))
      (when send-texture
        (send-texture gl-text))
      (gethash char (character-hash-of gl-text)))))

