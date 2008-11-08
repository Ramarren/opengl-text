(in-package :opengl-text)

(defclass mipmap-opengl-text (base-opengl-text)
  ((textures :accessor textures-of
             :initarg :textures
             :initform nil
             :documentation "Textures first primary, the mipmaps from largest to smallest")))


(defmethod initialize-instance :after ((instance mipmap-opengl-text) &rest initargs)
  (declare (ignore initargs))
  (setf (emsquare-of instance)
        (ceiling-power-of-two (max 1 (emsquare-of instance)))))

(defmethod (setf emsquare-of) :after (new-value (object mipmap-opengl-text))
  (setf (slot-value object 'emsquare)
        (ceiling-power-of-two (max 1 (emsquare-of object))))
  (flush-texture object :new-texture-array t))

(defmethod draw-char (char (gl-text mipmap-opengl-text))
  (iter (for em initially (emsquare-of gl-text) then (ash em -1))
        (with font = (font-loader-of gl-text))
        (while (plusp em))
        (collect (draw-character char em font))))

(defmethod size-texture ((gl-text mipmap-opengl-text) (len integer) &key (preserve t))
  (let ((*coerce-em-to-power-of-two* t)
        (primary-em (emsquare-of gl-text))
        (old-texs (textures-of gl-text))
        (old-primary-tex (car (textures-of gl-text))))
    (unless (and preserve
                 old-primary-tex
                 (<= len (character-cells primary-em old-primary-tex)))
      (let ((new-textures (iter (for em initially primary-em then (ash em -1))
                                (while (plusp em))
                                (collect (make-new-texture-array em len)))))
        (setf (textures-of gl-text) new-textures)
        (when (and preserve old-texs)
          (iter (for (nil glyph) in-hashtable (character-hash-of gl-text))
                (iter (for old-tex in old-texs)
                      (for new-texture in new-textures)
                      (for em initially primary-em then (ash em -1))
                      (copy-character old-tex (cell-of glyph) new-texture (cell-of glyph) em)))
          (chars-recoordinate (character-hash-of gl-text) (car new-textures) primary-em))))))

(defmethod add-char ((char character) (gl-text mipmap-opengl-text) &optional (send-texture t))
  (let ((new-count (1+ (hash-table-count (character-hash-of gl-text)))))
    (size-texture gl-text new-count :preserve t)
    (add-new-character char gl-text (1- new-count) (car (textures-of gl-text)) (emsquare-of gl-text))
    (iter (for new-texture in (textures-of gl-text))
          (for em initially (emsquare-of gl-text) then (ash em -1))
          (for char-array in (draw-char char gl-text))
          (copy-character char-array 0 new-texture (1- new-count) em))
    (when send-texture
      (send-texture gl-text))
    (gethash char (character-hash-of gl-text))))

(defmethod send-texture ((gl-text mipmap-opengl-text))
  (when *opengl-active*
    (if (texture-number-of gl-text)
        (cl-opengl:bind-texture :texture-2d (texture-number-of gl-text))
        (let ((new-number (car (cl-opengl:gen-textures 1))))
          (setf (texture-number-of gl-text) new-number)
          (cl-opengl:bind-texture :texture-2d new-number)
          (trivial-garbage:finalize gl-text #'(lambda ()
                                                (gl:delete-textures (list new-number))))))
    ;; first generate mipmaps automatically, this is because OpenGL requires mipmaps for all levels,
    ;; and I don't want to generate mipmaps for those levels where emsquare is smaller than one
    ;; pixel, since at that point it doesn't really matter what is in the texture, it could be
    ;; filled with 0 or #xff, but I don't want to worry about exact details
    (let ((texture (car (textures-of gl-text))))
      (with-pointer-to-array (texture
                              tex-pointer
                              :uint8
                              (reduce #'* (array-dimensions texture))
                              :copy-in)
        (glu:build-2d-mipmaps :texture-2d :intensity (array-dimension texture 1)
                              (array-dimension texture 0) :luminance :unsigned-byte tex-pointer)))
    ;; then load textures for those levels that are drawn by cl-vectors
    (iter (for texture in (textures-of gl-text))
          (for level from 0)
          (with-pointer-to-array (texture tex-pointer :uint8 (reduce #'* (array-dimensions texture)) :copy-in)
            (cl-opengl:tex-image-2d :texture-2d level :intensity (array-dimension texture 1)
                                    (array-dimension texture 0) 0 :luminance :unsigned-byte tex-pointer)))))

(defmethod clear-texture ((gl-text mipmap-opengl-text))
  (iter (for tex in (textures-of gl-text))
        (let ((vec (find-original-array tex)))
          (iter (for i index-of-vector vec)
                (setf (aref vec i) 0)))))

(defmethod ensure-characters ((characters sequence) (gl-text mipmap-opengl-text))
  (let ((chars-loaded (hash-table-keys (character-hash-of gl-text)))
        (textures (textures-of gl-text)))
    (let ((more-chars (set-difference (coerce characters 'list) chars-loaded)))
      (when more-chars
        (let ((chars (append chars-loaded more-chars)))
          (when chars-loaded (assert textures))
          (size-texture gl-text (length chars) :preserve t)
          (map nil (rcurry #'add-char gl-text nil) more-chars)
          (send-texture gl-text))))))

(defmethod :before ((string string) (gl-text mipmap-opengl-text) &key (kerning t) (depth-shift 0.0))
  (declare (ignore kerning string depth-shift))
  (gl:tex-parameter :texture-2d :texture-min-filter :nearest-mipmap-nearest)
  (gl:tex-parameter :texture-2d :texture-mag-filter :nearest-mipmap-nearest))
