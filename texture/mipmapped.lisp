(in-package :opengl-text)

;;; pack glyphs as rectangles to avoid scaling issues

(defclass mipmap-opengl-text (textured-opengl-text)
  ())

(defmethod send-texture ((gl-text mipmap-opengl-text))
  (when *opengl-active*
    (if (texture-number-of gl-text)
        (cl-opengl:bind-texture :texture-2d (texture-number-of gl-text))
        (let ((new-number (car (cl-opengl:gen-textures 1))))
          (setf (texture-number-of gl-text) new-number)
          (cl-opengl:bind-texture :texture-2d new-number)
          (trivial-garbage:finalize gl-text #'(lambda ()
                                                (gl:delete-textures (list new-number))))))
    (let ((texture (texture-array-of (texture-of gl-text))))
      (with-pointer-to-array (texture
                              tex-pointer
                              :uint8
                              (length (find-original-array texture))
                              :copy-in)
        (glu:build-2d-mipmaps :texture-2d :intensity (array-dimension texture 1)
                              (array-dimension texture 0) :luminance :unsigned-byte tex-pointer)))))

(defmethod :before ((string string) (gl-text mipmap-opengl-text) &key (kerning t) (depth-shift 0.0))
  (declare (ignore kerning string depth-shift))
  (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
  (gl:tex-parameter :texture-2d :texture-mag-filter :linear-mipmap-linear))
