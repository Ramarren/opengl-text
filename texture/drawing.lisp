(in-package :opengl-text)

(defgeneric get-glyph (char gl-text)
  (:method ((char character) (gl-text textured-opengl-text))
    (or (gethash char (character-hash-of gl-text))
                      (add-char char gl-text))))

(defun generate-vertices (vertices tex-coords string gl-text kerning depth-shift)
  (iter (with kfun = (kerning-function-of gl-text))
        (with afun = (advance-function-of gl-text))
        (with chash = (character-hash-of gl-text))
        (for c in-string string)
        (for cp previous c initially nil)
        (for i from 0 by 4)
        (for j from 0)
        (when (and cp kerning);;note k comes from summing clause below
          (incf k (funcall kfun c cp)))
        (for (xmin ymin xmax ymax) next (actual-slice-of (gethash c chash)))
        (setf (aref vertices i 0) (+ k xmin)
              (aref vertices i 1) ymin
              (aref vertices i 2) (* j depth-shift)
              (aref vertices (+ i 1) 0) (- (1+ k) (- 1 xmax))
              (aref vertices (+ i 1) 1) ymin
              (aref vertices (+ i 1) 2) (* j depth-shift)
              (aref vertices (+ i 2) 0) (- (1+ k) (- 1 xmax))
              (aref vertices (+ i 2) 1) ymax
              (aref vertices (+ i 2) 2) (* j depth-shift)
              (aref vertices (+ i 3) 0) (+ k xmin)
              (aref vertices (+ i 3) 1) ymax
              (aref vertices (+ i 3) 2) (* j depth-shift))
        (let ((tex-coord (tex-coord-of (get-glyph c gl-text))))
         (iter (for ii from i to (+ i 3))
               (for k from 0)
               (setf (aref tex-coords ii 0) (aref tex-coord k 0)
                     (aref tex-coords ii 1) (aref tex-coord k 1))))
        (sum (funcall afun c) into k)))

(defgeneric get-buffers (l gl-text)
  (:method ((l integer) (gl-text textured-opengl-text))
    (let ((lg (length-of gl-text)))
      (when (and *auto-extend-buffers*
                 (> l (length-of gl-text)))
        (setf (length-of gl-text) l))
      (values (if (and (>= lg l)
                       (vertices-of gl-text))
                  (vertices-of gl-text)
                  (make-ffa (list (* 4 l) 3) :float))
              (if (and (>= lg l)
                       (tex-coords-of gl-text))
                  (tex-coords-of gl-text)
                  (make-ffa (list (* 4 l) 2) :float))))))

(defgeneric draw-gl-string (string gl-text &key kerning depth-shift)
  (:method :before ((string string) (gl-text textured-opengl-text) &key (kerning t) (depth-shift 0.0))
    (declare (ignore kerning string depth-shift))
    (gl:enable :blend)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:enable :texture-2d)
    (gl:tex-env :texture-env :texture-env-mode :modulate)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear))
  (:method ((string string) (gl-text textured-opengl-text) &key (kerning t) (depth-shift 0.0))
    (ensure-characters (remove-duplicates string) gl-text)
    (let ((l (length string)))
     (when (zerop l) (return-from draw-gl-string))
     (multiple-value-bind (vertices tex-coords) (get-buffers l gl-text)
       (generate-vertices vertices tex-coords string gl-text kerning depth-shift)
       (with-pointers-to-arrays ((vertices v-pointer :float (length (find-original-array vertices)) :copy-in)
                                 (tex-coords t-pointer :float (length (find-original-array tex-coords)) :copy-in))
         (%gl:vertex-pointer 3 :float 0 v-pointer)
         (%gl:tex-coord-pointer 2 :float 0 t-pointer)
         (gl:bind-texture :texture-2d (texture-number-of gl-text))
         (gl:with-pushed-matrix
           (when-let (slide (funcall (placement-function-of gl-text) (char string 0)))
             (apply #'gl:translate slide))
           (gl:draw-arrays :quads 0 (* 4 (length string)))))))))
