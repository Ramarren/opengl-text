(in-package :opengl-text)

(defparameter *current-triangle* nil)
(defparameter *triangle-list* nil)

(cffi:defcenum gl-primitives
  (:points #x0000)
  (:lines #x0001)
  (:line-loop #x0002)
  (:line-strip #x0003)
  (:triangles #x0004)
  (:triangle-strip #x0005)
  (:triangle-fan #x0006)
  (:quads #x0007)
  (:quad-strip #x0008)
  (:polygon #x0009))

(cffi:defcallback tess-begin :void ((type gl-primitives))
  (declare (ignore type))
  (values))

(cffi:defcallback tess-end :void ()
  (when *current-triangle*
    (push *current-triangle* *triangle-list*)
    (setf *current-triangle* nil))
  (values))

(cffi:defcallback tess-vertex :void ((data :pointer))
  (push (vector (cffi:mem-aref data :double 0)
                (cffi:mem-aref data :double 1)
                (cffi:mem-aref data :double 2))
        *current-triangle*)
  (when (length= *current-triangle* 3)
    (push *current-triangle* *triangle-list*)
    (setf *current-triangle* nil)))

(cffi:defcallback tess-edge-flag :void ((flag :unsigned-char))
  (declare (ignore flag))
  (values))

(cffi:defcenum tess-callback-enum
  (:tess-begin 100100)
  (:tess-vertex 100101)
  (:tess-end 100102)
  (:tess-error 100103)
  (:tess-edge-flag 100104))


(cffi:defcfun ("gluTessCallback" register-tess-callback) :void
  (tess glu::tesselator)
  (which tess-callback-enum)
  (function-pointer :pointer))

(defmacro with-tesselator (tesselator &body body)
  `(let ((,tesselator (glu::new-tess)))
    (unwind-protect
         (progn
           (register-tess-callback ,tesselator :tess-begin (cffi:callback tess-begin))
           (register-tess-callback ,tesselator :tess-end (cffi:callback tess-end))
           (register-tess-callback ,tesselator :tess-vertex (cffi:callback tess-vertex))
           (register-tess-callback ,tesselator :tess-edge-flag (cffi:callback tess-edge-flag))
           ,@body
           )
      (glu::delete-tess ,tesselator))))

(defun test-tesselator ()
  (setf *triangle-list* nil)
  (setf *current-triangle* nil)
  (let ((data (make-ffa 12 :double :initial-contents '(0d0 0d0 0d0 1d0 0d0 0d0 1d0 1d0 0d0 0d0 1d0 0d0))))
    (with-pointer-to-array (data data-ptr :double (length data) :copy-in)
      (with-tesselator tess
       (glu::tess-begin-polygon tess (cffi:null-pointer))
       (glu::tess-begin-contour tess)
       (iter (with ptr = data-ptr)
             (for i from 0 below (/ (length data) 3))
             (glu::tess-vertex tess ptr ptr)
             (cffi:incf-pointer ptr (* 3 (cffi:foreign-type-size :double))))
       (glu::tess-end-contour tess)
       (glu::tess-end-polygon tess)))))

(defun create-data-ffa (path)
  (let ((iterator (paths:path-iterator-segmented path)))
    (let ((point-list
           (iter (for (values nil (x . y) endp) = (paths:path-iterator-next iterator))
                 (until endp)
                 (collect (float x 0d0))
                 (collect (float y 0d0))
                 (collect 0d0))))
      (make-ffa (length point-list) :double :initial-contents point-list))))

(defun create-vertex-array (triangle-list)
  (let ((ffa (make-ffa (* (length triangle-list) 9) :double)))
    (iter (for triangle in triangle-list)
          (for i from 0 by 9)
          (iter (for j from 0 by 3)
                (for vertex in triangle)
                (setf (subseq ffa (+ i j) (+ i j 3)) vertex)))
    ffa))

(defun tesselate-character (char font)
  "Take a character and a font and return a FFA of vertices."
  (let ((glyph (zpb-ttf:find-glyph char font)))
    (let ((paths (paths-ttf:paths-from-glyph glyph
                                             :scale-x (/ (zpb-ttf:units/em font))
                                             :scale-y (/ (zpb-ttf:units/em font)))))
      (let ((*current-triangle* nil)
            (*triangle-list* nil)
            (path-ffas (mapcar #'create-data-ffa paths)))
        (with-tesselator tess
          (glu::tess-begin-polygon tess (cffi:null-pointer))
          (dolist (data path-ffas)
            (with-pointer-to-array (data data-ptr :double (length data) :copy-in)
              (glu::tess-begin-contour tess)
              (iter (with ptr = data-ptr)
                    (for i from 0 below (/ (length data) 3))
                    (glu::tess-vertex tess ptr ptr)
                    (cffi:incf-pointer ptr (* 3 (cffi:foreign-type-size :double))))
              (glu::tess-end-contour tess)))
          (glu::tess-end-polygon tess))
        (create-vertex-array *triangle-list*)))))
