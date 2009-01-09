(defpackage :opengl-text-test (:use :cl :opengl-text :opengl-text-vector :iterate))

(in-package :opengl-text-test)

(defparameter *font-file*
  "/usr/share/fonts/dejavu/DejaVuSerif.ttf")
(defvar *the-gl-font*)
(defvar *the-info-gl-font*)
(defvar *the-font*)
(defparameter *test-string* "This is a test. Wo P. T. YcVoi")
(defparameter *kerning* t)
(defparameter *kind* :basic)

(defclass opengl-text-window (glut:window)
  ()
  (:default-initargs :width 570 :height 570 :title "opengl-text-test"
                     :mode '(:single :rgba)))

(defmethod glut:display-window :before ((w opengl-text-window))
  (gl:shade-model :smooth)
  (gl:clear-color 0 0 0 0.5)
  (gl:hint :perspective-correction-hint :nicest)
  (setf *the-font* *font-file*)
  (setf *the-gl-font* (setup-font *the-font* 64))
  (setf *the-info-gl-font* (setup-font *the-font* 32)))

(defun setup-font (font emsquare)
  (make-vector-gl-text *the-font*))

(defmethod glut:reshape ((window opengl-text-window) w h)
  (gl:viewport 0 0 w h)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d 0 w 0 h)
  (gl:translate 0 h 0)                  ;start drawing at the top
  ;; this'll sort-of shrink text as the window shrinks
  (gl:scale (min w h) (min w h) 1))

(defun test-simple-draw-string (str scale)
  (gl:enable :texture-2d)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:enable-client-state :vertex-array)
  (gl:enable-client-state :texture-coord-array)
  (gl:clear :color-buffer-bit)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  ;; each line is about this %age of the screen's shorter dimension
  (gl:scale scale scale 1)
  (gl:translate 0 -1.5 0)
  (gl:bind-texture :texture-2d 0)
  (gl:with-primitive :quads
    (gl:color 0 0 1)
    (gl:vertex 0 0 0)
    (gl:vertex 1 0 0)
    (gl:vertex 1 1 0)
    (gl:vertex 0 1 0))
  (gl:color 0.9 0.9 0.9)
  (draw-gl-string str *the-gl-font* :kerning *kerning*)
  (gl:flush)
  (gl:translate 0 -1 0)
  (gl:color 0.6 0.7 0.8)
  (draw-gl-string (format nil "emsquare: ~D  kerning: ~A"
                          (emsquare-of *the-gl-font*) *kerning*)
                  *the-info-gl-font*)
  (gl:flush)
  (gl:translate 0 -1 0)
  (gl:with-pushed-matrix
    (gl:translate 1 -0.4 0)
    (gl:rotate 30 0 0 1)
    (gl:color 1 1 1 0.7)
    (draw-gl-string str *the-gl-font* :kerning nil)
    (gl:translate 3 0 0)
    (gl:color 1 1 1 0.5)
    (draw-gl-string str *the-gl-font*)
    (gl:flush))
  (gl:translate 4 0 0)
  (gl:scale 2 2 1)
  (gl:bind-texture :texture-2d (opengl-text::texture-number-of *the-gl-font*))
  (gl:tex-env :texture-env :texture-env-mode :blend)
  (gl:tex-env :texture-env :texture-env-color '(1 1 1 1))
  (ecase *kind*
    (:mipmap
       (progn
         (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
         (gl:tex-parameter :texture-2d :texture-mag-filter :linear-mipmap-linear)))
    ((nil)
       (progn
         (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
         (gl:tex-parameter :texture-2d :texture-mag-filter :nearest))))
  (gl:with-primitive :quads
    (gl:color 0 1 0)
    (gl:tex-coord 0 0)
    (gl:vertex -1 -5 -1)
    (gl:tex-coord 0 1)
    (gl:vertex -1 0 -1)
    (gl:tex-coord 1 1)
    (gl:vertex 4 0 -1)
    (gl:tex-coord 1 0)
    (gl:vertex 4 -5 -1))
  (gl:flush))

(defmethod glut:display ((window opengl-text-window))
  (with-simple-restart (skip-draw "Skip this drawing")
    (test-simple-draw-string *test-string* 1/15)))

(defmethod glut:keyboard ((window opengl-text-window) key x y)
  (declare (ignore x y))
  (cond
    ((eql key #\Tab)
     (setf *test-string* (format nil "~A~A" *test-string*
                                 (code-char (+ 33 (random 94))))))
    ((eql key #\Backspace)
     (setf *test-string* (subseq *test-string* 0
                                 (max 0 (1- (length *test-string*))))))
    ((eql key #\Esc)
     ;; FIXME: hack!  should go back to using finalizer!
     (zpb-ttf:close-font-loader *the-font*)
     (glut:destroy-current-window)
     (return-from glut:keyboard))
    ((char< key #\Space)
     (let ((control-char (code-char (+ 64 (char-code key)))))
       (cond
         ((eql control-char #\R))
         ((eql control-char #\F)
          (setf *the-gl-font*
                (setup-font *the-font* (emsquare-of *the-gl-font*))))
         ((eql control-char #\M)
          (setf (emsquare-of *the-gl-font*)
                (* 2 (emsquare-of *the-gl-font*))))
         ((eql control-char #\N)
          (setf (emsquare-of *the-gl-font*)
                (max 1
                     (/ (emsquare-of *the-gl-font*) 2))))
         ((eql control-char #\K)
          (setf *kerning* (not *kerning*)))
          )))
    (t
     (setf *test-string* (format nil "~A~A" *test-string* key))))
  (glut:post-redisplay))

(defgeneric text-test (&optional kind))

(defmethod text-test (&optional (kind nil))
  (let ((*kind* kind))
    (glut:display-window (make-instance 'opengl-text-window))))
