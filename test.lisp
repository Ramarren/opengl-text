(defpackage :opengl-text-test (:use :cl :opengl-text :iterate))

(in-package :opengl-text-test)

(defparameter *font-name*
  "/usr/share/fonts/truetype/ttf-dejavu/DejaVuSerif.ttf")
(defvar *the-gl-font*)

(defclass opengl-text-window (glut:window)
  ()
  (:default-initargs :width 570 :height 570 :title "opengl-text-test"
                     :mode '(:single :rgba)))

(defmethod glut:display-window :before ((w opengl-text-window))
  (gl:shade-model :smooth)
  (gl:clear-color 0 0 0 0.5)
  (gl:hint :perspective-correction-hint :nicest)
  (setup-font))

(defun setup-font ()
  (let ((font (zpb-ttf:open-font-loader *font-name*)))
    (setf *the-gl-font* (make-instance 'opengl-text :font font :emsquare 128))
    (trivial-garbage:finalize *the-gl-font*
			      #'(lambda ()
				  (zpb-ttf:close-font-loader font)))))

(defmethod glut:reshape ((window opengl-text-window) w h)
  (gl:viewport 0 0 w h)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d 0 w 0 h)
  (gl:translate 0 h 0)			;start drawing at the top
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
  (gl:color 1 1 0)
  (draw-gl-string str *the-gl-font*)
  (gl:flush)
  (gl:translate 0 -1 0)
  (draw-gl-string str *the-gl-font* :kerning nil)
  (gl:flush)
  (gl:translate 0 -1 0)
  (when nil				;this slows things down!
    (gl:translate 0 -0.4 0)
    (gl:rotate 30 0 0 1)
    (setf (emsquare-of *the-gl-font*) 128)
    (draw-gl-string str *the-gl-font* :kerning nil)
    (gl:translate 3 0 0)
    (draw-gl-string str *the-gl-font*)
    (gl:translate -2 -1 0))
  (gl:translate 4 0 0)
  (gl:scale 2 2 1)
  (gl:bind-texture :texture-2d (opengl-text::texture-number-of *the-gl-font*))
  (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
  (gl:tex-env :texture-env :texture-env-mode :decal)
  (gl:with-primitive :quads
    (gl:color 0 1 0)
    (gl:tex-coord 0 0)
    (gl:vertex -1 -5 -1)
    (gl:tex-coord 0 1)
    (gl:vertex -1 0 -1)
    (gl:tex-coord 1 1)
    (gl:vertex 4 0 -1)
    (gl:tex-coord 1 0)
    (gl:vertex 4 -5 -1)))

(defmethod glut:display ((window opengl-text-window))
  (with-simple-restart (skip-draw "Skip this drawing")
    (test-simple-draw-string "This is a test." 1/15))
  (gl:flush))

(defmethod glut:keyboard ((window opengl-text-window) key x y)
  (declare (ignore x y))
  (when (eql key #\r)
    (glut:post-redisplay))
  (when (eql key #\f)
    (setup-font)
    (glut:post-redisplay))
  (when (eql key #\Esc)
    (glut:destroy-current-window)))

(defmethod text-test ()
  (glut:display-window (make-instance 'opengl-text-window)))
