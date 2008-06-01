(defpackage :opengl-text-test (:use :cl :opengl-text :sdl))

(in-package :opengl-text-test)

(defun launch-sdl-opengl (w h)
  (init *init-video*)
  (gl-set-attribute +gl-red-size+ 8)
  (gl-set-attribute +gl-green-size+ 8)
  (gl-set-attribute +gl-blue-size+ 8)
  (gl-set-attribute +gl-alpha-size+ 8)
  (gl-set-attribute +gl-buffer-size+ 32)
  (gl-set-attribute +gl-depth-size+ 24)
  (gl-set-attribute +gl-doublebuffer+ 1)
  (set-video-mode w h 32 *opengl*)
  (cl-opengl:shade-model :smooth)
  (cl-opengl:clear-color 0 0 0 0.5)
  (cl-opengl:clear-depth 1.0)
  (cl-opengl:enable :depth-test)
  (cl-opengl:depth-func :lequal)
  (cl-opengl:hint :perspective-correction-hint :nicest)
  (cl-opengl:viewport 0 0 w h)
  (cl-opengl:matrix-mode :projection)
  (cl-opengl:load-identity)
  (cl-glu:perspective 45 (/ w h) 0.1 100)
  (cl-opengl:matrix-mode :modelview)
  (cl-opengl:load-identity))

(defun test-simple-draw-string (str)
  (gl:enable :texture-2d)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:enable-client-state :vertex-array)
  (gl:enable-client-state :texture-coord-array)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:translate 0 0 -8)
  (zpb-ttf:with-font-loader (font "/usr/share/fonts/dejavu/DejaVuSerif.ttf")
   (let ((gl-text (make-instance 'opengl-text :font font)))
     (gl:with-primitive :quads
       (gl:color 0 0 1)
       (gl:vertex 0 0 -1)
       (gl:vertex 1 0 -1)
       (gl:vertex 1 1 -1)
       (gl:vertex 0 1 -1))
     (print (gl:get-error))
     (draw-gl-string str gl-text)
     (gl-swap-buffers))))
