(in-package :opengl-text-test)

(defclass mipmap-opengl-text-window (opengl-text-window)
  ()
  (:default-initargs :width 570 :height 570 :title "mipmap-opengl-text-test"
                     :mode '(:single :rgba)))

(defun setup-font-mipmap (font emsquare)
  (make-instance 'mipmap-opengl-text :font font :emsquare emsquare))

(defmethod glut:display-window :before ((w mipmap-opengl-text-window))
  (gl:shade-model :smooth)
  (gl:clear-color 0 0 0 0.5)
  (gl:hint :perspective-correction-hint :nicest)
  (setf *the-font* (zpb-ttf:open-font-loader *font-file*))
  (setf *the-gl-font* (setup-font-mipmap *the-font* 64))
  (setf *the-info-gl-font* (setup-font-mipmap *the-font* 32)))

(defmethod text-test ((mipmap (eql t)))
  (glut:display-window (make-instance 'mipmap-opengl-text-window)))