(in-package :opengl-text)

(defvar *auto-extend-buffers* t)

;;; draw-glyph-function is a function of two arguments, character and size of the font in pixels (of
;;; the emsquare), and returns a luminance array with the glyph

;;; kerning-function returns kerning offset in em-squares

(defclass textured-opengl-text ()
  ((draw-glyph-function :initarg  :draw-glyph-function :accessor draw-glyph-function-of)
   (kerning-function    :initarg  :kerning-function    :initform (constantly nil)       :accessor kerning-function-of)
   (emsquare            :initarg  :emsquare            :initform 32                     :accessor emsquare-of)
   (length              :initarg  :length              :initform 20                     :accessor length-of)
   (vertices            :accessor vertices-of          :initform nil)
   (tex-coords          :accessor tex-coords-of        :initform nil)
   (character-hash      :initform (make-hash-table)    :accessor character-hash-of)
   (texture-number      :accessor texture-number-of    :initarg  :texture-number        :initform nil)
   (texture             :initform nil                  :accessor texture-of)))

(defmethod initialize-instance :after ((instance textured-opengl-text) &rest initargs)
  (declare (ignore initargs))
  ;; force :after method on setf to run, so that scaler field is initialized
  (when *coerce-em-to-power-of-two*
    (setf (emsquare-of instance)
          (ceiling-power-of-two (emsquare-of instance))))
  (when (length-of instance)
    (setf (length-of instance) (length-of instance)))
  (when (draw-glyph-function-of instance)
    (setf (draw-glyph-function-of instance) (draw-glyph-function-of instance))))

(defmethod (setf length-of) :after (new-value (object textured-opengl-text))
  (when (plusp new-value)
    (setf (vertices-of object)
          (make-ffa (list (* 4 new-value) 3) :float))
    (setf (tex-coords-of object)
          (make-ffa (list (* 4 new-value) 2) :float))))

(defmethod (setf emsquare-of) :after (new-value (object textured-opengl-text))
  (when *coerce-em-to-power-of-two*
    (setf (slot-value object 'emsquare)
          (ceiling-power-of-two (emsquare-of object))))
  (flush-texture object :new-texture-array t))

(defmethod (setf draw-glyph-function-of) :after (new-value (object textured-opengl-text))
  (flush-texture object :new-texture-array t))
