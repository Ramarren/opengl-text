(in-package :opengl-text)

(defvar *opengl-active* t)
(defvar *coerce-em-to-power-of-two* t)

(defun ceiling-power-of-two (number)
  (expt 2 (ceiling (log number 2))))

(defun maybe-ceiling-power-of-two (number)
  (declare (inline ceiling-power-of-two))
  (if *coerce-em-to-power-of-two*
      (ceiling-power-of-two number)
      number))

(defclass opengl-text ()
  ((font-loader :initarg :font :accessor font-loader-of)
   (emsquare :initarg :emsquare :initform 32 :accessor emsquare-of)
   (scaler :accessor scaler-of :initform nil)
   (scale-to-unit :accessor scale-to-unit-of :initform nil)
   (texture :initform nil :accessor texture-of)
   (texture-number :initform nil :accessor texture-number-of)
   (character-hash :initform (make-hash-table) :accessor character-hash-of)
   (character-cells :initform (make-hash-table) :accessor character-cells-of)))

(defmethod initialize-instance :after ((instance opengl-text) &rest initargs)
  (declare (ignore initargs))
  (when *coerce-em-to-power-of-two*
    (setf (emsquare-of instance)
	  (ceiling-power-of-two (emsquare-of instance))))
  ;; force :after method on setf to run, so that scaler field is initialized
  (when (font-loader-of instance)
   (setf (font-loader-of instance) (font-loader-of instance))))

(defmethod (setf emsquare-of) :after (new-value (object opengl-text))
  (when *coerce-em-to-power-of-two*
    (setf (slot-value object 'emsquare)
	  (ceiling-power-of-two (emsquare-of object))))
  (flush-texture object :new-texture-array t))

(defmethod (setf font-loader-of) :after (new-value (object opengl-text))
  (let ((bb (zpb-ttf:bounding-box new-value)))
    (let ((scaler (max (- (zpb-ttf:xmax bb)
			  (zpb-ttf:xmin bb))
		       (- (zpb-ttf:ymax bb)
			  (zpb-ttf:ymin bb)))))
      (setf (scaler-of object) scaler)
      (setf (scale-to-unit-of object) (/ scaler (zpb-ttf:units/em new-value)))))
  (flush-texture object))

