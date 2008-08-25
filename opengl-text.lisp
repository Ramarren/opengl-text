(in-package :opengl-text)

(defvar *opengl-active* t)
(defvar *coerce-em-to-power-of-two* t)
(defvar *auto-extend-buffers* t)

(defun ceiling-power-of-two (number)
  (expt 2 (ceiling (log number 2))))

(defun maybe-ceiling-power-of-two (number)
  (declare (inline ceiling-power-of-two))
  (if *coerce-em-to-power-of-two*
      (ceiling-power-of-two number)
      number))

(defun get-font-loader (font-designator)
  (etypecase font-designator
    ((or string pathname) (zpb-ttf:open-font-loader font-designator))
    (zpb-ttf::font-loader font-designator)))

(defclass opengl-text ()
  ((font-loader :initarg :font :accessor font-loader-of)
   (emsquare :initarg :emsquare :initform 32 :accessor emsquare-of)
   (length :initarg :length :initform 20 :accessor length-of)
   (vertices :accessor vertices-of :initform nil)
   (tex-coords :accessor tex-coords-of :initform nil)
   (scaler :accessor scaler-of :initform nil)
   (scale-to-unit :accessor scale-to-unit-of :initform nil)
   (texture :initform nil :accessor texture-of)
   (texture-number :initform nil :accessor texture-number-of)
   (character-hash :initform (make-hash-table) :accessor character-hash-of)))

(defmethod initialize-instance :after ((instance opengl-text) &rest initargs)
  (declare (ignore initargs))
  (when *coerce-em-to-power-of-two*
    (setf (emsquare-of instance)
          (ceiling-power-of-two (emsquare-of instance))))
  ;; force :after method on setf to run, so that scaler field is initialized
  (when (font-loader-of instance)
   (setf (font-loader-of instance) (get-font-loader
 (font-loader-of instance))))
  (when (length-of instance)
    (setf (length-of instance) (length-of instance))))

(defmethod (setf length-of) :after (new-value (object opengl-text))
  (when (plusp new-value)
     (setf (vertices-of object)
           (make-ffa (list (* 4 new-value) 3) :float))
     (setf (tex-coords-of object)
           (make-ffa (list (* 4 new-value) 2) :float))))

(defmethod (setf emsquare-of) :after (new-value (object opengl-text))
  (when *coerce-em-to-power-of-two*
    (setf (slot-value object 'emsquare)
          (ceiling-power-of-two (emsquare-of object))))
  (flush-texture object :new-texture-array t))

(defmethod (setf font-loader-of) :around ((new-value string) (object opengl-text))
  (setf (font-loader-of object) (get-font-loader new-value)))

(defmethod (setf font-loader-of) :around ((new-value pathname) (object opengl-text))
  (setf (font-loader-of object) (get-font-loader new-value)))

(defmethod (setf font-loader-of) :after ((new-value zpb-ttf::font-loader) (object opengl-text))
  (let ((bb (zpb-ttf:bounding-box new-value)))
    (let ((scaler (max (- (zpb-ttf:xmax bb)
                          (zpb-ttf:xmin bb))
                       (- (zpb-ttf:ymax bb)
                          (zpb-ttf:ymin bb)))))
      (setf (scaler-of object) scaler)
      (setf (scale-to-unit-of object) (/ scaler (zpb-ttf:units/em new-value)))))
  (flush-texture object))
