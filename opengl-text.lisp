(in-package :opengl-text)

(defvar *opengl-active* t)

(defclass opengl-text ()
  ((font-loader :initarg :font :accessor font-loader-of)
   (emsquare :initarg :emsquare :initform 32 :accessor emsquare-of)
   (scaler :accessor scaler-of :initform nil)
   (scale-to-unit :accessor scale-to-unit-of :initform nil)
   (color :accessor color-of :initarg :color :initform (list 255 255 255))
   (texture :initform nil :accessor texture-of)
   (texture-number :initform nil :accessor texture-number-of)
   (character-hash :initform (make-hash-table) :accessor character-hash-of)))

(defmethod initialize-instance :after ((instance opengl-text) &rest initargs)
  (declare (ignore initargs))
  (when *coerce-em-to-power-of-two*
    (setf (emsquare-of instance)
	  (ceiling-power-of-two (emsquare-of instance)))))

(defmethod (setf emsquare-of) :after (new-value (object opengl-text))
  (when *coerce-em-to-power-of-two*
    (setf (slot-value object 'emsquare)
	  (ceiling-power-of-two (emsquare-of object))))
  (flush-texture object :new-texture-array t))

(defmethod (setf color-of) :after (new-value (object opengl-text))
  (declare (ignore new-value))
  (flush-texture object))

(defmethod (setf font-loader-of) :after (new-value (object opengl-text))
  (declare (ignore new-value))
  (flush-texture object))

(defgeneric ensure-characters (characters gl-text)
  (:method ((characters sequence) (gl-text opengl-text))
    (let ((chars-loaded (hash-table-keys (character-hash-of gl-text)))
	  (texture (texture-of gl-text))
	  (em (emsquare-of gl-text)))
      (let ((more-chars (set-difference (coerce characters 'list) chars-loaded)))
	(when more-chars
	  (let ((chars (append chars-loaded more-chars)))
	    (setf (texture-of gl-text) (make-new-texture-array em (length chars)))
	    (when chars-loaded
	      (map-subarray texture (texture-of gl-text)
			    :target-range `(:all (0 ,(1- (array-dimension texture 1))) :all)))
	    (map nil (rcurry #'get-char-texture-coords gl-text) more-chars)))))))
