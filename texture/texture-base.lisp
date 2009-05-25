(in-package :opengl-text)

(defvar *auto-extend-buffers* t)

;; each character in character-hash refers to a glyph object
(defclass glyph ()
  ((tex-coord :initarg :tex-coord :accessor tex-coord-of)
   (cell :initarg :cell :accessor cell-of)
   (actual-slice :initarg :actual-slice :accessor actual-slice-of)))

;;; draw-glyph-function is a function of two arguments, character and size of the font in pixels (of
;;; the emsquare), and returns a luminance array with the glyph and actual slice of the unit square
;;; in which the array is placed

;;; kerning-function takes two arguments, characters or nils, and returns kerning offset in em-squares or nil
;;; advance function return glyph advance width in em-squares
;;; placement function takes first character and returns an xy shift in em-squares or nil

(defclass textured-opengl-text ()
  ((draw-glyph-function :initarg  :draw-glyph-function :accessor draw-glyph-function-of)
   (kerning-function    :initarg  :kerning-function    :initform (constantly nil)       :accessor kerning-function-of)
   (advance-function    :initarg  :advance-function    :accessor advance-function-of)
   (placement-function  :initarg  :placement-function  :initform (constantly nil) :accessor placement-function-of)
   (emsquare            :initarg  :emsquare            :initform 32                     :accessor emsquare-of)
   (length              :initarg  :length              :initform 20                     :accessor length-of)
   (vertices            :accessor vertices-of          :initform nil)
   (tex-coords          :accessor tex-coords-of        :initform nil)
   (character-hash      :initform (make-hash-table)    :accessor character-hash-of)
   (texture-number      :accessor texture-number-of    :initarg  :texture-number        :initform nil)
   (texture             :initform (make-instance 'simple-cell-texture)  :accessor texture-of)))

(defgeneric flush-texture (gl-text &key new-texture-array))
(defgeneric send-texture (gl-text))
(defgeneric clear-texture (gl-text))

(defmethod initialize-instance :after ((instance textured-opengl-text) &rest initargs)
  (declare (ignore initargs))
  ;; force :after method on setf to run, so that scaler field is initialized
  (when (length-of instance)
    (setf (length-of instance) (length-of instance)))
  (when (draw-glyph-function-of instance)
    (setf (draw-glyph-function-of instance) (draw-glyph-function-of instance))))

(defmethod (setf length-of) :after (new-value (object textured-opengl-text))
  (when (plusp new-value)
    (setf (vertices-of object)
          (make-array (list (* 4 new-value) 3) :element-type 'single-float))
    (setf (tex-coords-of object)
          (make-array (list (* 4 new-value) 2) :element-type 'single-float))))

(defmethod (setf emsquare-of) :after (new-value (object textured-opengl-text))
  (declare (ignore new-value))
  (flush-texture object :new-texture-array t))

(defmethod (setf draw-glyph-function-of) :after (new-value (object textured-opengl-text))
  (declare (ignore new-value))
  (flush-texture object :new-texture-array t))

(defgeneric draw-char (char gl-text)
  (:method (char (gl-text textured-opengl-text))
    (funcall (draw-glyph-function-of gl-text)
             char
             (emsquare-of gl-text))))

(defgeneric add-char (char gl-text &optional send-texture))

(defgeneric ensure-characters (characters gl-text)
  (:method ((characters sequence) (gl-text textured-opengl-text))
    (let ((chars-loaded (hash-table-keys (character-hash-of gl-text)))
          (texture (texture-of gl-text)))
      (let ((more-chars (set-difference (coerce characters 'list) chars-loaded)))
        (when more-chars
          (when chars-loaded (assert texture))
          (map nil (rcurry #'add-char gl-text nil) more-chars)
          (send-texture gl-text))))))
