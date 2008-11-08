(in-package :opengl-text)

;;; pack glyphs in texture without scaling them

(defclass cell-texture ()
  ((texture-array  :accessor texture-array-of  :initarg :texture-array  :initform nil)
   (size           :accessor size-of           :initarg :size           :initform 0)
   (kind           :accessor kind-of           :initarg :type           :initform :uint8)
   (texture-width  :accessor texture-width-of  :initarg :texture-width)
   (texture-height :accessor texture-height-of :initarg :texture-height)
   (cell-map       :accessor cell-map-of       :initarg :cell-map       :initform (make-hash-table))))

(defgeneric get-cell (cell cell-tex)
  (:documentation "Extract cell into new array"))

(defgeneric pack-rectangle (cell-tex rectangle)
  (:documentation "Packs rectangle (2d array) into cell-tex, possible enlarging it. This might mean cell remapping"))

(defgeneric guarantee-space (cell-tex w h)
  (:documentation "Enlarge cell texture until rectangle with dimensions wxh fits."))

(defgeneric enlarge-cell-texture (cell-tex)
  (:documentation "Enlarge cell texture to the next power of two. Cell textures are always squares. Minimal size is 64."))

;;; first implement simple and fast algorithm, but with very inefficient space usage

(defclass simple-cell-texture (cell-texture)
  ((current-top  :accessor current-top-of  :initarg :current-top)
   (current-left :accessor current-left-of :initarg :current-left)
   (current-h    :accessor current-h-of    :initarg :current-h)))

(defmethod get-cell ((cell integer) (cell-tex cell-texture))
  (destructuring-bind (xmin ymin xmax ymax) (gethash cell (cell-map-of cell-tex))
    (let ((new-array (make-ffa (list (- xmax xmin) (- ymax ymin)) (kind-of cell-tex)))
          (cell-array (texture-array-of cell-tex)))
      (iter (for x from xmin to xmax)
            (iter (for y from ymin to ymax)
                  (setf (aref new-array (- x xmin) (- y ymin))
                        (aref cell-array x y)))))))

(defmethod enlarge-cell-texture ((cell-tex cell-texture))
  (let ((new-size (ash 32 (1+ (size-of cell-tex)))))
    (let ((new-texture (make-ffa (list new-size new-size) (kind-of cell-tex))))
      (incf (size-of cell-tex))
      (setf (texture-width-of cell-tex) new-size)
      (setf (texture-height-of cell-tex) new-size)
      (if (texture-array-of cell-tex)
          (let ((old-cells (iter (for cell in (hash-table-keys (cell-map-of cell-tex)))
                                 (collect (get-cell cell cell-tex)))))
            (setf (cell-map-of cell-tex) (make-hash-table)
                  (texture-of cell-tex) new-texture)
            (mapc (curry #'pack-rectangle cell-tex) old-cells))
          (setf (cell-map-of cell-tex) (make-hash-table)
                (texture-of cell-tex) new-texture)))))

(defmethod guarantee-space ((cell-tex simple-cell-texture) w h)
  (with-accessors ((top current-top-of) (left current-left-of)
                   (tw texture-width-of) (th texture-height-of)) cell-tex
    (let ((space-horizontal (- tw left))
          (space-vertical (- th top)))
      (unless (and (>= space-horizontal w)
                   (>= space-vertical h))
        (enlarge-cell-texture cell-tex)
        (guarantee-space cell-tex w h)))))

;;; simple-cell-texture is, as the name says, simple, it packs rectangles in rows as high as the
;;; biggest rectangle in row, starting new row when it runs out of space
(defmethod pack-rectangle ((cell-tex simple-cell-texture) rectangle)
  (destructuring-bind (w h) (array-dimensions rectangle)
    (with-accessors ((top current-top-of) (left current-left-of) (c-h current-h-of)
                     (array texture-array-of) (tw texture-width-of) (th texture-height-of)) cell-tex
      (let ((space-horizontal (- tw left))
            (space-vertical (- th top)))
        (when ))
      
      ))
  )