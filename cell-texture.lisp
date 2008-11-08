(in-package :opengl-text)

;;; pack glyphs in texture without scaling them

(defclass cell-texture ()
  ((texture-array  :accessor texture-array-of  :initarg :texture-array  :initform nil)
   (size           :accessor size-of           :initarg :size           :initform 0)
   (kind           :accessor kind-of           :initarg :type           :initform :uint8)
   (texture-width  :accessor texture-width-of  :initarg :texture-width :initform 0)
   (texture-height :accessor texture-height-of :initarg :texture-height :initform 0)
   (cell-map       :accessor cell-map-of       :initarg :cell-map       :initform (make-hash-table))))

(defgeneric get-cell (cell cell-tex)
  (:documentation "Extract cell into new array"))

(defgeneric pack-rectangle (cell-tex rectangle)
  (:documentation "Packs rectangle (2d array) into cell-tex, possible enlarging it. This might mean cell remapping"))

(defgeneric guarantee-space (cell-tex w h)
  (:documentation "Enlarge cell texture until rectangle with dimensions wxh fits."))

(defgeneric enlarge-cell-texture (cell-tex)
  (:documentation "Enlarge cell texture to the next power of two. Cell textures are always squares. Minimal size is 64."))

(defgeneric make-new-cell (cell-tex array top left)
  (:documentation "Add new cell and copy array into top/left position."))

;;; first implement simple and fast algorithm, but with very inefficient space usage

(defclass simple-cell-texture (cell-texture)
  ((current-top  :accessor current-top-of  :initarg :current-top :initform 0)
   (current-left :accessor current-left-of :initarg :current-left :initform 0)
   (current-h    :accessor current-h-of    :initarg :current-h :initform 0)))

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
                  (texture-array-of cell-tex) new-texture)
            (mapc (curry #'pack-rectangle cell-tex) old-cells))
          (setf (cell-map-of cell-tex) (make-hash-table)
                (texture-array-of cell-tex) new-texture)))))

(defmethod guarantee-space ((cell-tex simple-cell-texture) w h)
  (with-accessors ((top current-top-of) (left current-left-of) (c-h current-h-of)
                   (tw texture-width-of) (th texture-height-of)) cell-tex
    ;;check that there is either space in a row, or to create a new row
    (let ((space-horizontal (- tw left))
          (space-vertical (- th top c-h)))
      (unless (or (>= space-horizontal w)
                  (>= space-vertical h))
        (enlarge-cell-texture cell-tex)
        (guarantee-space cell-tex w h)))))

(defmethod make-new-cell (cell-tex array top left)
  (destructuring-bind (w h) (array-dimensions array)
    (let ((cell-map (cell-map-of cell-tex))
          (cell-array (texture-array-of cell-tex)))
      (let ((new-cell-number (hash-table-count cell-map))
            (xmax (+ left (1- w)))
            (ymax (+ top (1- h))))
        (iter (for x from left to xmax)
              (for ax from 0)
              (iter (for y from top to ymax)
                    (for ay from 0)
                    (setf (aref cell-array x y)
                          (aref array ax ay))))
        (setf (gethash new-cell-number cell-map)
              (list left top xmax ymax))
        new-cell-number))))

;;; simple-cell-texture is, as the name says, simple, it packs rectangles in rows as high as the
;;; biggest rectangle in row, starting new row when it runs out of space
(defmethod pack-rectangle ((cell-tex simple-cell-texture) rectangle)
  (destructuring-bind (w h) (array-dimensions rectangle)
    (with-accessors ((top current-top-of) (left current-left-of) (c-h current-h-of)
                     (array texture-array-of) (tw texture-width-of) (th texture-height-of)) cell-tex
      (assert (and (plusp w) (plusp h)))
      (guarantee-space cell-tex w h)
      ;; guarantee-space means that there is either space in a row or to create a new row
      (cond
        ;; try to fit in row
        ((>= (- tw left) w)
         (prog1
             (make-new-cell cell-tex rectangle top left)
           (setf c-h (max c-h h))
           (incf left w)))
        ;; move to a new row
        (t
         (incf top c-h)
         (setf left 0)
         (setf c-h h)
         (prog1
             (make-new-cell cell-tex rectangle top left)
           (incf left w)))))))
