(in-package :opengl-text)

;;; pack glyphs in texture without scaling them

(defclass cell-texture ()
  ((texture-array  :accessor texture-array-of  :initarg :texture-array  :initform nil)
   (size           :accessor size-of           :initarg :size           :initform 0)
   (kind           :accessor kind-of           :initarg :type           :initform :uint8)
   (texture-width  :accessor texture-width-of  :initarg :texture-width :initform 0)
   (texture-height :accessor texture-height-of :initarg :texture-height :initform 0)
   (cell-map       :accessor cell-map-of       :initarg :cell-map       :initform (make-hash-table))))

(defgeneric clear-texture (cell-tex)
  (:documentation "Remove all cells and zero the array, but leave size."))

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

(defgeneric transform-cell-coords (cell cell-tex)
  (:documentation "Transform cell coordinates to 0..1 space."))

(defmethod clear-texture ((cell-tex cell-texture))
  (destructuring-bind (w h) (texture-array-of cell-tex)
    (iter (for i from 0 below w)
          (with texture = (texture-array-of cell-tex))
          (iter (for j from 0 below h)
                (setf (aref texture i j) 0))))
  (setf (cell-map-of cell-tex) (make-hash-table)))

(defmethod transform-cell-coords ((cell integer) (cell-tex cell-texture))
  (destructuring-bind (xmin ymin xmax ymax) (gethash cell (cell-map-of cell-tex))
    (destructuring-bind (w h) (array-dimensions (texture-array-of cell-tex))
      (let ((xmax (1+ xmax))
            (ymax (1+ ymax)));;cell boundaries are inclusive
       (make-array '(4 2)
                   :element-type 'single-float
                   :initial-contents
                   (mapcar (curry #'mapcar #'float)
                           (list (list (/ ymin h) (/ xmin w))
                                 (list (/ ymin h) (/ xmax w))
                                 (list (/ ymax h) (/ xmax w))
                                 (list (/ ymax h) (/ xmin w)))))))))

(defmethod get-cell ((cell integer) (cell-tex cell-texture))
  (destructuring-bind (xmin ymin xmax ymax) (gethash cell (cell-map-of cell-tex))
    (let ((new-array (make-ffa (list (1+ (- xmax xmin)) (1+ (- ymax ymin))) (kind-of cell-tex)))
          (cell-array (texture-array-of cell-tex)))
      (iter (for x from xmin to xmax)
            (iter (for y from ymin to ymax)
                  (setf (aref new-array (- x xmin) (- y ymin))
                        (aref cell-array x y))))
      new-array)))

(defmethod enlarge-cell-texture ((cell-tex cell-texture))
  (let ((new-size (ash 32 (1+ (size-of cell-tex)))))
    (let ((new-texture (make-ffa (list new-size new-size) (kind-of cell-tex))))
      (incf (size-of cell-tex))
      (setf (texture-width-of cell-tex) new-size)
      (setf (texture-height-of cell-tex) new-size)
      (if (texture-array-of cell-tex)
          (let ((old-cells (iter (for cell from 0 below (hash-table-count (cell-map-of cell-tex)))
                                 (collect (get-cell cell cell-tex)))))
            (setf (cell-map-of cell-tex) (make-hash-table)
                  (texture-array-of cell-tex) new-texture)
            (mapc (curry #'pack-rectangle cell-tex) old-cells))
          (setf (cell-map-of cell-tex) (make-hash-table)
                (texture-array-of cell-tex) new-texture)))))

(defmethod make-new-cell ((cell-tex cell-texture) array top left)
  (destructuring-bind (w h) (array-dimensions array)
    (let ((cell-map (cell-map-of cell-tex))
          (cell-array (texture-array-of cell-tex)))
      (let ((new-cell-number (hash-table-count cell-map))
            (xmax (+ left (1- w)))
            (ymax (+ top (1- h))))
        (iter (for x from left to xmax)
              (for ax from 0 below w)
              (iter (for y from top to ymax)
                    (for ay from 0 below h)
                    (setf (aref cell-array x y)
                          (aref array ax ay))))
        (setf (gethash new-cell-number cell-map)
              (list left top xmax ymax))
        new-cell-number))))

;;; first implement simple and fast algorithm, but with very inefficient space usage

(defclass simple-cell-texture (cell-texture)
  ((current-top  :accessor current-top-of  :initarg :current-top :initform 0)
   (current-left :accessor current-left-of :initarg :current-left :initform 0)
   (current-h    :accessor current-h-of    :initarg :current-h :initform 0)))

(defmethod enlarge-cell-texture :before ((cell-tex simple-cell-texture))
  (setf (current-top-of cell-tex) 0
        (current-left-of cell-tex) 0
        (current-h-of cell-tex) 0))

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
