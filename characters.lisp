(in-package :opengl-text)

(defun create-char-path (char font em)
  (let ((bb-glyph (zpb-ttf:bounding-box (zpb-ttf:find-glyph char font)))
	(em (1- em)))
    (let ((scale-x (- (zpb-ttf:xmax bb-glyph) (zpb-ttf:xmin bb-glyph)))
	  (scale-y (- (zpb-ttf:ymax bb-glyph) (zpb-ttf:ymin bb-glyph))))
      ;; whitespace has no extent
      (when (zerop scale-x) (setf scale-x 1))
      (when (zerop scale-y) (setf scale-y 1))
      (paths-ttf:paths-from-glyph (zpb-ttf:find-glyph char font)
				  :offset (paths:make-point (* em
							       (- (/ (zpb-ttf:xmin bb-glyph) scale-x)))
							    (+ (1+ em)
							       (* em
								  (/ (zpb-ttf:ymin bb-glyph) scale-y))))
				  :scale-x (/ em scale-x)
				  :scale-y (- (/ em scale-y))))))

(defun draw-char (char gl-text)
  (let ((em (emsquare-of gl-text)))
   (let ((char-path (create-char-path char (font-loader-of gl-text) em))
	 (aa-state (aa:make-state))
	 (out-array (make-array (list em em 4) :initial-element 0)))
     (destructuring-bind (r g b) (color-of gl-text)
       (flet ((draw-function (x y alpha)
		(if (array-in-bounds-p out-array (- em y) x 0)
		    (setf (aref out-array (- em y) x 0) r
			  (aref out-array (- em y) x 1) g
			  (aref out-array (- em y) x 2) b
			  (aref out-array (- em y) x 3) (clamp alpha 0 255))
		    (warn "Out of bounds: ~a ~a" (- em y) x))))
	 (aa:cells-sweep (vectors:update-state aa-state char-path) #'draw-function)
	 out-array)))))

(defun compute-actual-slice (char gl-text)
  (let ((font (font-loader-of gl-text)))
    (let ((glyph (zpb-ttf:find-glyph char font))
	  (bb (zpb-ttf:bounding-box font)))
      (let ((base (max (- (zpb-ttf:xmax bb)
			  (zpb-ttf:xmin bb))
		       (- (zpb-ttf:ymax bb)
			  (zpb-ttf:ymin bb))))
	    (x-edge (zpb-ttf:xmin bb))
	    (y-edge (zpb-ttf:ymin bb))
	    (bb-glyph (zpb-ttf:bounding-box glyph)))
	(mapcar #'float
		(list (/ (- (zpb-ttf:xmin bb-glyph) x-edge) base)
		      (/ (- (zpb-ttf:ymin bb-glyph) y-edge) base)
		      (/ (- (zpb-ttf:xmax bb-glyph) x-edge) base)
		      (+ (/ (- (zpb-ttf:ymax bb-glyph) y-edge) base)
			 (/ 4 (emsquare-of gl-text)))))))))

(defun transform-cell (cell array em)
  "Transform cell to relative coordinates (OpenGL TexCoords)."
  (destructuring-bind ((xmin xmax) (ymin ymax) rgba) (cell-range cell em array)
    (assert (eql rgba :all));sanity check
    (destructuring-bind (w h rgba) (array-dimensions array)
      (assert (= rgba 4));sanity check
      (make-array '(4 2)
		  :initial-contents
		  (list (list (/ xmin w) (/ ymin h))
			(list (/ xmax w) (/ ymin h))
			(list (/ xmax w) (/ ymax h))
			(list (/ xmin w) (/ ymax h)))))))

(defun old-chars-reinsert-add-new (new-char character-hash character-cells cell array em)
  "Return a hash table with coordinates relative to array, with new character added to cell.
   Update character-cells hashtable."
  (let ((old-chars (sort (hash-table-keys character-hash)
			 #'< :key #'(lambda (k)
				      (gethash (car k) character-cells))))
	(new-character-hash (make-hash-table)))
    (iter (for old-char in old-chars)
	  (for old-cell from 0)
	  (setf (gethash old-char new-character-hash)
		(transform-cell old-cell array em)))
    (setf (gethash new-char character-cells)
	  cell)
    (setf (gethash new-char new-character-hash)
	  (transform-cell cell array em))
    new-character-hash))

(defgeneric add-char (char gl-text)
  (:method ((char character) (gl-text opengl-text))
    (let ((charh (character-hash-of gl-text))
	  (em (emsquare-of gl-text)))
      (let ((new-count (1+ (hash-table-count charh))))
	(let ((new-texture (if (or (null (texture-of gl-text))
				    (> new-count (character-cells em (texture-of gl-text))))
				(make-new-texture-array em new-count)
				(texture-of gl-text))))
	  (when (and (texture-of gl-text)
		     (not (eq (texture-of gl-text) new-texture)))
	    (iter (for cell from 0 below (hash-table-count charh))
		  (copy-character (texture-of gl-text) cell new-texture cell em)))
	  (copy-character (draw-char char gl-text) 0 new-texture new-count em)
	  (setf (character-hash-of gl-text)
		(old-chars-reinsert-add-new char charh
					    (character-cells-of gl-text)
					    new-count new-texture em))
	  (setf (texture-of gl-text) new-texture)
	  (send-texture new-texture gl-text)
	  (gethash char (character-hash-of gl-text)))))))

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
	      (iter (for cell from 0 below (length chars-loaded))
		    (copy-character texture cell (texture-of gl-text) cell em)))
	    (map nil (rcurry #'get-char-texture-coords gl-text) more-chars)))))))
