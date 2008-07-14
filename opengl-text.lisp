(in-package :opengl-text)

(defvar *opengl-active* t)
(defvar *coerce-em-to-power-of-two* t)

(defclass opengl-text ()
  ((font-loader :initarg :font :accessor font-loader-of)
   (emsquare :initarg :emsquare :initform 32 :accessor emsquare-of)
   (scaler :accessor scaler-of :initform nil)
   (scale-to-unit :accessor scale-to-unit-of :initform nil)
   (color :accessor color-of :initarg :color :initform (list 255 255 255))
   (texture :initform nil :accessor texture-of)
   (texture-number :initform nil :accessor texture-number-of)
   (character-hash :initform (make-hash-table) :accessor character-hash-of)))

(defun ceiling-power-of-two (number)
  (expt 2 (ceiling (log number 2))))

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

(defgeneric flush-texture (gl-text &key new-texture-array)
  (:method ((gl-text opengl-text) &key (new-texture-array nil))
    ;; not the most efficient method
    (let ((chars (hash-table-keys (character-hash-of gl-text)))
	  (em (emsquare-of gl-text)))
      (when chars
	(setf (character-hash-of gl-text) (make-hash-table))
	(if new-texture-array
	    (setf (texture-of gl-text) (make-ffa (list em
						       (if *coerce-em-to-power-of-two*
							   (ceiling-power-of-two (* em (length chars)))
							  (* em (length chars)))
						       4)
						 :uint8))
	    (let ((vec (find-original-array (texture-of gl-text))))
	     (iter (for i index-of-vector vec)
		   (setf (aref vec i) 0))))
	(mapc (rcurry #'get-char-texture-coords gl-text) chars)))))

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
	    (setf (texture-of gl-text) (make-ffa (list em
						       (if *coerce-em-to-power-of-two*
							   (ceiling-power-of-two (* em (length chars)))
							   (* em (length chars)))
						       4)
						 :uint8))
	    (when chars-loaded
	      (map-subarray texture (texture-of gl-text)
			    :target-range `(:all (0 ,(1- (array-dimension texture 1))) :all)))
	    (map nil (rcurry #'get-char-texture-coords gl-text) more-chars)))))))

(defun create-char-path (char shift gl-text)
  (let ((bb (zpb-ttf:bounding-box (font-loader-of gl-text)))
	(bb-glyph (zpb-ttf:bounding-box (zpb-ttf:find-glyph char (font-loader-of gl-text))))
	(em (1- (emsquare-of gl-text)))
	(font (font-loader-of gl-text)))
    (let ((scaler (max (- (zpb-ttf:xmax bb)
			  (zpb-ttf:xmin bb))
		       (- (zpb-ttf:ymax bb)
			  (zpb-ttf:ymin bb))))
	  (scale-x (- (zpb-ttf:xmax bb-glyph) (zpb-ttf:xmin bb-glyph)))
	  (scale-y (- (zpb-ttf:ymax bb-glyph) (zpb-ttf:ymin bb-glyph))))
      (setf (scaler-of gl-text) scaler)
      (setf (scale-to-unit-of gl-text) (/ scaler (zpb-ttf:units/em font)))
      (paths-ttf:paths-from-glyph (zpb-ttf:find-glyph char font)
				  :offset (paths:make-point (+ shift
							       (* em
								  (- (/ (zpb-ttf:xmin bb-glyph) scale-x))))
							    (+ (1+ em)
							       (* em
								  (/ (zpb-ttf:ymin bb-glyph) scale-y))))
				  :scale-x (/ em scale-x)
				  :scale-y (- (/ em scale-y))))))

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

(defun draw-char-on (char tex-array shift gl-text)
  (let ((char-path (create-char-path char shift gl-text))
	(aa-state (aa:make-state))
	(h (array-dimension tex-array 0)))
    (destructuring-bind (r g b) (color-of gl-text)
      (flet ((draw-function (x y alpha)
	       (if (array-in-bounds-p tex-array (- h y) x 0)
		   (setf (aref tex-array (- h y) x 0) r
			 (aref tex-array (- h y) x 1) g
			 (aref tex-array (- h y) x 2) b
			 (aref tex-array (- h y) x 3) (clamp alpha 0 255))
		   (warn "Out of bounds: ~a ~a" (- h y) x))))
	(aa:cells-sweep (vectors:update-state aa-state char-path) #'draw-function)))))

(defun send-texture (new-texture gl-text)
  (when *opengl-active*
    (with-pointer-to-array (new-texture tex-pointer
					:uint8
					(reduce #'* (array-dimensions new-texture))
					:copy-in)
      (if (texture-number-of gl-text)
	  (cl-opengl:bind-texture :texture-2d (texture-number-of gl-text))
	  (let ((new-number (car (cl-opengl:gen-textures 1))))
	    (setf (texture-number-of gl-text) new-number)
	    (cl-opengl:bind-texture :texture-2d new-number)
	    (trivial-garbage:finalize gl-text #'(lambda ()
						  (gl:delete-textures (list new-number))))))
      (cl-opengl:tex-image-2d :texture-2d 0 :rgba (array-dimension new-texture 1)
			      (array-dimension new-texture 0) 0 :rgba :unsigned-byte tex-pointer))))

(defun old-chars-reinsert-add-new (charh new-char new-count new-count-ext)
  (let ((old-chars (sort (hash-table-alist charh)
			 #'< :key #'(lambda (k)
				      (aref (cdr k) 0 0))))
	(new-charh (make-hash-table)))
    (iter (for (old-char . nil) in old-chars)
	  (for i from 0)
	  (setf (gethash old-char new-charh)
		(make-array '(4 2)
			    :initial-contents
			    (list (list (float (/ i new-count-ext)) 0.0)
				  (list (float (/ (1+ i) new-count-ext)) 0.0)
				  (list (float (/ (1+ i) new-count-ext)) 1.0)
				  (list (float (/ i new-count-ext)) 1.0)))))
    (setf (gethash new-char new-charh)
	  (make-array '(4 2)
		      :initial-contents
		      (list (list (float (/ (1- new-count) new-count-ext)) 0.0)
			    (list (float (/ new-count new-count-ext)) 0.0)
			    (list (float (/ new-count new-count-ext)) 1.0)
			    (list (float (/ (1- new-count) new-count-ext)) 1.0))))
    new-charh))

(defgeneric add-char (char gl-text)
  (:method ((char character) (gl-text opengl-text))
    (let ((charh (character-hash-of gl-text))
	  (em (emsquare-of gl-text)))
      (let ((new-count (1+ (hash-table-count charh))))
	(let ((new-size (if *coerce-em-to-power-of-two*
			    (ceiling-power-of-two (* em new-count))
			    (* em new-count))))
	  (let ((new-texture (if (or (null (texture-of gl-text))
				     (> new-size (array-dimension (texture-of gl-text) 1)))
				 (make-ffa (list em new-size 4) :uint8)
				 (texture-of gl-text)))
		(new-count-ext (/ new-size em)))
	    (when (and (texture-of gl-text)
		       (not (eq (texture-of gl-text) new-texture)))
	      (map-subarray (texture-of gl-text) new-texture
			    :target-range `(:all (0 ,(1- (array-dimension (texture-of gl-text) 1))) :all)))
	    (draw-char-on char new-texture (* em (hash-table-count charh)) gl-text)
	    (setf (character-hash-of gl-text)
		  (old-chars-reinsert-add-new charh char new-count new-count-ext))
	    (setf (texture-of gl-text) new-texture)
	    (send-texture new-texture gl-text)
	    (gethash char (character-hash-of gl-text))))))))

(defgeneric get-char-texture-coords (char gl-text)
  (:method ((char character) (gl-text opengl-text))
    (let ((char-coords (gethash char (character-hash-of gl-text))))
      (if char-coords
	  char-coords
	  (add-char char gl-text)))))

(defun generate-vertices (vertices tex-coords string gl-text kerning depth-shift)
  (iter (with font = (font-loader-of gl-text))
	(with scaler = (scaler-of gl-text))
	(for c in-string string)
	(for g next (zpb-ttf:find-glyph c font))
	(for gp previous g initially nil)
	(for i from 0 by 4)
	(for j from 0)
	(when (and gp kerning)
	  (incf k (/ (zpb-ttf:kerning-offset gp g font) scaler)))
	(for (xmin ymin xmax ymax) next (compute-actual-slice c gl-text))
	(let ((vertex (make-array '(4 3)
				  :initial-contents
				  (list (list (+ k xmin) ymin (* j depth-shift))
					(list (- (1+ k) (- 1 xmax)) ymin (* j depth-shift))
					(list (- (1+ k) (- 1 xmax)) ymax (* j depth-shift))
					(list (+ k xmin) ymax (* j depth-shift)))))
	      (tex-coord (get-char-texture-coords c gl-text)))
	  (map-subarray vertex vertices :target-range `((,i ,(+ i 3)) :all))
	  (map-subarray tex-coord tex-coords :target-range `((,i ,(+ i 3)) :all)))
	(sum (/ (+ (zpb-ttf:advance-width g)) scaler) into k)))

(defgeneric draw-gl-string (string gl-text &key kerning depth-shift)
  (:method ((string string) (gl-text opengl-text) &key (kerning t) (depth-shift 0.0))
    (ensure-characters (remove-duplicates string) gl-text)
    (let ((l (length string)))
     (let ((vertices (make-ffa (list (* 4 l) 3) :float))
	   (tex-coords (make-ffa (list (* 4 l) 2) :float)))
       (generate-vertices vertices tex-coords string gl-text kerning depth-shift)
       (with-pointers-to-arrays ((vertices v-pointer :float (length (find-original-array vertices)) :copy-in)
				 (tex-coords t-pointer :float (length (find-original-array tex-coords)) :copy-in))
	 (%gl:vertex-pointer 3 :float 0 v-pointer)
	 (%gl:tex-coord-pointer 2 :float 0 t-pointer)
	 (gl:bind-texture :texture-2d (texture-number-of gl-text))
	 (gl:tex-env :texture-env :texture-env-mode :replace)
	 (gl:tex-parameter :texture-2d :texture-min-filter :linear)
	 (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
	 (gl:with-pushed-matrix
	   (gl:scale (scale-to-unit-of gl-text)
		     (scale-to-unit-of gl-text)
		     1.0)
	   (let ((font (font-loader-of gl-text)))
	    (gl:translate (/ (- (zpb-ttf:xmin (zpb-ttf:bounding-box font))
				(zpb-ttf:xmin (zpb-ttf:bounding-box (zpb-ttf:find-glyph (char string 0) font))))
			     (scaler-of gl-text))
			  (/ (zpb-ttf:descender font) (scaler-of gl-text))
			  0))
	   (gl:draw-arrays :quads 0 (* 4 (length string)))))))))