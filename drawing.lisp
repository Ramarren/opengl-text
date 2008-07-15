(in-package :opengl-text)

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
	#+(or)(let ((vertex (make-array '(4 3)
					:initial-contents
					(list (list (+ k xmin) ymin (* j depth-shift))
					      (list (- (1+ k) (- 1 xmax)) ymin (* j depth-shift))
					      (list (- (1+ k) (- 1 xmax)) ymax (* j depth-shift))
					      (list (+ k xmin) ymax (* j depth-shift)))))
		    (tex-coord (get-char-texture-coords c gl-text)))
		(map-subarray vertex vertices :target-range `((,i ,(+ i 3)) :all))
		(map-subarray tex-coord tex-coords :target-range `((,i ,(+ i 3)) :all)))
	(setf (aref vertices i 0) (+ k xmin)
	      (aref vertices i 1) ymin
	      (aref vertices i 2) (* j depth-shift)
	      (aref vertices (+ i 1) 0) (- (1+ k) (- 1 xmax))
	      (aref vertices (+ i 1) 1) ymin
	      (aref vertices (+ i 1) 2) (* j depth-shift)
	      (aref vertices (+ i 2) 0) (- (1+ k) (- 1 xmax))
	      (aref vertices (+ i 2) 1) ymax
	      (aref vertices (+ i 2) 2) (* j depth-shift)
	      (aref vertices (+ i 3) 0) (+ k xmin)
	      (aref vertices (+ i 3) 1) ymax
	      (aref vertices (+ i 3) 2) (* j depth-shift))
	(let ((tex-coord (get-char-texture-coords c gl-text)))
	 (iter (for ii from i to (+ i 3))
	       (for k from 0)
	       (setf (aref tex-coords ii 0) (aref tex-coord k 0)
		     (aref tex-coords ii 1) (aref tex-coord k 1))))
	(sum (/ (+ (zpb-ttf:advance-width g)) scaler) into k)))

(defgeneric draw-gl-string (string gl-text &key kerning depth-shift vertices tex-coords color)
  (:method ((string string) (gl-text opengl-text) &key (kerning t) (depth-shift 0.0) (vertices nil) (tex-coords nil) (color '(1 1 1 1)))
    (ensure-characters (remove-duplicates string) gl-text)
    (let ((l (length string)))
     (let ((vertices (if vertices
			 vertices
			 (make-ffa (list (* 4 l) 3) :float)))
	   (tex-coords (if tex-coords
			   tex-coords
			   (make-ffa (list (* 4 l) 2) :float))))
       (generate-vertices vertices tex-coords string gl-text kerning depth-shift)
       (with-pointers-to-arrays ((vertices v-pointer :float (length (find-original-array vertices)) :copy-in)
				 (tex-coords t-pointer :float (length (find-original-array tex-coords)) :copy-in))
	 (%gl:vertex-pointer 3 :float 0 v-pointer)
	 (%gl:tex-coord-pointer 2 :float 0 t-pointer)
	 (gl:bind-texture :texture-2d (texture-number-of gl-text))
	 (gl:tex-env :texture-env :texture-env-mode :blend)
	 (gl:tex-env :texture-env :texture-env-color color)
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
