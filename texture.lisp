(in-package :opengl-text)

(defun character-cells (em array)
  (destructuring-bind (h w) (array-dimensions array)
    (* (/ h em) (/ w em))))

(defun cell-range (cell em array)
  "Return affi range for cell in array with emsquare em."
  (destructuring-bind (aw ah) (array-dimensions array)
    (let ((x (mod cell (/ aw em))))
      (let ((y (floor (/ cell (/ aw em)))))
	(assert (< x (/ aw em)))
	(assert (< y (/ ah em)))
	(list (list (* em x) (1- (* em (1+ x))))
	      (list (* em y) (1- (* em (1+ y)))))))))

(defun copy-character (source-array source-cell target-array target-cell em)
  (destructuring-bind ((sxmin sxmax) (symin symax)) (cell-range source-cell em source-array)
    (destructuring-bind ((txmin txmax) (tymin tymax)) (cell-range target-cell em target-array)
      (iter (for sx from sxmin to sxmax)
	    (for tx from txmin to txmax)
	    (iter (for sy from symin to symax)
		  (for ty from tymin to tymax)
		  (setf (aref target-array tx ty)
			(aref source-array sx sy)))))))

(defun make-new-texture-array (em len)
  (let ((h (maybe-ceiling-power-of-two (* em (ceiling (sqrt len))))))
    (make-ffa (list h
		    (maybe-ceiling-power-of-two (* em (ceiling (/ len (/ h em))))))
	      :uint8)))

(defgeneric size-texture (gl-text len &key preserve)
  (:documentation
   "Create or resize texture-of GL-TEXT large enough to hold LEN
    chars, possibly preserving its prior values.  Don't do anything
    if PRESERVE is true and the existing texture is large enough.")
  (:method ((gl-text opengl-text) (len integer) &key (preserve t))
    (let ((em (emsquare-of gl-text))
	  (old-tex (texture-of gl-text)))
      (unless (and preserve old-tex
		   (<= len (character-cells em old-tex)))
	(let ((new-texture (make-new-texture-array em len)))
	  (setf (texture-of gl-text) new-texture)
	  (when (and preserve old-tex)
	    (iter (for cell from 0 below (hash-table-count (character-hash-of gl-text)))
		  (copy-character old-tex cell new-texture cell em))
	    (setf (character-hash-of gl-text)
		  (chars-recoordinate (character-hash-of gl-text) new-texture em))))))))

(defgeneric flush-texture (gl-text &key new-texture-array)
  (:method ((gl-text opengl-text) &key (new-texture-array nil))
    ;; not the most efficient method
    (let ((chars (hash-table-keys (character-hash-of gl-text))))
      (when chars
	(setf (character-hash-of gl-text) (make-hash-table))
	(if new-texture-array
	    (size-texture gl-text (length chars) :preserve nil)
	    (let ((vec (find-original-array (texture-of gl-text))))
	     (iter (for i index-of-vector vec)
		   (setf (aref vec i) 0))))
	(ensure-characters chars gl-text)))))

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
      (cl-opengl:tex-image-2d :texture-2d 0 :intensity (array-dimension new-texture 1)
			      (array-dimension new-texture 0) 0 :luminance :unsigned-byte tex-pointer))))
