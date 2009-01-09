(in-package :opengl-text)

(defclass polygonal-opengl-text ()
  ((font-loader               :accessor font-loader-of               :initarg :font)
   (bezier-distance-tolerance :accessor bezier-distance-tolerance-of :initarg :bezier-distance-tolerance :initform 0.5)
   (bezier-angle-tolerance    :accessor bezier-angle-tolerance-of    :initarg :bezier-angle-tolerance :initform 0.05)
   (vertices                  :accessor vertices-of                  :initarg :vertices                  :initform (make-ffa 0 :double))
   (character-hash            :accessor character-hash-of            :initarg :character-hash            :initform (make-hash-table))))

(defgeneric retesselate (gl-text)
  (:method ((gl-text polygonal-opengl-text))
   (let ((chars (hash-table-keys (character-hash-of gl-text))))
     (setf (character-hash-of gl-text) (make-hash-table))
     (setf (vertices-of gl-text) (make-ffa 0 :double))
     (ensure-characters chars gl-text))))

(defmethod shared-initialize :after ((instance polygonal-opengl-text) slot-names &rest initargs)
  (declare (ignore initargs))
  (when (or (eql slot-names t)
            (member 'font-loader slot-names))
    (setf (font-loader-of instance) (font-loader-of instance))))

(defmethod (setf font-loader-of) :around ((new-value string) (object polygonal-opengl-text))
  (setf (font-loader-of object) (get-font-loader new-value)))

(defmethod (setf font-loader-of) :around ((new-value pathname) (object polygonal-opengl-text))
  (setf (font-loader-of object) (get-font-loader new-value)))

(defmethod (setf font-loader-of) :after ((new-value zpb-ttf::font-loader) (object polygonal-opengl-text))
  (retesselate object))

(defmethod (setf bezier-distance-tolerance-of) :after (new-value (object polygonal-opengl-text))
  (retesselate object))

(defmethod (setf bezier-angle-tolerance-of) :after (new-value (object polygonal-opengl-text))
  (retesselate object))

(defclass polygonal-glyph ()
  ((character :accessor character-of :initarg :character)
   (glyph     :accessor glyph-of     :initarg :glyph)
   (start     :accessor start-of     :initarg :start)
   (count     :accessor count-of     :initarg :count)))

(defgeneric tesselate-character (char gl-text)
    (:documentation "Take a character and a font and return a FFA of vertices."))

(defgeneric add-polygonal-character (character gl-text)
  (:method ((character character) (gl-text polygonal-opengl-text))
    (let ((paths:*bezier-distance-tolerance* (bezier-distance-tolerance-of gl-text))
          (paths:*bezier-angle-tolerance* (bezier-angle-tolerance-of gl-text)))
     (let ((char-ffa (tesselate-character character gl-text))
           (ver-ffa (vertices-of gl-text)))
       (let ((new-ffa (make-ffa (+ (length char-ffa) (length ver-ffa)) :double)))
         (setf (subseq new-ffa 0 (length ver-ffa)) ver-ffa)
         (setf (subseq new-ffa (length ver-ffa)) char-ffa)
         (setf (vertices-of gl-text) new-ffa)
         (setf (gethash character (character-hash-of gl-text))
               (make-instance 'polygonal-glyph
                              :character character
                              :glyph (zpb-ttf:find-glyph character (font-loader-of gl-text))
                              :start (/ (length ver-ffa) 3)
                              :count (/ (length char-ffa) 3))))))))

(defmethod ensure-characters ((characters sequence) (gl-text polygonal-opengl-text))
  (let ((more-chars (set-difference (coerce characters 'list) (hash-table-keys (character-hash-of gl-text)))))
    (when more-chars
      (mapc (rcurry #'add-polygonal-character gl-text) more-chars))))

(defmethod draw-gl-string ((string string) (gl-text polygonal-opengl-text) &key (kerning t) (depth-shift 0.0))
  (ensure-characters (remove-duplicates string) gl-text)
  (gl:disable :texture-2d)
  (let* ((vertices (vertices-of gl-text))
         (font (font-loader-of gl-text))
         (chash (character-hash-of gl-text))
         (scaler (zpb-ttf:units/em font)))
    (with-pointer-to-array (vertices vertex-pointer :double (length vertices) :copy-in)
      (%gl:vertex-pointer 3 :double 0 vertex-pointer)
      (gl:enable :polygon-smooth)       ;this doesn't seem to work
      (gl:hint :polygon-smooth-hint :nicest)
      (gl:enable :blend)
      (gl:disable :depth-test)
      (gl:blend-func :src-alpha-saturate :one)
      ;; so that string begins at 0,0,0
      (gl:with-pushed-matrix
       (gl:translate 0
                     (- (/ (zpb-ttf:descender font) scaler))
                     0)
       (labels ((draw-string ()
                  (gl:with-pushed-matrix
                    (iter
                      (for c in-string string)
                      (for polyglyph = (gethash c chash))
                      (for g = (glyph-of polyglyph))
                      (for gp previous g initially nil)
                      (when (and gp kerning)
                        (gl:translate (/ (zpb-ttf:kerning-offset gp g font) scaler) 0 0))
                      (gl:draw-arrays :triangles (start-of polyglyph) (count-of polyglyph))
                      (gl:translate (/ (+ (zpb-ttf:advance-width g)) scaler) 0 depth-shift)))))
         (draw-string))))))
