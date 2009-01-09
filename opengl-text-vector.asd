(asdf:defsystem opengl-text-vector
  :description "Subsystem providing glyphs drawn with cl-vectors for opengl-text"
  :licence "BSD-style"
  :depends-on (:iterate :alexandria  :cl-paths-ttf :cl-vectors :opengl-text)
  :components ((:module "drawing"
                        :components ((:file "package")
                                     (:file "vector-gl-text" :depends-on ("characters"))
                                     (:file "characters" :depends-on ("package"))))))
