(asdf:defsystem opengl-text-vector
  :description "Subsystem providing glyphs drawn with cl-vectors for opengl-text"
  :licence "BSD-style"
  :depends-on (:iterate :alexandria  :cl-paths-ttf :cl-vectors)
  :components ((:module "drawing"
                        :components ((:file "package")
                                     (:file "characters" :depends-on ("package" "opengl-text" "texture"))
                                     (:file "drawing" :depends-on ("package" "texture" "characters" "opengl-text"))))))