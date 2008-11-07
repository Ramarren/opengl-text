(asdf:defsystem opengl-text
  :description "Textured fonts for OpenGL"
  :licence "BSD-style"
  :depends-on (:cl-opengl :iterate :alexandria :cl-paths-ttf :cl-vectors :ffa :trivial-garbage :cl-glu)
  :components ((:static-file "opengl-text.asd" :pathname "opengl-text.asd")
               (:file "package")
               (:file "opengl-text" :depends-on ("package"))
               (:file "texture" :depends-on ("package" "opengl-text"))
               (:file "characters" :depends-on ("package" "opengl-text" "texture"))
               (:file "drawing" :depends-on ("package" "texture" "characters" "opengl-text"))
               (:file "polygonal" :depends-on ("package" "tesselate" "characters" "drawing" "opengl-text"));for generic functions
               (:file "tesselate" :depends-on ("package"))))

