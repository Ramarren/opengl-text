(asdf:defsystem opengl-text
  :description "Textured fonts for OpenGL"
  :licence "BSD-style"
  :depends-on (:cl-opengl :iterate :alexandria :cl-paths-ttf :cl-vectors :ffa :trivial-garbage :cl-glu)
  :components ((:static-file "opengl-text.asd" :pathname "opengl-text.asd")
               (:file "package")
               (:file "opengl-text" :depends-on ("package"))
               (:file "texture" :depends-on ("package" "opengl-text"))
               (:file "cell-texture" :depends-on ("package"))
               (:file "characters" :depends-on ("package" "opengl-text" "texture"))
               (:file "drawing" :depends-on ("package" "texture" "characters" "opengl-text"))
               (:file "mipmaped" :depends-on ("package" "characters" "opengl-text" "drawing"))
               (:file "packed-mipmaps" :depends-on ("package" "characters" "opengl-text" "drawing"))
               (:file "polygonal" :depends-on ("package" "characters" "drawing" "opengl-text"));for generic functions
               (:file "outline" :depends-on ("polygonal"))
               (:file "tesselate" :depends-on ("package" "polygonal"))))

