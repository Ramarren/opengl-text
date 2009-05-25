(asdf:defsystem opengl-text
  :description "Textured fonts for OpenGL"
  :licence "BSD-style"
  :depends-on (:cl-opengl :iterate :alexandria :ffa :array-operations :trivial-garbage :cl-glu)
  :components ((:static-file "opengl-text.asd" :pathname "opengl-text.asd")
               (:file "package")
               (:module "texture"
                        :depends-on ("package")
                        :components ((:file "texture-base")
                                     (:file "texture" :depends-on ("cell-texture" "texture-base"))
                                     (:file "cell-texture")
                                     (:file "mipmapped" :depends-on ("texture" "cell-texture" "texture-base"))
                                     (:file "drawing" :depends-on ("texture-base"))))))
