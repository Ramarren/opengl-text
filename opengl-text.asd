(asdf:defsystem opengl-text
  :description "Textured fonts for OpenGL"
  :licence "BSD-style"
  :depends-on (:cl-opengl :iterate :alexandria :ffa :trivial-garbage)
  :components ((:static-file "opengl-text.asd" :pathname "opengl-text.asd")
               (:file "package")
               (:file "opengl-text" :depends-on ("package"))
               (:module "texture"
                        :depends-on ("package" "opengl-text")
                        :components ((:file "texture")
                                     (:file "cell-texture")
                                     (:file "mipmaped" :depends-on ("package" "characters" "opengl-text" "drawing"))
                                     (:file "packed-mipmaps" :depends-on ("package" "characters" "opengl-text" "drawing"))))))
