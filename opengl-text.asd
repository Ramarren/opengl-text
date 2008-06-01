(asdf:defsystem opengl-text
  :description "Textured fonts for OpenGL"
  :licence "BSD-style"
  :depends-on (:cl-opengl :iterate :alexandria :cl-paths-ttf :cl-vectors :ffa :affi)
  :components ((:static-file "opengl-text.asd" :pathname "opengl-text.asd")
	       (:file "package")
	       (:file "opengl-text" :depends-on ("package"))))

