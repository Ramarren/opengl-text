(asdf:defsystem opengl-text-test
  :version "0"
  :description "Tests for opengl-text"
  :maintainer " <ramarren@cignet.higersbergernet>"
  :author " <ramarren@cignet.higersbergernet>"
  :licence "BSD-style"
  :depends-on (:opengl-text :sdl-cffi :cl-glu)
  :components ((:file "test")))

