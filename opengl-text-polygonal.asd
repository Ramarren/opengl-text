(asdf:defsystem opengl-text-vector
  :description "Subsystem providing polygonal opengl text."
  :licence "BSD-style"
  :depends-on (:iterate :alexandria  :cl-paths-ttf :cl-vectors :ffa :cl-glu :opengl-text)
  :components ((:module "polygonal"
                        :components ((:file "package")
                                     (:file "polygonal" :depends-on ("package" "characters" "drawing" "opengl-text"));for generic functions
                                     (:file "outline" :depends-on ("polygonal"))
                                     (:file "tesselate" :depends-on ("package" "polygonal"))))))