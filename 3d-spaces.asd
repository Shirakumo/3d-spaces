(asdf:defsystem 3d-spaces
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A library implementing spatial query structures"
  :homepage "https://shirakumo.github.io/3d-spaces/"
  :bug-tracker "https://github.com/shirakumo/3d-spaces/issues"
  :source-control (:git "https://github.com/shirakumo/3d-spaces.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "protocol")
               (:file "bvh2")
               (:file "quadtree")
               (:file "grid3")
               (:file "kd-tree")
               (:file "bsp")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :3d-math
               :text-draw
               :trivial-extensible-sequences
               :for
               :nibbles
               :babel)
  :in-order-to ((asdf:test-op (asdf:test-op :3d-spaces/test))))

(asdf:defsystem 3d-spaces/test
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Tests for the 3d-spaces system."
  :homepage "https://shirakumo.github.io/3d-spaces/"
  :bug-tracker "https://github.com/shirakumo/3d-spaces/issues"
  :source-control (:git "https://github.com/shirakumo/3d-spaces.git")
  :serial T
  :components ((:file "test") (:file "bsp-test"))
  :depends-on (:3d-spaces :parachute :cl-wavefront)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.fraf.trial.space.test)))
