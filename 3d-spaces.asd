#|
 This file is a part of 3d-spaces
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem 3d-spaces
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A library implementing spatial query structures"
  :homepage "https://shirakumo.github.io/3d-spaces/"
  :bug-tracker "https://github.com/shirakumo/3d-spaces/issues"
  :source-control (:git "https://github.com/shirakumo/3d-spaces.git")
  :serial T
  :components ((:file "package")
               (:file "protocol")
               (:file "bvh2")
               (:file "quadtree")
               (:file "grid3")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :3d-vectors
               :3d-matrices
               :trivial-extensible-sequences
               :for)
  :in-order-to ((asdf:test-op (asdf:test-op :3d-spaces-test))))
