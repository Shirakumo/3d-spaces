#|
 This file is a part of 3d-spaces
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem 3d-spaces-test
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Tests for the 3d-spaces system."
  :homepage "https://shirakumo.github.io/3d-spaces/"
  :bug-tracker "https://github.com/shirakumo/3d-spaces/issues"
  :source-control (:git "https://github.com/shirakumo/3d-spaces.git")
  :serial T
  :components ((:file "test"))
  :depends-on (:3d-spaces :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.fraf.trial.space.test)))
