#|
 This file is a part of 3d-spaces
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.trial.space.test
  (:use #:cl #:parachute #:org.shirakumo.flare.vector)
  (:local-nicknames
   (#:space #:org.shirakumo.fraf.trial.space)
   (#:bvh2 #:org.shirakumo.fraf.trial.space.bvh2)
   (#:quadtree #:org.shirakumo.fraf.trial.space.quadtree)
   (#:grid3 #:org.shirakumo.fraf.trial.space.grid3))
  (:export
   #:test))

(in-package #:org.shirakumo.fraf.trial.space.test)

(define-test 3d-spaces)

(define-test 2d
  :parent 3d-spaces)

(define-test 3d
  :parent 3d-spaces)

(define-test bvh2
  :parent 2d
  (test-container-generic #'bvh2:make-bvh))

(define-test quadtree
  :parent 2d
  (test-container-generic #'quadtree:make-quadtree))

(define-test grid3
  :parent 3d
  (test-container-generic #'grid3:make-grid 10))

(defun test-container-generic (constructor &rest args)
  (flet ((construct ()
           (apply constructor args)))
    (of-type space:container (construct))
    (finish (space:check (construct)))
    (finish (space:clear (construct)))
    (finish (space:reoptimize (construct)))
    (finish (space:do-all (object (construct))
              (false object)))
    (finish (space:do-contained (object (construct) (space:region 0 0 0 0 0 0))
              (false object)))
    (finish (space:do-overlapping (object (construct) (space:region 0 0 0 0 0 0))
              (false object)))))
