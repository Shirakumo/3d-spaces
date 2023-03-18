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
  :parent 3d-spaces
  (true (space:region-overlaps-p
         (box2 (vec 0 0) (vec 10 10))
         (space:region -10 -10 0 20 20 0)))
  (true (space:region-overlaps-p
         (box2 (vec -5 -5) (vec 10 10))
         (space:region -10 -10 0 20 20 0)))
  (false (space:region-overlaps-p
          (box2 (vec 30 30) (vec 10 10))
          (space:region -10 -10 0 20 20 0)))
  (true (space:region-contains-p
         (box2 (vec 0 0) (vec 10 10))
         (space:region -10 -10 0 20 20 0)))
  (false (space:region-contains-p
          (box2 (vec -5 -5) (vec 10 10))
          (space:region -10 -10 0 20 20 0)))
  (false (space:region-contains-p
          (box2 (vec 30 30) (vec 10 10))
          (space:region -10 -10 0 20 20 0))))

(define-test 3d
  :parent 3d-spaces
  (true (space:region-overlaps-p
         (box3 (vec 0 0 0) (vec 10 10 10))
         (space:region -10 -10 -10 20 20 20)))
  (true (space:region-overlaps-p
         (box3 (vec -5 -5 -5) (vec 10 10 10))
         (space:region -10 -10 -10 20 20 20)))
  (false (space:region-overlaps-p
          (box3 (vec 30 30 30) (vec 10 10 10))
          (space:region -10 -10 -10 20 20 20)))
  (true (space:region-contains-p
         (box3 (vec 0 0 0) (vec 10 10 10))
         (space:region -10 -10 -10 20 20 20)))
  (false (space:region-contains-p
          (box3 (vec -5 -5 -5) (vec 10 10 10))
          (space:region -10 -10 -10 20 20 20)))
  (false (space:region-contains-p
          (box3 (vec 30 30 30) (vec 10 10 10))
          (space:region -10 -10 -10 20 20 20))))

(define-test bvh2
  :parent 2d
  (test-container-generic #'bvh2:make-bvh #'box2))

(define-test quadtree
  :parent 2d
  (test-container-generic #'quadtree:make-quadtree #'box2))

(define-test grid3
  :parent 3d
  (test-container-generic (lambda () (grid3:make-grid 10)) #'box3))

(defclass box3 ()
  ((location :initarg :location :initform (vec 0 0 0) :accessor space:location)
   (bsize :initarg :bsize :initform (vec 0 0 0) :accessor space:bsize)))

(defmethod print-object ((box box3) stream)
  (prin1 (list 'box3 (space:location box) (space:bsize box)) stream))

(defun box3 (&optional (location (vec 0 0 0)) (bsize (vec 0 0 0)))
  (make-instance 'box3 :location location :bsize bsize))

(defclass box2 ()
  ((location :initarg :location :initform (vec 0 0) :accessor space:location)
   (bsize :initarg :bsize :initform (vec 0 0) :accessor space:bsize)))

(defmethod print-object ((box box2) stream)
  (prin1 (list 'box2 (space:location box) (space:bsize box)) stream))

(defun box2 (&optional (location (vec 0 0)) (bsize (vec 0 0)))
  (make-instance 'box2 :location (vxy location) :bsize (vxy bsize)))

(defun random* (min max)
  (+ min (random (- max min))))

(defun test-container-generic (constructor object-constructor)
  (flet ((make-container ()
           (funcall constructor))
         (make-object (&rest args)
           (apply object-constructor args)))
    (group (empty)
      (of-type space:container (make-container))
      (finish (space:check (make-container)))
      (finish (space:clear (make-container)))
      (finish (space:reoptimize (make-container)))
      (finish (space:do-all (object (make-container))
                (false object)))
      (finish (space:do-contained (object (make-container) (space:region 0 0 0 0 0 0))
                (false object)))
      (finish (space:do-overlapping (object (make-container) (space:region 0 0 0 0 0 0))
                (false object))))

    (group (single)
      (let ((container (make-container))
            (box (make-object)))
        (finish (space:enter box container))
        (finish (space:do-all (object container)
                  (is eq box object)))
        (finish (space:do-contained (object container box)
                  (is eq box object)))
        (finish (space:do-overlapping (object container box)
                  (is eq box object)))
        (finish (space:leave box container))
        (finish (space:do-all (object container)
                  (false object)))))

    (group (fixed)
      (let ((container (make-container))
            (a (make-object (vec 0 0 0) (vec 5 5 5)))
            (b (make-object (vec 15 0 0) (vec 5 5 5)))
            (c (make-object (vec 300 0 0) (vec 5 5 5))))
        (finish (space:enter (list a b c) container))
        (finish (space:do-all (object container)
                  (true (or (eq object a) (eq object b) (eq object c)))))
        (finish (space:do-overlapping (object container (space:region 0 0 0 10 10 10))
                  (false (eq object c))))))

    (group (randomized)
      (let ((container (make-container))
            (objects (loop repeat 10
                           collect (make-object (vrand (vec 0 0 0) 100) (vrand (vec 50 50 50) 100)))))
        (finish (space:enter objects container))
        (finish (space:do-all (object container)
                  (true (find object objects))))
        (finish (space:do-overlapping (object container (space:region -100 -100 -100 200 200 200))
                  (true (find object objects))))
        (loop repeat 20
              for region = (space:region (random* -100 100) (random* -100 100) (random* -100 100)
                                         (random* 0 200) (random* 0 200) (random* 0 200))
              do (space:do-contained (object container region)
                   (true (space:region-overlaps-p object region))))))))
