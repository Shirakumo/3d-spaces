#|
 This file is a part of 3d-spaces
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.trial.space.grid3
  (:use #:cl #:org.shirakumo.flare.vector #:org.shirakumo.fraf.trial.space)
  (:export
   #:grid
   #:make-grid
   #:grid-resize
   #:grid-move
   #:grid-insert
   #:grid-remove
   #:grid-update))

(in-package #:org.shirakumo.fraf.trial.space.grid3)

(declaim (inline clamp))
(defun clamp (min x max)
  (max min (min x max)))

(defstruct (grid
            (:include container)
            (:constructor %make-grid (location cell))
            (:copier NIL)
            (:predicate NIL))
  (location (vec 0 0 0) :type vec3)
  (w 0 :type (unsigned-byte 32))
  (h 0 :type (unsigned-byte 32))
  (d 0 :type (unsigned-byte 32))
  (cell 0.0 :type single-float)
  (data #() :type simple-vector)
  (table (make-hash-table :test 'eq) :type hash-table))

(defmethod print-object ((grid grid) stream)
  (print-unreadable-object (grid stream :type T)
    (let ((c (grid-cell grid)))
      (format stream "~a x ~a x ~a @ ~a"
              (* c (grid-w grid)) (* c (grid-h grid)) (* c (grid-d grid))
              (grid-location grid)))))

(defun make-grid (cell-size &key (location (vec 0 0 0)) (bsize (vec 100 100 100)))
  (grid-resize (%make-grid location (float cell-size 0f0)) :bsize bsize))

(defun grid-resize (grid &key (bsize (bsize grid)) (cell-size (grid-cell grid)))
  (reoptimize grid :bsize bsize :cell-size cell-size :location (grid-location grid)))

(defun grid-move (grid location)
  (let ((old (make-array 0 :adjustable T :fill-pointer T)))
    (do-all (object grid)
      (vector-push-extend object old))
    (clear grid)
    (v<- (grid-location grid) location)
    (enter old grid)
    grid))

(defmacro %with-grid-coordinates ((x y z) (grid xv yv zv) &body body)
  `(let* ((gl (location ,grid))
          (c (grid-cell ,grid))
          (ch (* c 0.5))
          (w (grid-w ,grid))
          (h (grid-h ,grid))
          (d (grid-d ,grid))
          (,x (clamp 0 (the (signed-byte 32) (floor (+ (- ,xv (vx3 gl)) (* ch w)) w)) (1- w)))
          (,y (clamp 0 (the (signed-byte 32) (floor (+ (- ,yv (vy3 gl)) (* ch h)) h)) (1- h)))
          (,z (clamp 0 (the (signed-byte 32) (floor (+ (- ,zv (vz3 gl)) (* ch d)) d)) (1- d))))
     ,@body))

(defun grid-insert (object grid)
  (declare (optimize speed (safety 1)))
  ;; The grid is "bottom left" corner aligned, as are the cells we insert each object into.
  ;; We only insert an object into one cell, and instead broaden our search when we traverse.
  ;; This makes it a lot cheaper to keep track of where the object is, at the cost of only
  ;; allowing objects that are up to the size of a single grid cell. Users must be aware of
  ;; this.
  ;; 
  ;; Objects that are outside the grid will be clamped inside it. No automatic resizing of
  ;; the grid occurs, so the grid may become denormalised if the user does not take care of
  ;; properly sizing it for their use-case.
  (let* ((ol (location object))
         (os (bsize object)))
    (%with-grid-coordinates (x y z) (grid (- (vx3 ol) (vx3 os)) (- (vy3 ol) (vy3 os)) (- (vz3 ol) (vz3 os)))
      (let* ((y (the (unsigned-byte 32) (* y w)))
             (z (the (unsigned-byte 32) (* z w h)))
             (i (+ x y z)))
        (push object (aref (grid-data grid) i))
        (setf (gethash object (grid-table grid)) i)))))

(defun grid-remove (object grid)
  (declare (optimize speed (safety 1)))
  (let* ((data (grid-data grid))
         (table (grid-table grid))
         (i (gethash object table)))
    (etypecase i
      (null)
      ((unsigned-byte 32)
       (setf (aref data i) (delete object (the list (aref data i))))
       (remhash object table)))))

(defun grid-update (object grid)
  (grid-remove object grid)
  (grid-insert object grid))

(defmethod clear ((grid grid))
  (clrhash (grid-table grid))
  (loop with data = (grid-data grid)
        for i from 0 below (length data)
        do (setf (aref data i) ()))
  grid)

(defmethod reoptimize ((grid grid) &key location bsize cell-size)
  (let ((old (make-array 0 :adjustable T :fill-pointer T)))
    (do-all (object grid)
      (vector-push-extend object old))
    (clrhash (grid-table grid))
    (unless (and location bsize)
      (let ((region (find-region old)))
        (unless location (setf location (v- region (region-size region))))
        (unless bsize (setf bsize (v* (region-size region) 0.5)))))
    (let* ((c (grid-cell grid))
           (w (ceiling (* 2.0 (vx bsize)) c))
           (h (ceiling (* 2.0 (vy bsize)) c))
           (d (ceiling (* 2.0 (vz bsize)) c)))
      (setf (grid-w grid) w)
      (setf (grid-h grid) h)
      (setf (grid-d grid) d)
      (setf (grid-data grid) (make-array (* w h d) :initial-element ()))
      (when cell-size (setf (grid-cell grid) cell-size))
      (v<- (grid-location grid) location))
    (enter old grid)
    grid))

(defmethod enter (object (grid grid))
  (grid-insert object grid))

(defmethod leave (object (grid grid))
  (grid-remove object grid))

(defmethod update (object (grid grid))
  (grid-update object grid))

(defmethod location ((grid grid))
  (grid-location grid))

(defmethod bsize ((grid grid))
  (let ((c (* 0.5 (grid-cell grid))))
    (vec (* c (grid-w grid)) (* c (grid-h grid)) (* c (grid-d grid)))))

(defmethod call-with-all (function (grid grid))
  (loop for cell across (grid-data grid)
        do (loop for element in cell
                 do (funcall function element))))

(defmacro with-nesting (&body body)
  (when (rest body)
    (setf (cdr (last (first body)))
          `((with-nesting ,@(rest body)))))
  (first body))

(defmethod call-with-contained (function (grid grid) (region region))
  (declare (optimize speed (safety 1)))
  (let ((function (etypecase function
                    (symbol (fdefinition function))
                    (function function)))
        (size (region-size region))
        (data (grid-data grid)))
    (%with-grid-coordinates (x- y- z-) (grid (vx3 region) (vy3 region) (vz3 region))
      (%with-grid-coordinates (x+ y+ z+) (grid (+ (vx3 region) (vx3 size)) (+ (vy3 region) (vy3 size)) (+ (vz3 region) (vz3 size)))
        (with-nesting
          (loop for z from z- below z+
                for zi = (the (unsigned-byte 32) (* z w h))
                do)
          (loop for y from y- below y+
                for yi = (the (unsigned-byte 32) (* y w))
                do)
          (loop for x from x- below x+
                for i = (+ x yi zi)
                do)
          (loop for object in (aref data i)
                do (funcall function object)))))))

(defmethod call-with-overlapping (function (grid grid) (region region))
  (declare (optimize speed (safety 1)))
  (let ((function (etypecase function
                    (symbol (fdefinition function))
                    (function function)))
        (size (region-size region))
        (data (grid-data grid)))
    (%with-grid-coordinates (x- y- z-) (grid (vx3 region) (vy3 region) (vz3 region))
      (%with-grid-coordinates (x+ y+ z+) (grid (+ (vx3 region) (vx3 size)) (+ (vy3 region) (vy3 size)) (+ (vz3 region) (vz3 size)))
        ;; We expand the search cells by one to ensure we grab objects that overlap
        ;; into the space from outside, since we store bottom left corners only.
        (setf x- (max 0 (1- x-)))
        (setf y- (max 0 (1- y-)))
        (setf z- (max 0 (1- z-)))
        (with-nesting
          (loop for z from z- below z+
                for zi = (the (unsigned-byte 32) (* z w h))
                do)
          (loop for y from y- below y+
                for yi = (the (unsigned-byte 32) (* y w))
                do)
          (loop for x from x- below x+
                for i = (+ x yi zi)
                do)
          (loop for object in (aref data i)
                do (funcall function object)))))))
