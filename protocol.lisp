#|
 This file is a part of 3d-spaces
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.trial.space
  (:use #:cl #:org.shirakumo.flare.vector #:org.shirakumo.flare.matrix)
  (:local-nicknames
   (#:sequences #:org.shirakumo.trivial-extensible-sequences))
  (:export
   #:location
   #:bsize
   #:radius
   #:ensure-region
   #:check
   #:clear
   #:reoptimize
   #:enter
   #:leave
   #:update
   #:call-with-all
   #:call-with-contained
   #:call-with-overlapping
   #:container
   #:container-p
   #:region
   #:region-size
   #:do-all
   #:do-contained
   #:do-overlapping
   #:find-region
   #:region-overlaps-p
   #:region-contains-p))

(in-package #:org.shirakumo.fraf.trial.space)

(defgeneric location (object))
(defgeneric bsize (object))
(defgeneric radius (object))
(defgeneric ensure-region (object &optional region))

(defgeneric check (container))
(defgeneric clear (container))
(defgeneric reoptimize (container &key))
(defgeneric enter (object container))
(defgeneric leave (object container))
(defgeneric update (object container))
(defgeneric call-with-all (function container))
(defgeneric call-with-contained (function container region))
(defgeneric call-with-overlapping (function container region))

(defstruct (container
            (:constructor NIL)
            (:copier NIL)))

(declaim (inline %region))
(defstruct (region
            (:include vec3)
            (:constructor %region (3d-vectors::%vx3 3d-vectors::%vy3 3d-vectors::%vz3 size))
            (:predicate NIL)
            (:copier NIL))
  (size NIL :type vec3))

(defmethod print-object ((object region) stream)
  (let ((size (region-size object)))
    (prin1 (list 'region
                 (vx object) (vy object) (vz object)
                 (vx size) (vy size) (vz size))
           stream)))

(defmethod make-load-form ((region region) &optional environment)
  (declare (ignore environment))
  (let ((size (region-size region)))
    `(%region ,(vx region) ,(vy region) ,(vz region)
              ,(vx size) ,(vy size) ,(vz size))))

(declaim (inline region))
(defun region (x y z w h d)
  (%region (float x 0f0) (float y 0f0) (float z 0f0) (vec w h d)))

(defmethod location ((region region))
  (nv+ (bsize region) region))

(defmethod bsize ((region region))
  (v* (region-size region) 0.5))

(defmethod ensure-region ((object region) &optional region)
  (cond (region
         (v<- (region-size region) (region-size region))
         (v<- region object))
        (T
         object)))

(defmacro with-region ((var) &body body)
  (let ((size (gensym "SIZE"))
        (region (gensym "REGION")))
    `(let* ((,size (3d-vectors::%vec3 0.0 0.0 0.0))
            (,region (%region 0.0 0.0 0.0 ,size)))
       (declare (dynamic-extent ,size ,region))
       (let ((,var ,region))
         ,@body))))

(defmethod radius (object)
  (vlength (bsize object)))

(defmethod ensure-region (object &optional region)
  (let ((location (location object))
        (bsize (bsize object)))
    (cond (region
           (etypecase location
             (vec3 (setf (vx3 region) (- (vx3 location) (vx3 bsize))
                         (vy3 region) (- (vy3 location) (vy3 bsize))
                         (vz3 region) (- (vz3 location) (vz3 bsize))))
             (vec2 (setf (vx3 region) (- (vx2 location) (vx2 bsize))
                         (vy3 region) (- (vy2 location) (vy2 bsize))
                         (vz3 region) 0.0)))
           (let ((rsize (region-size region)))
             (etypecase bsize
               (vec3 (setf (vx3 rsize) (* 2.0 (vx3 bsize))
                           (vy3 rsize) (* 2.0 (vy3 bsize))
                           (vz3 rsize) (* 2.0 (vz3 bsize))))
               (vec2 (setf (vx3 rsize) (* 2.0 (vx2 bsize))
                           (vy3 rsize) (* 2.0 (vy2 bsize))
                           (vz3 rsize) 0.0))))
           region)
          (T
           (ensure-region object (%region 0.0 0.0 0.0 (vec3 0.0 0.0 0.0)))))))

(defmethod check ((container container)))
(defmethod reoptimize ((container container) &key))

(defmethod enter ((object sequences:sequence) (container container))
  (sequences:dosequence (child object)
    (enter child container)))

(defmethod leave ((object sequences:sequence) (container container))
  (sequences:dosequence (child object)
    (leave child container)))

(defmethod update ((object sequences:sequence) (container container))
  (sequences:dosequence (child object)
    (update child container)))

(defmethod call-with-contained (function (container container) thing)
  (with-region (region)
    (ensure-region thing region)
    (call-with-contained function container region)))

(defmethod call-with-overlapping (function (container container) thing)
  (with-region (region)
    (ensure-region thing region)
    (call-with-overlapping function container region)))

(defmacro do-all ((element container &optional result) &body body)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk (,element)
              ,@body))
       (declare (dynamic-extent #',thunk))
       (block NIL
         (call-with-all #',thunk ,container)
         ,result))))

(defmacro do-contained ((element container region &optional result) &body body)
  (let ((thunk (gensym "THUNK"))
        (regiong (gensym "REGION")))
    `(flet ((,thunk (,element)
              ,@body))
       (declare (dynamic-extent #',thunk))
       (with-region (,regiong)
         (ensure-region ,region ,regiong)
         (block NIL
           (call-with-contained #',thunk ,container ,regiong)
           ,result)))))

(defmacro do-overlapping ((element container region &optional result) &body body)
  (let ((thunk (gensym "THUNK"))
        (regiong (gensym "REGION")))
    `(flet ((,thunk (,element)
              ,@body))
       (declare (dynamic-extent #',thunk))
       (with-region (,regiong)
         (ensure-region ,region ,regiong)
         (block NIL
           (call-with-overlapping #',thunk ,container ,regiong)
           ,result)))))

(defun find-region (objects)
  (let ((x- most-positive-single-float)
        (x+ most-negative-single-float)
        (y- most-positive-single-float)
        (y+ most-negative-single-float)
        (z- most-positive-single-float)
        (z+ most-negative-single-float))
    (flet ((expand (loc bs)
             (setf x- (min x- (- (vx loc) (vx bs))))
             (setf x+ (max x+ (+ (vx loc) (vx bs))))
             (setf y- (min y- (- (vy loc) (vy bs))))
             (setf y+ (max y+ (+ (vy loc) (vy bs))))
             (setf z- (min z- (- (vz loc) (vz bs))))
             (setf z+ (max z+ (+ (vz loc) (vz bs))))))
      (sequences:dosequence (object objects)
        (expand (location object) (bsize object)))
      (when (= x- most-positive-single-float)
        (setf x- 0.0 x+ 0.0 y- 0.0 y+ 0.0 z- 0.0 z+ 0.0)))
    (%region x- y- z- (vec (- x+ x-) (- y+ y-) (- z+ z-)))))

(declaim (inline region-overlaps-p))
(defun region-overlaps-p (object region)
  (declare (optimize speed))
  (let ((ol (location object))
        (ob (bsize object))
        (s (region-size region)))
    (etypecase ol
      (vec3
       (let ((rl (vec3 (+ (vx3 region) (* 0.5 (vx3 s)))
                       (+ (vy3 region) (* 0.5 (vy3 s)))
                       (+ (vz3 region) (* 0.5 (vz3 s))))))
         (declare (dynamic-extent rl))
         (and (<= (abs (- (vx3 ol) (vx3 rl))) (+ (* 0.5 (vx3 s)) (vx3 ob)))
              (<= (abs (- (vy3 ol) (vy3 rl))) (+ (* 0.5 (vy3 s)) (vy3 ob)))
              (<= (abs (- (vz3 ol) (vz3 rl))) (+ (* 0.5 (vz3 s)) (vz3 ob))))))
      (vec2
       (let ((rl (vec2 (+ (vx3 region) (* 0.5 (vx3 s)))
                       (+ (vy3 region) (* 0.5 (vy3 s))))))
         (declare (dynamic-extent rl))
         (and (<= (abs (- (vx2 ol) (vx2 rl))) (+ (* 0.5 (vx3 s)) (vx2 ob)))
              (<= (abs (- (vy2 ol) (vy2 rl))) (+ (* 0.5 (vy3 s)) (vy2 ob)))))))))

(declaim (inline region-contains-p))
(defun region-contains-p (object region)
  (declare (optimize speed))
  (let ((ol (location object))
        (ob (bsize object))
        (s (region-size region)))
    (etypecase ol
      (vec3
       (let ((rl (vec3 (+ (vx3 region) (* 0.5 (vx3 s)))
                       (+ (vy3 region) (* 0.5 (vy3 s)))
                       (+ (vz3 region) (* 0.5 (vz3 s))))))
         (declare (dynamic-extent rl))
         (and (<= (abs (- (vx3 ol) (vx3 rl))) (- (* 0.5 (vx3 s)) (vx3 ob)))
              (<= (abs (- (vy3 ol) (vy3 rl))) (- (* 0.5 (vy3 s)) (vy3 ob)))
              (<= (abs (- (vz3 ol) (vz3 rl))) (- (* 0.5 (vz3 s)) (vz3 ob))))))
      (vec2
       (let ((rl (vec2 (+ (vx3 region) (* 0.5 (vx3 s)))
                       (+ (vy3 region) (* 0.5 (vy3 s))))))
         (declare (dynamic-extent rl))
         (print rl)
         (and (<= (abs (- (vx2 ol) (vx2 rl))) (- (* 0.5 (vx3 s)) (vx2 ob)))
              (<= (abs (- (vy2 ol) (vy2 rl))) (- (* 0.5 (vy3 s)) (vx2 ob)))))))))
