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
   #:region-bsize
   #:do-all
   #:do-contained
   #:do-overlapping
   #:find-centroid))

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
            (:constructor %region (3d-vectors::%vx3 3d-vectors::%vy3 3d-vectors::%vz3 bsize))
            (:predicate NIL)
            (:copier NIL))
  (bsize NIL :type vec3))

(defmethod print-object ((object region) stream)
  (let ((bsize (region-bsize object)))
    (prin1 (list 'region
                 (vx object) (vy object) (vz object)
                 (vx bsize) (vy bsize) (vz bsize))
           stream)))

(defmethod make-load-form ((region region) &optional environment)
  (declare (ignore environment))
  (let ((bsize (region-bsize region)))
    `(%region ,(vx region) ,(vy region) ,(vz region)
              ,(vx bsize) ,(vy bsize) ,(vz bsize))))

(declaim (inline region))
(defun region (x y z w h d)
  (%region (float x 0f0) (float y 0f0) (float z 0f0) (vec w h d)))

(defmethod location ((object region)) 
  object)

(defmethod bsize ((object region))
  (region-bsize object))

(defmethod ensure-region ((object region) &optional region)
  (cond (region
         (v<- (region-bsize region) (region-bsize region))
         (v<- region object))
        (T
         object)))

(defmacro with-region ((var) &body body)
  (let ((bsize (gensym "BSIZE"))
        (region (gensym "REGION")))
    `(let* ((,bsize (3d-vectors::%vec3 0.0 0.0 0.0))
            (,region (%region 0.0 0.0 0.0 ,bsize)))
       (declare (dynamic-extent ,bsize ,region))
       (let ((,var ,region))
         ,@body))))

(defmethod radius (object)
  (vlength (bsize object)))

(defmethod ensure-region (object &optional region)
  (let ((location (location object))
        (bsize (bsize object)))
    (cond (region
           (etypecase location
             (vec3 (v<- region location))
             (vec2 (setf (vx3 region) (vx2 location)
                         (vy3 region) (vy2 location)
                         (vz3 region) 0.0)))
           (let ((rbsize (region-bsize region)))
             (etypecase bsize
               (vec3 (v<- rbsize bsize))
               (vec2 (setf (vx3 rbsize) (vx2 bsize)
                           (vy3 rbsize) (vy2 bsize)
                           (vz3 rbsize) 0.0))))
           region)
          ((vec3-p location)
           (%region (vx location) (vy location) (vz location) (vcopy bsize)))
          (T
           (%region (vy location) (vy location) 0.0 (vxy_ bsize))))))

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

(defun find-centroid (objects)
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
    (let ((bsize (vec (* 0.5 (- x+ x-))
                      (* 0.5 (- y+ y-))
                      (* 0.5 (- z+ z-)))))
      (values (nv+ (vec x- y- z-) bsize)
              bsize))))
