#|
 This file is a part of 3d-spaces
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

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
(defgeneric call-with-intersecting (funciton container ray-origin ray-direction))

(defgeneric serialize (container file object->id))
(defgeneric deserialize (container file id->object))

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

(defmethod ensure-region ((object vec2) &optional region)
  (if region
      (let ((rsize (region-size region)))
        (setf (vx3 region) (vx2 object)
              (vy3 region) (vy2 object)
              (vz3 region) 0.0
              (vx3 rsize) 0.0
              (vy3 rsize) 0.0
              (vz3 rsize) 0.0)
        region)
      (ensure-region object (%region 0.0 0.0 0.0 (vec3 0.0 0.0 0.0)))))

(defmethod ensure-region ((object vec3) &optional region)
  (if region
      (let ((rsize (region-size region)))
        (setf (vx3 region) (vx3 object)
              (vy3 region) (vy3 object)
              (vz3 region) (vz3 object)
              (vx3 rsize) 0.0
              (vy3 rsize) 0.0
              (vz3 rsize) 0.0)
        region)
      (ensure-region object (%region 0.0 0.0 0.0 (vec3 0.0 0.0 0.0)))))

(defmethod check ((container container)))
(defmethod reoptimize ((container container) &key))

(defmethod enter ((object sequences:sequence) (container container))
  (sequences:dosequence (child object)
    (enter child container)))

(defmethod leave ((object sequences:sequence) (container container))
  (sequences:dosequence (child object)
    (leave child container)))

(defmethod update (object (container container))
  (leave object container)
  (enter object container))

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
    `(block NIL
       (flet ((,thunk (,element)
                ,@body))
         (declare (dynamic-extent #',thunk))
         (call-with-all #',thunk ,container)
         ,result))))

(defmacro do-contained ((element container region &optional result) &body body)
  (let ((thunk (gensym "THUNK"))
        (regiong (gensym "REGION")))
    `(with-region (,regiong)
       (ensure-region ,region ,regiong)
       (block NIL
         (flet ((,thunk (,element)
                  ,@body))
           (declare (dynamic-extent #',thunk))
           (call-with-contained #',thunk ,container ,regiong)
           ,result)))))

(defmacro do-overlapping ((element container region &optional result) &body body)
  (let ((thunk (gensym "THUNK"))
        (regiong (gensym "REGION")))
    `(with-region (,regiong)
       (ensure-region ,region ,regiong)
       (block NIL
         (flet ((,thunk (,element)
                  ,@body))
           (declare (dynamic-extent #',thunk))
           (call-with-overlapping #',thunk ,container ,regiong)
           ,result)))))

(defmacro do-intersecting ((element container ray-origin ray-direction &optional result) &body body)
  (let ((thunk (gensym "THUNK")))
    `(block NIL
       (flet ((,thunk (,element)
                ,@body))
         (declare (dynamic-extent #',thunk))
         (call-with-intersecting #',thunk ,container ,ray-origin ,ray-direction)
         ,result))))

(defun find-region (objects)
  (let ((x- most-positive-single-float)
        (x+ most-negative-single-float)
        (y- most-positive-single-float)
        (y+ most-negative-single-float)
        (z- most-positive-single-float)
        (z+ most-negative-single-float))
    (flet ((expand (loc bs)
             (etypecase loc
               (vec3
                (setf x- (min x- (- (vx3 loc) (vx3 bs))))
                (setf x+ (max x+ (+ (vx3 loc) (vx3 bs))))
                (setf y- (min y- (- (vy3 loc) (vy3 bs))))
                (setf y+ (max y+ (+ (vy3 loc) (vy3 bs))))
                (setf z- (min z- (- (vz3 loc) (vz3 bs))))
                (setf z+ (max z+ (+ (vz3 loc) (vz3 bs)))))
               (vec2
                (setf x- (min x- (- (vx2 loc) (vx2 bs))))
                (setf x+ (max x+ (+ (vx2 loc) (vx2 bs))))
                (setf y- (min y- (- (vy2 loc) (vy2 bs))))
                (setf y+ (max y+ (+ (vy2 loc) (vy2 bs))))))))
      (sequences:dosequence (object objects)
        (expand (location object) (bsize object)))
      (when (= x- most-positive-single-float)
        (setf x- 0.0 x+ 0.0))
      (when (= y- most-positive-single-float)
        (setf y- 0.0 y+ 0.0))
      (when (= z- most-positive-single-float)
        (setf z- 0.0 z+ 0.0)))
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
         (and (<= (abs (- (vx2 ol) (vx2 rl))) (- (* 0.5 (vx3 s)) (vx2 ob)))
              (<= (abs (- (vy2 ol) (vy2 rl))) (- (* 0.5 (vy3 s)) (vx2 ob)))))))))

(defmethod serialize ((container container) file (object->id symbol))
  (serialize container file (fdefinition object->id)))

(defmethod deserialize ((container container) file (id->object symbol))
  (deserialize container file (fdefinition id->object)))

(defmethod serialize ((container container) (file string) object->id)
  (serialize container (parse-namestring file) object->id))

(defmethod dserialize ((container container) (file string) id->object)
  (deserialize container (parse-namestring file) id->object))

(defmethod serialize ((container container) (file pathname) object->id)
  (with-open-file (stream file :direction :output
                               :if-exists :supersede
                               :element-type '(unsigned-byte 8))
    (serialize container stream object->id)))

(defmethod deserialize ((container container) (file pathname) id->object)
  (with-open-file (stream file :direction :input
                               :if-exists :supersede
                               :element-type '(unsigned-byte 8))
    (deserialize container stream id->object)))
