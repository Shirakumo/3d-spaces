#|
 This file is a part of 3d-spaces
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.flare.space
  (:use #:cl #:org.shirakumo.flare.vector #:org.shirakumo.flare.matrix)
  (:local-nicknames
   (#:sequences #:org.shirakumo.trivial-extensible-sequences))
  (:export))

(in-package #:org.shirakumo.flare.space)

(defgeneric location (object))
(defgeneric bsize (object))
(defgeneric radius (object))
(defgeneric ensure-region (object &optional region))

(defgeneric check (container))
(defgeneric clear (container))
(defgeneric reoptimize (container))
(defgeneric enter (object container))
(defgeneric leave (object container))
(defgeneric update (object container))
(defgeneric call-with-contained (function container region))

(defstruct (region
            (:include vec3)
            (:constructor %region (x y z bsize))
            (:predicate NIL)
            (:copier NIL))
  (bsize (vec 0 0 0) :type vec3))

(defmethod print-object ((object region) stream)
  (let ((bsize (region-bsize object)))
    (prin1 (list 'region
                 (vx object) (vy object) (vz object)
                 (vx bsize) (vy bsize) (vz bsize))
           stream)))

(defmethod make-load-form ((region region) &optional environment)
  (declare (ignore environment))
  (let ((bsize (region-bsize object)))
    `(%region ,(vx object) ,(vy object) ,(vz object)
              ,(vx bsize) ,(vy bsize) ,(vz bsize))))

(declaim (inline region))
(defun region (x y z w h d)
  (%region x y z (vec w h d)))

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
       (declare (dynamic-extent bsize region))
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
                         (vy3 region) (vy2 location))))
           (let ((rbsize (region-bsize region)))
             (etypecase bsize
               (vec3 (v<- rbsize bsize))
               (vec2 (setf (vx3 rbsize) (vx2 bsize)
                           (vy3 rbsize) (vy2 bsize)))))
           region)
          ((vec3-p location)
           (%region (vx location) (vy location) (vz location) (vcopy bsize)))
          (T
           (%region (vy location) (vy location) 0 (vxy_ bsize))))))

(defmethod enter ((object sequences:sequence) container)
  (sequences:dosequence (child object)
    (enter child container)))

(defmethod leave ((object sequences:sequence) container)
  (sequences:dosequence (child object)
    (leave child container)))

(defmethod update ((object sequences:sequence) container)
  (sequences:dosequence (child object)
    (update child container)))

(defmacro do-contained ((element container region &optional result) &body body)
  (let ((thunk (gensym "THUNK"))
        (regiong (gensym "REGION")))
    `(flet ((,thunk (,element)
              ,@body))
       (declare (dynamic-extent #',thunk))
       (with-region (,regiong)
         (ensure-region ,region ,regiong)
         (call-with-contained #',thunk ,container ,regiong))
       ,result)))
