#|
 This file is a part of 3d-spaces
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.space.kd-tree)

(declaim (inline sqrdist))
(defun sqrdist (a b)
  (declare (type (simple-array single-float (3)) a b))
  (+ (expt (- (aref a 0) (aref b 0)) 2)
     (expt (- (aref a 1) (aref b 1)) 2)
     (expt (- (aref a 2) (aref b 2)) 2)))

(defmacro with-array ((array vec) &body body)
  (let ((vecg (gensym "VEC")))
    `(let ((,array (make-array 3 :element-type 'single-float))
           (,vecg ,vec))
       (declare (dynamic-extent ,array))
       (etypecase ,vecg
         (vec2
          (setf (aref ,array 0) (vx2 ,vecg))
          (setf (aref ,array 1) (vy2 ,vecg)))
         (vec3
          (setf (aref ,array 0) (vx3 ,vecg))
          (setf (aref ,array 1) (vy3 ,vecg))
          (setf (aref ,array 2) (vz3 ,vecg))))
       ,@body)))

(defstruct (node
            (:constructor make-node ())
            (:copier NIL)
            (:predicate NIL))
  (near NIL :type (or null node))
  (far NIL :type (or null node))
  (children (make-array 0 :adjustable T :fill-pointer T) :type (and vector (not simple-vector)))
  (axis 0 :type (unsigned-byte 8))
  (position 0.0 :type single-float)
  (tree-depth 0 :type (unsigned-byte 8)))

(defstruct (kd-tree
            (:include container)
            (:constructor %make-kd-tree (dimensions split-size max-depth root))
            (:copier NIL)
            (:predicate NIL))
  (dimensions 0 :type (unsigned-byte 8))
  (split-size 0 :type (unsigned-byte 8))
  (max-depth 0 :type (unsigned-byte 8))
  (root NIL :type node))

(defun %visit-sphere (f node center radius volume)
  (declare (type (simple-array single-float (3)) center volume))
  (declare (type single-float radius))
  (declare (type function f))
  (declare (type node node))
  (let ((a (node-near node))
        (b (node-far node))
        (axis (node-axis node))
        (position (node-position node)))
    (funcall f node)
    (when (< position (aref center axis))
      (rotatef a b))
    (when a
      (%visit-sphere f a center radius volume))
    (when b
      (let ((old (shiftf (aref volume axis) position)))
        ;; If the splitting axis is within the radius, also check the other side
        (when (< (sqrdist volume center) (* radius radius))
          (%visit-sphere f b center radius volume))
        (setf (aref volume axis) old)))))

(defun %visit-bbox (f node center bsize volume)
  (declare (type (simple-array single-float (3)) center bsize volume))
  (declare (type function f))
  (declare (type node node))
  (let ((a (node-near node))
        (b (node-far node))
        (axis (node-axis node))
        (position (node-position node)))
    (funcall f node)
    (when (< position (aref center axis))
      (rotatef a b))
    (when a
      (%visit-bbox f a center bsize volume))
    (when b
      (let ((old (shiftf (aref volume axis) position)))
        ;; If the splitting axis is within the bounding box, also check the other side.
        (when (<= (aref bsize axis) (abs (- (aref center axis) position)))
          (%visit-bbox f b center bsize volume))
        (setf (aref volume axis) old)))))

(defun visit-sphere (f node center radius)
  (with-array (v center)
    (with-array (c center)
      (%visit-sphere f node c (float radius 0f0) v))))

(defun visit-bbox (f node center bsize)
  (with-array (v center)
    (with-array (c center)
      (with-array (b bsize)
        (%visit-bbox f node c b v)))))

(defun split-node-axis (node children axis other-axes)
  (let ((dim-value (ecase axis
                     (0 (lambda (o) (vx (location o))))
                     (1 (lambda (o) (vy (location o))))
                     (2 (lambda (o) (vz (location o)))))))
    (sort children #'< :key dim-value)
    (let* ((mid (truncate (length children) 2))
           (median (if (oddp (length children))
                       (funcall dim-value (aref children mid))
                       (* 0.5 (/ (funcall dim-value (aref children (+ mid 0)))
                                 (funcall dim-value (aref children (+ mid 1))))))))
      ;; Okey, now redistribute.
      (let ((n (make-node))
            (f (make-node))
            (here (node-children node)))
        (setf (fill-pointer here) 0)
        (loop for child across children
              for location = (funcall dim-value (location child))
              do (cond ((< (abs (- location median)) (funcall dim-value (bsize child)))
                        ;; We intersect the hyperplane, so keep it here.
                        (vector-push child here))
                       ((< location median)
                        ;; Insert into near
                        (vector-push child (node-children n)))
                       (T
                        ;; Insert into far
                        (vector-push-extend child (node-children f)))))
        (cond ((/= (length here) (length children))
               ;; We split successfully, actually modify the node now.
               (incf (node-tree-depth node))
               (setf (node-axis node) axis)
               (setf (node-position node) median)
               (setf (node-near node) n)
               (setf (node-far node) f)
               (setf (node-children node) here))
              (other-axes
               ;; We failed to split and arrived at the initial state. Try another axis.
               (split-node-axis node children (pop other-axes) other-axes))
              (T
               ;; No other axes available, mark node as stuck.
               (setf (node-position node) most-negative-single-float)))))))

(defun split-node (node dims split-size)
  ;; TODO: specialise on DIMS so we can use VX2 / VX3 etc
  (declare (type node node))
  (unless (= most-negative-single-float (node-position node))
    (let* ((children (node-children node))
           (children (make-array (max split-size (length children))
                                 :adjustable T :fill-pointer (length children)
                                 :initial-contents children))
           (min 0.0) (max 0.0) (max-range 0.0) (max-dim 0) (others ()))
      ;; Figure out widest spread axis
      (dotimes (axis dims)
        (loop with accessor = (ecase axis
                                (0 #'vx) (1 #'vy) (2 #'vz))
              for child across children
              for loc = (funcall accessor (location child))
              for siz = (funcall accessor (bsize child))
              do (setf min (min min (- loc siz)))
                 (setf max (max max (+ loc siz))))
        (cond ((< max-range (- max min))
               (setf max-range (- max min))
               (setf max-dim axis))
              (T
               (push axis others))))
      ;; Try to split the node along the best axis.
      (split-node-axis node children max-dim others))))

(defun kd-tree-insert (object tree)
  (let ((dims (kd-tree-dimensions tree))
        (max-depth (kd-tree-max-depth tree))
        (split-size (kd-tree-split-size tree)))
    (with-array (v (location object))
      (with-array (c (location object))
        (with-array (b (location object))
          (flet ((check (node)
                   (let ((axis (node-axis node)))
                     (when (< (abs (- (aref c axis) (node-position node))) (aref b axis))
                       ;; We are intersecting the hyperplane, so insert here.
                       (vector-push-extend object (node-children node))
                       (cond ((< (length (node-children node)) split-size))
                             ;; Node is overfull, split it if we can.
                             ((and (< (node-tree-depth node) max-depth)
                                   (null (node-near node))
                                   (null (node-far node)))
                              (split-node node dims split-size))
                             ;; We should split, but are not leaf.
                             ((< (node-tree-depth node) max-depth)
                              ;; TODO: recompute this subtree
                              )))
                     (return-from kd-tree-insert))))
            (declare (dynamic-extent #'check))
            (%visit-bbox #'check (kd-tree-root tree) c b v)))))))

(defun kd-tree-remove (object tree)
  (with-array (v (location object))
    (with-array (c (location object))
      (with-array (b (location object))
        (flet ((check (node)
                 (let ((axis (node-axis node)))
                   (when (< (abs (- (aref c axis) (node-position node))) (aref b axis))
                     ;; We are intersecting the hyperplane, so we may reside here here.
                     (let* ((children (node-children node))
                            (pos (position object children)))
                       (when pos
                         (loop for i from pos below (length children)
                               do (setf (aref children i) (aref children (1+ i))))
                         (when (= 0 (decf (fill-pointer children)))
                           ;; TODO: Shrink again if possible?
                           )
                         (return-from kd-tree-remove)))))))
          (declare (dynamic-extent #'check))
          (%visit-bbox #'check (kd-tree-root tree) c b v))))))

(defun make-kd-tree (&key (dimensions 3) (split-size 8) (max-depth 255))
  (assert (<= 1 dimensions 3))
  (assert (< 1 split-size))
  (assert (< max-depth 256))
  (%make-kd-tree dimensions split-size max-depth (make-node)))

(defmethod clear ((tree kd-tree))
  (setf (kd-tree-root tree) (make-node))
  tree)

(defmethod reoptimize ((tree kd-tree) &key)
  tree)

(defmethod enter (object (tree kd-tree))
  (kd-tree-insert object tree))

(defmethod leave (object (tree kd-tree))
  (kd-tree-leave object tree))

(defmethod call-with-all (function (tree kd-tree))
  (let ((stack (make-array 0 :adjustable T :fill-pointer T))
        (function (etypecase function
                    (function function)
                    (symbol (fdefinition function)))))
    (declare (dynamic-extent stack))
    (vector-push-extend (kd-tree-root tree) stack)
    (loop for node = (vector-pop stack)
          do (loop for i across (node-children node)
                   do (funcall function i))
             (when (node-near node)
               (vector-push-extend (node-near node) stack))
             (when (node-far node)
               (vector-push-extend (node-far node) stack))
          while (< 0 (length stack)))))

(defmethod call-with-contained (function (tree kd-tree) (region region))
  )

(defmethod call-with-overlapping (function (tree kd-tree) (region region))
  (with-array (c region)
    (with-array (b (region-size region))
      (let ((v (make-array 3 :element-type 'single-float)))
        (declare (dynamic-extent v))
        (setf (aref v 0) (incf (aref c 0) (setf (aref b 0) (* 0.5 (aref b 0)))))
        (setf (aref v 1) (incf (aref c 1) (setf (aref b 1) (* 0.5 (aref b 1)))))
        (setf (aref v 2) (incf (aref c 2) (setf (aref b 2) (* 0.5 (aref b 2)))))
        (flet ((visit (node)
                 (loop for child across (node-children node)
                       do (funcall function child))))
          (declare (dynamic-extent #'visit))
          (%visit-bbox #'visit (kd-tree-root tree) c b v))))))

(defmethod call-with-overlapping (function (tree kd-tree) (sphere sphere))
  (with-array (c sphere)
    (let ((v (make-array 3 :element-type 'single-float)))
      (declare (dynamic-extent v))
      (setf (aref v 0) (aref c 0))
      (setf (aref v 1) (aref c 1))
      (setf (aref v 2) (aref c 2))
      (flet ((visit (node)
               (loop for child across (node-children node)
                     do (funcall function child))))
        (declare (dynamic-extent #'visit))
        (%visit-sphere #'visit (kd-tree-root tree) c (sphere-radius sphere) v)))))

(defmethod call-with-intersecting (function (tree kd-tree) ray-origin ray-direction)
  )
