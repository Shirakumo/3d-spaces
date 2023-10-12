(defpackage #:org.shirakumo.fraf.trial.space.test
  (:use #:cl #:parachute #:org.shirakumo.fraf.math.vectors)
  (:local-nicknames
   (#:space #:org.shirakumo.fraf.trial.space)
   (#:bvh2 #:org.shirakumo.fraf.trial.space.bvh2)
   (#:quadtree #:org.shirakumo.fraf.trial.space.quadtree)
   (#:grid3 #:org.shirakumo.fraf.trial.space.grid3)
   (#:kd-tree #:org.shirakumo.fraf.trial.space.kd-tree))
  (:export
   #:test
   #:benchmark-insert
   #:benchmark-remove))

(in-package #:org.shirakumo.fraf.trial.space.test)

(define-test 3d-spaces)

(defun set= (a b)
  (null (set-exclusive-or a b)))

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
  (test-container-generic #'bvh2:make-bvh #'box2 #'vec2-ignore-z))

(define-test quadtree
  :parent 2d
  (test-container-generic #'quadtree:make-quadtree #'box2 #'vec2-ignore-z))

(define-test kd2
  :parent 2d
  (test-container-generic (lambda () (kd-tree:make-kd-tree :dimensions 2)) #'box2 #'vec2-ignore-z))

(define-test grid3
  :parent 3d
  (test-container-generic (lambda () (grid3:make-grid 10)) #'box3 #'vec))

(define-test kd3
  :parent 3d
  (test-container-generic (lambda () (kd-tree:make-kd-tree :dimensions 3)) #'box3 #'vec))

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

(defun vec2-ignore-z (x y z)
  (declare (ignore z))
  (vec x y))

(defun random* (min max)
  (+ min (random (- max min))))

(defun test-container-generic (constructor object-constructor vector-constructor)
  (flet ((make-container ()
           (funcall constructor))
         (make-object (&rest args)
           (apply object-constructor args))
         ;; MAKE-VEC instead of VEC so that VEC calls in
         ;; macroexpansions use the correct definition.
         (make-vec (x y z)
           (funcall vector-constructor x y z)))
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
        (finish (space:do-candidates (object container box)
                  (is eq box object)))
        (finish (space:do-contained (object container box)
                  (is eq box object)))
        (finish (space:do-overlapping (object container box)
                  (is eq box object)))
        (is eq box (space:do-intersecting (object container (make-vec 0 0 0) (make-vec +1 0 0) NIL)
                     (return object)))
        (is eq NIL (space:do-intersecting (object container (make-vec -1 0 0) (make-vec -1 0 0) NIL)
                     (return object)))
        (finish (space:leave box container))
        (finish (space:do-all (object container)
                  (false object)))))

    (group (fixed)
      (let ((container (make-container))
            (a (make-object (make-vec 0 0 0) (make-vec 5 5 5)))
            (b (make-object (make-vec 15 0 0) (make-vec 5 5 5)))
            (c (make-object (make-vec 300 0 0) (make-vec 5 5 5))))
        (finish (space:enter (list a b c) container))
        (finish (space:do-all (object container)
                  (true (or (eq object a) (eq object b) (eq object c)))))
        (finish (space:do-candidates (object container (space:region 0 0 0 10 10 10))
                  (true (or (eq object a) (eq object b) (eq object c)))))
        (finish (space:do-overlapping (object container (space:region 0 0 0 10 10 10))
                  (false (eq object c))))
        (is set= (list a b c)
            (let ((list ()))
              (space:do-intersecting (object container (make-vec 0 0 0) (make-vec +1 0 0) list)
                (push object list))))
        (is set= (list)
            (let ((list ()))
              (space:do-intersecting (object container (make-vec -15 0 0) (make-vec 0 +1 0) list)
                (push object list))))
        (is set= (list a b)
            (let ((list ()))
              (space:do-intersecting (object container (make-vec 15 0 0) (make-vec -1 0 0) list)
                (push object list))))
        (is set= (list a)
            (let ((list ()))
              (space:do-intersecting (object container (make-vec 0 0 0) (make-vec 1 1 0) list)
                (push object list))))))

    #+(or) (group (randomized)
      (let ((container (make-container))
            (objects (loop repeat 10
                           collect (make-object (vrand (make-vec 0 0 0) 100) (vrand (make-vec 50 50 50) 100)))))
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

(defun make-nodes (count spread size-spread)
  (let ((nodes (make-array count)))
    (map-into nodes (lambda () (box3 (vrand (vec3) spread)
                                     (vrand (vec3) size-spread))))))

(defun benchmark-insert (constructor &key (nodes 1000000) (spread 100.0) (size-spread 1.0))
  (let ((nodes (make-nodes nodes spread size-spread))
        (container (funcall constructor)))
    (time (loop for node across nodes do (space:enter node container)))
    container))

(defun benchmark-remove (constructor &key (nodes 1000000) (spread 100.0) (size-spread 1.0) (percentile 0.5))
  (let ((nodes (make-nodes nodes spread size-spread))
        (to-remove (make-array (floor (* percentile nodes))))
        (container (funcall constructor)))
    (loop with i = 0
          while (< i (length to-remove))
          do (loop for node across nodes
                   while (< i (length to-remove))
                   do (when (< (random 1.0) percentile)
                        (setf (aref to-remove i) node)
                        (incf i))))
    (loop for node across nodes do (space:enter node container))
    (time (loop for node across to-remove do (space:leave node container)))
    container))
