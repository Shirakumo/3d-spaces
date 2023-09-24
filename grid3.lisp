(in-package #:org.shirakumo.fraf.trial.space.grid3)

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

;;; The volume of a grid cell at index (i, j, k) is
;;;
;;;   [x+c*i,x+c*(i+1)]x[y+c*j,y+c*(j+1)]x[z+c*k,z+c*(k+1)]
;;;
;;; where (x, y, z) = l-s/2 and l is the location vector of the grid
;;; and s is the full (not half) size vector of the volume of the grid
;;; and c is the grid cell size. That is, the cells (0, 0, 0) and (w,
;;; h, d) are at the corners of the volume grid and the cell
;;; (floor(w/2), floor(h/2), floor(d/2)) is at the center of the
;;; volume.
(defmacro %with-grid-coordinates ((x y z) (grid xv yv zv) &body body)
  `(let* ((gl (location ,grid))
          (c (grid-cell ,grid))
          (w (grid-w ,grid))
          (h (grid-h ,grid))
          (d (grid-d ,grid))
          (,x (clamp 0 (the (signed-byte 32) (floor (+ (/ (- ,xv (vx3 gl)) c) (* .5 w)))) (1- w)))
          (,y (clamp 0 (the (signed-byte 32) (floor (+ (/ (- ,yv (vy3 gl)) c) (* .5 h)))) (1- h)))
          (,z (clamp 0 (the (signed-byte 32) (floor (+ (/ (- ,zv (vz3 gl)) c) (* .5 d)))) (1- d))))
     (declare (type vec3 gl)
              (type (unsigned-byte 32) w h d)
              (type (unsigned-byte 32) ,x ,y ,z)
              (ignorable w h d))
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
    (declare (type vec3 ol os))
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
        (unless location
          (setf location (v- region (region-size region))))
        (unless bsize
          (setf bsize (v* (region-size region) 0.5)))
        (when (and (not cell-size) (< 0 (length old)))
          (let ((biggest 0.0))
            (loop for object across old
                  for bsize = (bsize object)
                  do (etypecase bsize
                       (vec3 (setf biggest (max biggest (vx3 bsize) (vy3 bsize) (vz3 bsize))))
                       (vec2 (setf biggest (max biggest (vx2 bsize) (vy2 bsize))))))
            (setf cell-size (* 2.0 biggest))))))
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
  (declare (optimize speed))
  (let ((function (ensure-function function)))
    (maphash (lambda (key value)
               (declare (ignore value))
               (funcall function key))
             (grid-table grid))))

(defmacro with-nesting (&body body)
  (destructuring-bind (first . rest) body
    (if rest
        (append first `((with-nesting ,@rest)))
        first)))

(defmethod call-with-contained (function (grid grid) (region region))
  (declare (optimize speed (safety 1)))
  (let ((function (ensure-function function))
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
  (let ((function (ensure-function function))
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

(defmethod call-with-intersecting (function (grid grid) ray-origin ray-direction)
  (declare (optimize speed (safety 1)))
  (check-type ray-origin vec3)
  (check-type ray-direction vec3)
  (let ((function (ensure-function function))
        (data (grid-data grid)))
    (%with-grid-coordinates (x0 y0 z0) (grid (vx3 ray-origin) (vy3 ray-origin) (vz3 ray-origin))
      (setf x0 (max 0 (1- x0)))
      (setf y0 (max 0 (1- y0)))
      (setf z0 (max 0 (1- z0)))
      (let* ((dx (abs (vx3 ray-direction))) (sx (if (<= 0 (vx3 ray-direction)) +1 -1))
             (dy (abs (vy3 ray-direction))) (sy (if (<= 0 (vy3 ray-direction)) +1 -1))
             (dz (abs (vz3 ray-direction))) (sz (if (<= 0 (vz3 ray-direction)) +1 -1))
             (dm (max dx dy dz))
             (x1 (* 0.5 dm)) (y1 x1) (z1 x1)
             (zstride (* w h))
             (i (+ x0 (* y0 w) (the (unsigned-byte 32) (* z0 zstride))))
             ;; We cache the last 16 indices here to avoid iterating over
             ;; duplicate indices. The idea being that testing this index
             ;; is going to be a lot faster than re-testing the fine collisions
             ;; of the objects, and as such well worth the price.
             (cache (make-array 16 :element-type '(unsigned-byte 32)))
             (cache-i 0))
        (declare (type (integer -1 +1) sx sy sz))
        (declare (type (unsigned-byte 32) i x0 y0 z0 zstride))
        (declare (type single-float dx dy dz dm x1 y1 z1))
        (declare (type (integer 0 16) cache-i))
        (flet ((try (i)
                 (when (and (<= 0 i (1- (length data)))
                            (loop for j from 0 below 16
                                  never (= i (aref cache j))))
                   (setf (aref cache cache-i) i)
                   (setf cache-i (mod (1+ cache-i) 16))
                   (loop for object in (aref data i)
                         do (funcall function object)))))
            (loop while (and (< 0 x0 (1- w))
                             (< 0 y0 (1- h))
                             (< 0 z0 (1- d)))
                  do (try i)
                     (try (+ i 1))
                     (try (+ i w))
                     (try (+ i (the (unsigned-byte 32) zstride)))
                     (try (+ i (the (unsigned-byte 32) (+ 1 w zstride))))
                     (decf x1 dx) (when (< x1 0) (incf x1 dm) (incf x0 sx) (incf i (* sx)))
                     (decf y1 dy) (when (< y1 0) (incf y1 dm) (incf y0 sy) (incf i (* sy w)))
                     (decf z1 dz) (when (< z1 0) (incf z1 dm) (incf z0 sz) (incf i (the (unsigned-byte 32) (* sz zstride))))))))))
