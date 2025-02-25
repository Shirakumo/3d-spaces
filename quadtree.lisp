(in-package #:org.shirakumo.fraf.trial.space.quadtree)

;;; This code uses the VEC4 type to represent 2D rectangles,
;;; indirectly by including VEC4 in the QUADTREE-NODE structure and
;;; also directly. In this representation, the vector components have
;;; the following semantics:
;;; x: x coordinate of top left corner
;;; x: y coordinate of top left corner
;;; z: x coordinate of bottom right corner
;;; w: y coordinate of bottom right corner

(declaim (inline make-object-vector))
(defun make-object-vector (&optional (initial-size 4))
  (make-array initial-size :adjustable T :fill-pointer 0))

(defstruct (quadtree-node
            (:include vec4) ;; See comment at beginning of file
            (:constructor %%make-quadtree-node
                (org.shirakumo.fraf.math.vectors::varr4
                 depth min-size threshold parent top-left top-right bottom-left bottom-right))
            (:copier NIL)
            (:predicate NIL))
  (depth 0 :type (unsigned-byte 32)) ;; For debugging.
  (min-size 1 :type (unsigned-byte 16)) ;; Minimum quad size.
  (threshold 1 :type (unsigned-byte 16)) ;; Number of objects in a quad before it's split.
  (parent NIL :type (or null quadtree-node))
  (top-left NIL :type (or null quadtree-node)) ;; Child quads.
  (top-right NIL :type (or null quadtree-node))
  (bottom-left NIL :type (or null quadtree-node))
  (bottom-right NIL :type (or null quadtree-node))
  (active-p NIL :type boolean) ;; We disable nodes when they have no content for searches.
  (objects (make-object-vector) :type (vector T))) ;; Stays very short.

(defun %make-quadtree-node (x y z w depth min-size threshold parent top-left top-right bottom-left bottom-right)
  (let ((arr (make-array 4 :element-type 'single-float)))
    (setf (aref arr 0) x)
    (setf (aref arr 1) y)
    (setf (aref arr 2) z)
    (setf (aref arr 3) w)
    (%%make-quadtree-node arr depth min-size threshold parent top-left top-right bottom-left bottom-right)))

(defmethod print-object ((node quadtree-node) stream)
  (print-unreadable-object (node stream :type T)
    (with-vec (x y z w) node
      (format stream "~a ~a ~a ~a: ~:d object~:p"
              x y z w (length (quadtree-node-objects node))))))

(declaim (inline node-empty-p))
(defun node-empty-p (node)
  (declare (optimize speed))
  (= 0 (length (quadtree-node-objects node))))

(declaim (inline node-active-p))
(defun node-active-p (node)
  (declare (optimize speed))
  (and node (quadtree-node-active-p node)))

(defun node-push-objects-into (node vector)
  ;; TODO: Is there a way to declare optimisation for speed here?
  (loop for object across (quadtree-node-objects node)
        do (vector-push-extend object vector))
  vector)

(defun node-clear-objects (node)
  (declare (optimize speed))
  (loop until (node-empty-p node)
        do (vector-pop (quadtree-node-objects node))))

(defun node-pop-objects (node &optional vector)
  (declare (optimize speed))
  (let ((vector (or vector (make-object-vector))))
    (loop until (node-empty-p node)
          do (vector-push-extend (vector-pop (quadtree-node-objects node)) vector)
          finally (return vector))))

(defmacro with-node-children ((top-left top-right bottom-left bottom-right) node &body body)
  `(let ((,top-left (quadtree-node-top-left ,node))
         (,top-right (quadtree-node-top-right ,node))
         (,bottom-left (quadtree-node-bottom-left ,node))
         (,bottom-right (quadtree-node-bottom-right ,node)))
     ,@body))

(defmacro for-node-children ((function node &rest arguments))
  (let ((top-left (gensym "TOP-LEFT"))
        (top-right (gensym "TOP-RIGHT"))
        (bottom-left (gensym "BOTTOM-LEFT"))
        (bottom-right (gensym "BOTTOM-RIGHT")))
    `(with-node-children (,top-left ,top-right ,bottom-left ,bottom-right) ,node
       (when ,top-left (,function ,top-left ,@arguments))
       (when ,top-right (,function ,top-right ,@arguments))
       (when ,bottom-left (,function ,bottom-left ,@arguments))
       (when ,bottom-right (,function ,bottom-right ,@arguments)))))

(defmacro for-node-active-children ((function node &rest arguments))
  (let ((top-left (gensym "TOP-LEFT"))
        (top-right (gensym "TOP-RIGHT"))
        (bottom-left (gensym "BOTTOM-LEFT"))
        (bottom-right (gensym "BOTTOM-RIGHT")))
    `(with-node-children (,top-left ,top-right ,bottom-left ,bottom-right) ,node
       (when (node-active-p ,top-left) (,function ,top-left ,@arguments))
       (when (node-active-p ,top-right) (,function ,top-right ,@arguments))
       (when (node-active-p ,bottom-left) (,function ,bottom-left ,@arguments))
       (when (node-active-p ,bottom-right) (,function ,bottom-right ,@arguments)))))

(defun node-active-children-p (node)
  (declare (optimize speed))
  (with-node-children (tl tr bl br) node
    (or (node-active-p tl)
        (node-active-p tr)
        (node-active-p bl)
        (node-active-p br))))

(defun node-children (node)
  (declare (optimize speed))
  (let ((children ()))
    (for-node-children (push node children))
    (nreverse children)))

(defun node-active-children (node)
  (declare (optimize speed))
  (let ((children ()))
    (for-node-active-children (push node children))
    (nreverse children)))

(defun ensure-child-nodes (node)
  (declare (optimize speed))
  (check-type node quadtree-node)
  (with-vec (left top right bottom) node
    (let ((depth (1+ (quadtree-node-depth node)))
          (min-size (quadtree-node-min-size node))
          (threshold (quadtree-node-threshold node))
          (mid-x (+ left (/ (- right left) 2f0)))
          (mid-y (+ top (/ (- bottom top) 2f0))))
      (unless (quadtree-node-top-left node)
        (setf (quadtree-node-top-left node) (%make-quadtree-node
                                             left top mid-x mid-y depth min-size threshold
                                             node NIL NIL NIL NIL)))
      (unless (quadtree-node-top-right node)
        (setf (quadtree-node-top-right node) (%make-quadtree-node
                                              mid-x top right mid-y depth min-size threshold
                                              node NIL NIL NIL NIL)))
      (unless (quadtree-node-bottom-left node)
        (setf (quadtree-node-bottom-left node) (%make-quadtree-node
                                                left mid-y mid-x bottom depth min-size threshold
                                                node NIL NIL NIL NIL)))
      (unless (quadtree-node-bottom-right node)
        (setf (quadtree-node-bottom-right node) (%make-quadtree-node
                                                 mid-x mid-y right bottom depth min-size threshold
                                                 node NIL NIL NIL NIL))))))

(defun node-remove-children (node &key recurse)
  (declare (optimize speed))
  (let ((children (node-children node)))
    (when children
      (setf (quadtree-node-top-left node) NIL)
      (setf (quadtree-node-top-right node) NIL)
      (setf (quadtree-node-bottom-left node) NIL)
      (setf (quadtree-node-bottom-right node) NIL)
      (loop for child in children
            do (setf (quadtree-node-parent child) NIL)
            when recurse do (node-remove-children child :recurse T)))))

(declaim (inline %region-contains-p))
(defun %region-contains-p (region other)
  (declare (optimize speed))
  (declare (type vec4 region other))
  (let ((rx (vx4 region))
        (ry (vy4 region))
        (rz (vz4 region))
        (rw (vw4 region))
        (ox (vx4 other))
        (oy (vy4 other))
        (oz (vz4 other))
        (ow (vw4 other)))
    (and (<= rx ox) ;; Contains completely.
         (<= ry oy)
         (<= oz rz)
         (<= ow rw))))

(declaim (inline region-contains-area-p))
(defun region-contains-area-p (region center size/2)
  (declare (optimize speed))
  (declare (type vec4 region))
  (declare (type vec2 center size/2))
  (let ((nx (vx4 region))
        (ny (vy4 region))
        (nz (vz4 region))
        (nw (vw4 region))
        (cx (vx2 center))
        (cy (vy2 center))
        (sx (vx2 size/2))
        (sy (vy2 size/2)))
    (and (<= nx (- cx sx)) ;; Contains completely.
         (<= ny (- cy sy))
         (<= (+ cx sx) nz)
         (<= (+ cy sy) nw))))

(defun region-contains-object-p (region object)
  (declare (optimize speed))
  (declare (type vec4 region))
  (let ((loc (location object))
        (siz (bsize object)))
    (declare (type vec2 loc siz))
    (region-contains-area-p region loc siz)))

(declaim (inline %region-overlaps-p))
(defun %region-overlaps-p (region other)
  (declare (optimize speed))
  (declare (type vec4 region other))
  (let ((rx (vx4 region))
        (ry (vy4 region))
        (rz (vz4 region))
        (rw (vw4 region))
        (ox (vx4 other))
        (oy (vy4 other))
        (oz (vz4 other))
        (ow (vw4 other)))
    (and (<= rx oz) ;; Partial touch.
         (<= ox rz)
         (<= ry ow)
         (<= oy rw))))

(declaim (inline region-overlaps-area-p))
(defun region-overlaps-area-p (region center size/2)
  (declare (optimize speed))
  (declare (type vec4 region))
  (declare (type vec2 center size/2))
  (let ((rx (vx4 region))
        (ry (vy4 region))
        (rz (vz4 region))
        (rw (vw4 region))
        (cx (vx2 center))
        (cy (vy2 center))
        (sx (vx2 size/2))
        (sy (vy2 size/2)))
    (and (<= rx (+ cx sx)) ;; Partial touch.
         (<= (- cx sx) rz)
         (<= ry (+ cy sy))
         (<= (- cy sx) rw))))

(defun region-overlaps-object-p (region object)
  (declare (optimize speed))
  (declare (type vec4 region))
  (let ((loc (location object))
        (siz (bsize object)))
    (declare (type vec2 loc siz))
    (region-overlaps-area-p region loc siz)))

(defun node-split (node table)
  (declare (optimize speed))
  ;; Do not split if the node is inactive, it's empty, it'd split the node below the minimum size,
  ;; or there are no active children while we are still below the threshold.
  (when (and (quadtree-node-active-p node)
             (not (node-empty-p node))
             (<= (* 2f0 (quadtree-node-min-size node)) (- (vz4 node) (vx4 node)))
             (<= (* 2f0 (quadtree-node-min-size node)) (- (vw4 node) (vy4 node)))
             (or (node-active-children-p node)
                 (< (quadtree-node-threshold node)
                    (length (quadtree-node-objects node)))))
    (let ((objects (node-pop-objects node))) ;; Clear and rearrange the objects.
      (declare (type (vector T) objects))
      (ensure-child-nodes node)
      (with-node-children (tl tr bl br) node
        (loop while (< 0 (length objects))
              for object = (vector-pop objects)
              for match = (cond
                            ((region-contains-object-p tl object) tl)
                            ((region-contains-object-p tr object) tr)
                            ((region-contains-object-p bl object) bl)
                            ((region-contains-object-p br object) br)
                            (T node))
              do (setf (gethash object table) match)
              do (setf (quadtree-node-active-p match) T)
              do (vector-push-extend object (quadtree-node-objects match))
              unless (eq match node) do (node-split match table)))))
  node)

(defun node-increase-depth (node)
  (declare (optimize speed))
  (incf (quadtree-node-depth node))
  (for-node-children (node-increase-depth node)))

(declaim (inline node-direction*))
(defun node-direction* (node loc)
  (declare (optimize speed))
  (declare (type vec4 node))
  (declare (type vec2 loc))
  (let ((nx (vx4 node))
        (ny (vy4 node))
        (lx (vx2 loc))
        (ly (vy2 loc)))
    (if (< lx nx)
        (if (< ly ny) :top-left :bottom-left)
        (if (< ly ny) :top-right :bottom-right))))

(defun node-direction (node object)
  (declare (optimize speed))
  (declare (type vec4 node))
  (let ((center (location object))
        (size/2 (bsize object)))
    (declare (type vec2 center size/2))
    (node-direction* node (v- center size/2))))

(defun node-extend (node direction)
  (declare (optimize speed))
  (check-type node quadtree-node)
  (with-vec (node-x node-y node-z node-w) node
    (let ((min-size (quadtree-node-min-size node))
          (threshold (quadtree-node-threshold node))
          (width (- node-z node-x))
          (height (- node-w node-y)))
      (flet ((child (x y z w)
               (%make-quadtree-node x y z w 1 min-size threshold NIL NIL NIL NIL NIL)))
        (multiple-value-bind (x y z w)
            (ecase direction
              (:bottom-right (values node-x node-y (+ node-z width) (+ node-w height)))
              (:bottom-left (values (- node-x width) node-y node-z (+ node-w height)))
              (:top-right (values node-x (- node-y height) (+ node-z width) node-w))
              (:top-left (values (- node-x width) (- node-y height) node-z node-w)))
          (let* ((mid-x (+ x width))
                 (mid-y (+ y height))
                 (top-left (if (eql direction :bottom-right) node (child x y mid-x mid-y)))
                 (top-right (if (eql direction :bottom-left) node (child mid-x y z mid-y)))
                 (bottom-left (if (eql direction :top-right) node (child x mid-y mid-x w)))
                 (bottom-right (if (eql direction :top-left) node (child mid-x mid-y z w)))
                 (parent (%make-quadtree-node
                          x y z w 0 min-size threshold NIL
                          top-left top-right bottom-left bottom-right)))
            (setf (quadtree-node-parent top-left) parent)
            (setf (quadtree-node-parent top-right) parent)
            (setf (quadtree-node-parent bottom-left) parent)
            (setf (quadtree-node-parent bottom-right) parent)
            (setf (quadtree-node-active-p parent) (quadtree-node-active-p node))
            (node-increase-depth node)
            parent))))))

(defun node-insert (node object table)
  (declare (optimize speed))
  (when (region-contains-object-p node object)
    (setf (gethash object table) node)
    (setf (quadtree-node-active-p node) T)
    (vector-push-extend object (quadtree-node-objects node))
    (node-split node table))
  node)

(defun node-insert-extend (node object table)
  (declare (optimize speed))
  (if (region-contains-object-p node object)
      (node-insert node object table)
      (node-insert-extend (node-extend node (node-direction node object)) object table)))

(defun node-clear (node vector)
  (declare (optimize speed))
  (when (quadtree-node-active-p node)
    (if vector
        (node-pop-objects node vector)
        (node-clear-objects node))
    (for-node-children (node-clear node vector))
    (setf (quadtree-node-active-p node) NIL))
  vector)

(defun node-reorder (node table)
  (declare (optimize speed))
  (let ((objects (node-clear node (make-object-vector))))
    (declare (type (vector T) objects))
    (loop while (< 0 (length objects))
          do (node-insert node (vector-pop objects) table))))

(defun node-check-activity (node)
  (declare (optimize speed))
  (when (and (node-empty-p node) (not (node-active-children-p node)))
    (setf (quadtree-node-active-p node) NIL)
    (when (quadtree-node-parent node)
      (node-check-activity (quadtree-node-parent node)))))

(defun node-remove (node object table)
  (declare (optimize speed))
  (let ((tmp (node-pop-objects node))
        (found NIL))
    (declare (type (vector T) tmp))
    (loop while (< 0 (length tmp)) ;; Clear the wanted object out.
          for obj = (vector-pop tmp)
          for match-p = (unless found (eq obj object))
          do (if match-p
                 (setf found obj)
                 (vector-push obj (quadtree-node-objects node))))
    (when found
      (when (node-active-children-p node)
        (node-reorder node table))
      (node-check-activity node))
    found))

(defun node-find-all (node vector)
  (declare (optimize speed))
  (when (quadtree-node-active-p node)
    (node-push-objects-into node vector)
    (for-node-children (node-find-all node vector)))
  vector)

(defun node-find (node region vector)
  (declare (optimize speed))
  (when (and (quadtree-node-active-p node) (%region-overlaps-p node region))
    (node-push-objects-into node vector)
    (if (%region-contains-p region node)
        (for-node-children (node-find-all node vector))
        (for-node-children (node-find node region vector))))
  vector)

(defun node-set-min-size (node min-size)
  (declare (optimize speed))
  (setf (quadtree-node-min-size node) min-size)
  (for-node-children (node-set-min-size node min-size)))

(defun node-set-threshold (node threshold)
  (declare (optimize speed))
  (setf (quadtree-node-threshold node) threshold)
  (for-node-children (node-set-threshold node threshold)))

(defstruct (quadtree
            (:include container)
            (:constructor make-quadtree ())
            (:copier NIL)
            (:predicate NIL))
  (root (%make-quadtree-node 0f0 0f0 100f0 100f0 0 1 1 NIL NIL NIL NIL NIL) :type quadtree-node)
  (table (make-hash-table :test 'eq) :type hash-table))

(defun make-quadtree-at (location size &key min-size threshold)
  (declare (optimize speed))
  (declare (type vec2 location size))
  (declare (type (or null (unsigned-byte 16)) min-size threshold))
  (let* ((tree (make-quadtree))
         (root (quadtree-root tree))
         (lx (vx2 location))
         (ly (vx2 location))
         (sx (vx2 size))
         (sy (vx2 size)))
    ;; Muffling warnings because it complains about the deletion of unreachable code otherwise.
    #+sbcl (locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
             (vsetf root lx ly (+ lx sx) (+ ly sy)))
    #-sbcl (vsetf root lx ly (+ lx sx) (+ ly sy))
    (when (and min-size (< 0 min-size))
      (node-set-min-size root min-size))
    (when (and threshold (< 0 threshold))
      (node-set-threshold root threshold))
    tree))

(defmethod print-object ((object quadtree) stream)
  (let ((root (quadtree-root object)))
    (print-unreadable-object (object stream :type T)
      (format stream "~a ~a ~a ~a: ~:d object~:p"
              (vx root) (vy root) (vz root) (vw root)
              (hash-table-count (quadtree-table object))))))

(defun quadtree-insert (tree object)
  (declare (optimize speed))
  (when (gethash object (quadtree-table tree))
    (quadtree-remove tree object))
  (setf (quadtree-root tree) (node-insert-extend (quadtree-root tree) object (quadtree-table tree)))
  object)

(defun quadtree-remove (tree object)
  (declare (optimize speed))
  (let* ((table (quadtree-table tree))
         (node (gethash object table)))
    (when node
      (remhash object table)
      (node-remove node object table))))

(defun quadtree-update (tree object)
  (declare (optimize speed))
  (let ((node (gethash object (quadtree-table tree))))
    (when node
      (cond
        ((region-contains-object-p node object)
         ;; Might need a split if it can now be stored in a sub-node.
         (node-split node (quadtree-table tree)))
        (T ;; If it no longer fits, just reinsert.
         (quadtree-insert tree object)))
      object)))

(defun quadtree-find-all (tree &optional vector)
  (declare (optimize speed))
  (let ((vector (or vector (make-object-vector))))
    (node-find-all (quadtree-root tree) vector)
    vector))

(defun quadtree-find-overlaps (tree region &optional vector)
  (declare (optimize speed))
  (declare (type vec4 region))
  (let ((tmp (make-object-vector))
        (vector (or vector (make-object-vector))))
    (node-find (quadtree-root tree) region tmp)
    (loop while (< 0 (length tmp))
          for object = (vector-pop tmp)
          when (region-overlaps-object-p region object)
          do (vector-push-extend object vector))
    vector))

(defun quadtree-find-overlaps-in (tree location size &optional vector)
  (declare (optimize speed))
  (declare (type vec2 location size))
  (let ((lx (vx2 location))
        (ly (vy2 location))
        (sx (vx2 size))
        (sy (vy2 size)))
    (quadtree-find-overlaps tree (vec4 lx ly (+ lx sx) (+ ly sy)) vector)))

(defun quadtree-find-for (tree object &optional vector)
  (declare (optimize speed))
  (quadtree-find-overlaps-in tree (location object) (bsize object) vector))

(defun quadtree-find-contained (tree region &optional vector)
  (declare (optimize speed))
  (declare (type vec4 region))
  (let ((tmp (make-object-vector))
        (vector (or vector (make-object-vector))))
    (node-find (quadtree-root tree) region tmp)
    (loop while (< 0 (length tmp))
          for object = (vector-pop tmp)
          when (region-contains-object-p region object)
          do (vector-push-extend object vector))
    vector))

(defun quadtree-find-contained-in (tree location size &optional vector)
  (declare (optimize speed))
  (declare (type vec2 location size))
  (let ((lx (vx2 location))
        (ly (vy2 location))
        (sx (vx2 size))
        (sy (vy2 size)))
    (quadtree-find-contained tree (vec4 lx ly (+ lx sx) (+ ly sy)) vector)))

(defmethod enter (object (tree quadtree))
  (quadtree-insert tree object))

(defmethod leave (object (tree quadtree))
  (quadtree-remove tree object))

(defmethod clear ((tree quadtree))
  (clrhash (quadtree-table tree))
  (node-clear (quadtree-root tree) NIL)
  tree)

(defmethod update (object (tree quadtree))
  (quadtree-update tree object))

(defmethod describe-object ((tree quadtree) stream)
  (call-next-method)
  (format stream "~%~&-------------------------")
  (org.shirakumo.text-draw:tree (quadtree-root tree) #'node-children :stream stream))

(defun quadtree-lines (tree)
  (let ((root (quadtree-root tree))
        (points ()))
    (labels ((depth-color (depth)
               (let ((value (max 0.0 (- 1.0 (/ depth 100)))))
                 (vec 1 value value 0.1)))
             (recurse (node)
               (let ((color (depth-color (quadtree-node-depth node))))
                 (push (list (vxy_ node) color) points)
                 (push (list (vzy_ node) color) points)
                 (push (list (vxw_ node) color) points)
                 (push (list (vzw_ node) color) points)
                 (push (list (vxy_ node) color) points)
                 (push (list (vxw_ node) color) points)
                 (push (list (vzy_ node) color) points)
                 (push (list (vzw_ node) color) points)
                 (for-node-active-children (recurse node)))))
      (when (node-active-p root) (recurse root))
      points)))

(defmethod check ((tree quadtree)) ;; None of these things should happen.
  (declare (optimize speed))
  (labels ((recurse (node)
             (when (and (not (quadtree-node-active-p node)) (node-active-children-p node))
               (error "Node ~a~%has active children without being active itself." node))
             (when (and (not (quadtree-node-active-p node))
                        (< 0 (length (quadtree-node-objects node))))
               (error "Node ~a~%has objects without being active itself." node))
             (when (and (quadtree-node-active-p node) (not (node-active-children-p node))
                        (= 0 (length (quadtree-node-objects node))))
               (error "Node ~a~%is active without active children or objects." node))
             (for-node-children (recurse node))
             (let ((objects (make-object-vector)))
               (node-push-objects-into node objects)
               (loop while (< 0 (length objects))
                     for object = (vector-pop objects)
                     unless (eq node (gethash object (quadtree-table tree)))
                     do (error "Node ~a~%is not assigned to object~%  ~a~% as it is assigned to~%node ~a"
                               node object (gethash object (quadtree-table tree)))))))
    (recurse (quadtree-root tree)))
  (loop for object being the hash-keys of (quadtree-table tree)
        for node being the hash-values of (quadtree-table tree)
        for objects = (make-object-vector)
        do (node-push-objects-into node objects)
        do (loop while (< 0 (length objects))
                 for obj = (vector-pop objects)
                 for match = (eq obj object)
                 until match
                 finally (unless match
                           (error "Node ~a~%does not refer to object~%  ~a" node object)))))

(defmethod reoptimize ((tree quadtree) &key) ;; Useful only if something's gone very wrong.
  (declare (optimize speed))
  (let ((objects (node-clear (quadtree-root tree) (make-object-vector))))
    (declare (type (vector T) objects))
    (clrhash (quadtree-table tree))
    (loop while (< 0 (length objects))
          do (quadtree-insert tree (vector-pop objects)))))

(defstruct (quadtree-iterator
            (:constructor make-quadtree-iterator (tree region contain-p))
            (:copier NIL)
            (:predicate NIL))
  (tree NIL :type quadtree)
  (region NIL :type vec4)
  (contain-p NIL :type boolean))

(defmethod for:make-iterator ((tree quadtree) &key region contain)
  (make-quadtree-iterator tree (or region (quadtree-root tree)) contain))

(defmethod for:step-functions ((iterator quadtree-iterator))
  (declare (optimize speed))
  (let* ((region (quadtree-iterator-region iterator))
         (contain-p (quadtree-iterator-contain-p iterator))
         (node (quadtree-root (quadtree-iterator-tree iterator))) ;; Current node.
         (objects (node-push-objects-into node (make-object-vector))) ;; Current node's objects.
         (next-object NIL)
         (child-stack (make-array 1 :fill-pointer 0 :adjustable T
                                    :element-type '(or null list)
                                    :initial-element NIL)))
    (declare (type (vector T) objects))
    (when (or (eq node region) (%region-contains-p region node))
      (setf region NIL))
    (labels ((pop-stack () ;; Keep track of child nodes that haven't been checked yet.
               (vector-pop child-stack))
             (push-stack (parent)
               (vector-push-extend (node-active-children parent) child-stack))
             (pop-child ()
               (let ((children (vector-pop child-stack)))
                 (prog1 (when children (pop children))
                   (vector-push children child-stack))))
             (next-object () ;; Ensures that the next object is within region.
               (loop while (and (null next-object) (< 0 (length objects)))
                     for object = (vector-pop objects)
                     when (or (null region) (if contain-p
                                                (region-contains-object-p region object)
                                                (region-overlaps-object-p region object)))
                     do (setf next-object object)))
             (next-quad () ;; Updates the current node, object list, and maintains the child stack.
               (let ((child (pop-child)))
                 (cond
                   ((and child (or (null region) (%region-overlaps-p region child)))
                    (node-push-objects-into child objects)
                    (push-stack child)
                    (setf node child))
                   ((null child)
                    (pop-stack)
                    (if (< 0 (length child-stack))
                        (next-quad)
                        (setf node NIL)))
                   (T (next-quad)))))
             (next () ;; Updates the current node and the next object.
               (setf next-object NIL)
               (loop until (or (null node) next-object)
                     do (next-object)
                     unless next-object do (next-quad))))
      (push-stack node)
      (next)
      (values
       (lambda ()
         (when node
           (prog1 next-object
             (next))))
       (lambda ()
         (and node (not (null next-object))))
       (lambda (value)
         (declare (ignore value))
         (error "Not supported"))
       (lambda ())))))

(defmethod call-with-all (function (tree quadtree))
  (let ((function (ensure-function function)))
    (for:for ((object over tree))
      (funcall function object))))

(defmethod call-with-contained (function (tree quadtree) (region region))
  (let* ((function (ensure-function function))
         (x (vx3 region))
         (y (vy3 region))
         (size (region-size region))
         (region (vec x y (+ x (vx3 size)) (+ y (vy3 size)))))
    (declare (dynamic-extent region))
    (for:for ((object over tree :region region :contain T))
      (funcall function object))))

(defmethod call-with-overlapping (function (tree quadtree) (region region))
  (let* ((function (ensure-function function))
         (x (vx3 region))
         (y (vy3 region))
         (size (region-size region))
         (region (vec x y (+ x (vx3 size)) (+ y (vy3 size)))))
    (declare (dynamic-extent region))
    (for:for ((object over tree :region region :contain NIL))
      (funcall function object))))
