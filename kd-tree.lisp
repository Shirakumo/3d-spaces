(in-package #:org.shirakumo.fraf.trial.space.kd-tree)

;;; This kd-tree implementation follows Houthuys(1987): "Box Sort, a
;;; multidimensional binary sorting method for rectangular boxes, used
;;; for quick range searching" to some extent. In particular, like
;;; Houthuys, the extension for handling rectangular regions instead
;;; of just points uses the minimal corner of each rectangular region
;;; as the primary property and adjusts all structures and operations
;;; to account for the fact that a volume extends from that point.
;;;
;;; In a (2D) figure:
;;;
;;;                  splitting plane
;;;                         │
;;;       near node         │           far node
;;;                         │
;;;                         │
;;;         ┌───────────────────────┐
;;;         │ rectangle 1           │
;;;         x───────────────────────┘
;;;   minimal corner        │
;;;       ┌─────────────┐   │
;;;       │ rectangle 2 │   │
;;;       └─────────────┘   │
;;;                         │
;;;                         │
;;;                         │   ┌────────────────────┐
;;;                         │   │ rectangle 3        │
;;;                         │   └────────────────────┘
;;;                         │
;;;
;;; For rectangles 1 and 2, the minimal corner is below (to left in
;;; the figure) the position of the splitting plane so those are
;;; associated with the near node. The minimal corner of rectangle 3
;;; is above the position of the splitting pane so that rectangle is
;;; associated with the far node.
;;;
;;; In addition to its associated objects, each node has an associated
;;; bounding box:
;;;
;;;                  splitting plane
;;;                         │
;;;       near node         │           far node
;;;                         │
;;;      ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
;;;      ┃  ┌───────────────────────┐┃
;;;      ┃  │ rectangle 1           │┃
;;;      ┃  └───────────────────────┘┃
;;;      ┃                  │        ┃
;;;      ┃┌─────────────┐   │        ┃
;;;      ┃│ rectangle 2 │   │        ┃
;;;      ┃└─────────────┘   │        ┃
;;;      ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
;;;                         │  ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;                         │  ┃┌────────────────────┐
;;;                         │  ┃│ rectangle 3        │
;;;                         │  ┃└────────────────────┘
;;;                         │  ┃
;;;
;;; The splitting plane in combination with the node bounding boxes
;;; allows subtrees to be omitted from tree operations under certain
;;; conditions:
;;;
;;; * If, for the current axis, the region of interest is entirely
;;;   below the splitting plane, rectangles which are associated with
;;;   the far node cannot overlap the region of interest. The
;;;   equivalent is not true for the far side of the splitting plane:
;;;   If the region of interest is entirely above the splitting plane,
;;;   rectangles which are associated with the near node can still
;;;   overlap the region of interest (like rectangle 1 in the above
;;;   figures).
;;;
;;; * If, for the current axis, the region of interest does not
;;;   overlap the bounding box of the near node, rectangles which are
;;;   associated with the near node cannot overlap the region of
;;;   interest.
;;;
;;; * If, for the current axis, the region of interest does not
;;;   overlap the bounding box of the far node, rectangles which are
;;;   associated with the far node cannot overlap the region of
;;;   interest.

;;; Utilities

(declaim (inline ensure-vec3))
(defun ensure-vec3 (vec)
  (etypecase vec
    (vec3 vec)
    (vec2 (vec3 vec 0))))

(declaim (inline sqrdist))
(defun sqrdist (a b)
  (declare (type (simple-array single-float (3)) a b))
  (+ (expt (- (aref a 0) (aref b 0)) 2)
     (expt (- (aref a 1) (aref b 1)) 2)
     (expt (- (aref a 2) (aref b 2)) 2)))

(deftype dimension-count ()
  `(integer 1 3))

(deftype axis-index ()
  `(integer 0 2))

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

(defmacro with-axis-dispatch ((&rest clauses) axis-form &body body)
  (flet ((emit-case (axis reader)
           (flet ((emit-binding (binding)
                    (destructuring-bind (name kind) binding
                      (ecase kind
                        (:array-access
                         `((,name (container)
                             `(aref ,container ,',axis))))
                        (:vector-access
                         `((,name (container)
                             `(,',reader ,container))))))))
             `(,axis
               (macrolet (,@(mapcan #'emit-binding clauses))
                 ,@body)))))
    `(ecase ,axis-form
       ,(emit-case 0 'vx3)
       ,(emit-case 1 'vy3)
       ,(emit-case 2 'vz3))))

;;; Object info structure
;;;
;;; Each instance of this structure stores an object together with its
;;; axis-aligned bounding rectangle. Experiments have shown that
;;; storing the minimal and maximal corners of the bounding rectangle
;;; is much more efficient than computing the information on-demand by
;;; calling `location' and `bsize'.

(defstruct (object-info
            (:constructor %make-object-info (object bb-min bb-max))
            (:copier NIL)
            (:predicate NIL))
  (object (error "required") :read-only T)
  ;; Cached object bounding box. Always stored as `vec3'. For
  ;; dimensions < 3, only the first components are accessed.
  (bb-min (error "required") :type vec3 :read-only T)
  (bb-max (error "required") :type vec3 :read-only T))

(declaim (inline make-object-info))
(defun make-object-info (object)
  (let* ((location (ensure-vec3 (location object)))
         (size/2 (ensure-vec3 (bsize object)))
         (bb-min (v- location size/2))
         (bb-max (v+ location size/2)))
    (%make-object-info object bb-min bb-max)))

(declaim (inline nexpand-bounds-for-object))
(defun nexpand-bounds-for-object (min max object-info)
  (nvmin min (object-info-bb-min object-info))
  (nvmax max (object-info-bb-max object-info)))

;;; Node structures
;;;
;;; The `node' structure contains common slots, `inner-node' and
;;; `leaf' are specialized subclasses.
;;;
;;; The depth of a node is 0 for leafs and (1+ (depth <any child>))
;;; for inner nodes.

(defstruct (node
            (:constructor NIL)
            (:copier NIL)
            (:predicate NIL))
  ;; Node bounding box which bounds everything in the subtree rooted
  ;; at the node. Always stored as `vec3'. For dimensions < 3, only
  ;; the first components are accessed. The values of the slots are
  ;; destructively adjusted when
  ;; 1. objects are added or removed in the `leaf' subclass
  ;; 2. children are added or removed in the `inner-node' subclass
  (bb-min (error "required") :type vec3 :read-only T)
  (bb-max (error "required") :type vec3 :read-only T))

(defstruct (inner-node
            (:include node)
            (:conc-name node-)
            (:constructor %make-inner-node-with-bounds (near far axis position bb-min bb-max))
            (:copier NIL)
            (:predicate NIL))
  ;; Tree structure
  (near (error "required") :type node)
  (far (error "required") :type node)
  ;; Splitting axis
  (axis 0 :type axis-index)
  (position 0.0f0 :type single-float))

(defun make-inner-node (near far axis position bb-min bb-max)
  (%make-inner-node-with-bounds near far axis position bb-min bb-max))

(defmethod print-object ((node inner-node) stream)
  (print-unreadable-object (node stream :type T)
    (format stream "~[X~;Y~;Z~] ~f"
            (node-axis node) (node-position node))))

(defun compute-bounds-for-objects (infos)
  (let ((min (vec #1=most-positive-single-float #1# #1#))
        (max (vec #2=most-negative-single-float #2# #2#)))
    (unless (null infos)
      (map NIL (lambda (info)
                 (nexpand-bounds-for-object min max info))
           infos))
    (values min max)))

(deftype object-info-vector ()
  ;; TODO(jmoringe): shouldn't this work without U-A-E-T? But SBCL
  ;; derives (array * (*)) instead of (array t (*)) in some places
  ;; without U-A-E-T.
  `(and (vector ,(upgraded-array-element-type 'object-info))
        (not simple-vector)))

(declaim (inline make-object-info-vector))
(defun make-object-info-vector ()
  (make-array 0 :adjustable T :fill-pointer T :element-type 'object-info))

(defstruct (leaf
            (:include node)
            (:conc-name node-)
            (:constructor %make-leaf (objects bb-min bb-max))
            (:copier NIL))
  (objects (error "required") :type object-info-vector :read-only T))

(declaim (ftype (function (&optional object-info-vector) (values node &optional NIL))
                make-leaf))
(defun make-leaf (&optional object-infos)
  (multiple-value-bind (min max) (compute-bounds-for-objects object-infos)
    (if object-infos
        (%make-leaf object-infos min max)
        (%make-leaf (make-object-info-vector) min max))))

(defun make-leaf-with-bounds (object-infos bb-min bb-max)
  (%make-leaf object-infos bb-min bb-max))

(defmethod print-object ((node leaf) stream)
  (print-unreadable-object (node stream :type T)
    (format stream "leaf (~d object~:p)" (length (node-objects node)))))

(defun leaf-push-object (object-info leaf)
  ;; Return the index of the stored info. The caller can use this to
  ;; decide whether the leaf should be split.
  (prog1
      (vector-push-extend object-info (node-objects leaf))
    (nexpand-bounds-for-object (node-bb-min leaf) (node-bb-max leaf) object-info)))

(defun leaf-delete-object (object leaf)
  (let* ((objects (node-objects leaf))
         (position (position object objects :key #'object-info-object)))
    (when position
      (loop for i from position below (1- (length objects))
            do (setf (aref objects i) (aref objects (1+ i))))
      (decf (fill-pointer objects))
      ;; TODO(jmoringe): could check whether object was part of the
      ;; support of the bounding box and skip the re-computation if it
      ;; was not.
      (multiple-value-bind (bb-min bb-max) (compute-bounds-for-objects objects)
        (v<- (node-bb-min leaf) bb-min)
        (v<- (node-bb-max leaf) bb-max))
      T)))

(declaim (inline nexpand-bounds-for-node))
(defun nexpand-bounds-for-node (node other-node)
  (nvmin (node-bb-min node) (node-bb-min other-node))
  (nvmax (node-bb-max node) (node-bb-max other-node)))

;;; kd-tree structure

(defstruct (kd-tree
            (:include container)
            (:constructor %make-kd-tree (dimensions split-size max-depth root))
            (:copier NIL)
            (:predicate NIL))
  (dimensions 0 :type dimension-count :read-only T)
  (split-size 0 :type (integer 2 255) :read-only T)
  (max-depth 0 :type (unsigned-byte 8) :read-only T)
  (root (error "required") :type node)

(defun make-kd-tree (&key (dimensions 3) (split-size 8) (max-depth 255))
  (check-type dimensions dimension-count)
  (assert (< 1 split-size))
  (assert (< max-depth 256))
  (%make-kd-tree dimensions split-size max-depth (make-leaf)))

(defmethod print-object ((tree kd-tree) stream)
  (print-unreadable-object (tree stream :type T)
    (format stream "~d (~d object~:p)"
            (kd-tree-dimensions tree)
            (object-count tree))))

(defmethod describe-object ((tree kd-tree) stream)
  (call-next-method)
  (format stream "~&~%-------------------------~%")
  (describe-tree (kd-tree-root tree)
                 (lambda (node)
                   (unless (leaf-p node)
                     (list (node-near node) (node-far node))))
                 stream))

(defun %visit-sphere (f node center radius volume)
  (declare (optimize speed (safety 1)))
  (declare (type (simple-array single-float (3)) center volume))
  (declare (type single-float radius))
  (declare (type function f))
  (declare (type node node))
  (let ((a (node-near node))
        (b (node-far node))
        (axis (node-axis node))
        (position (node-position node)))
    (funcall f node)
    (when a
      (when (< position (aref center axis))
        (rotatef a b))
      (%visit-sphere f a center radius volume)
      (let ((old (shiftf (aref volume axis) position)))
        ;; If the splitting axis is within the radius, also check the other side
        (when (< (sqrdist volume center) (* radius radius))
          (%visit-sphere f b center radius volume))
        (setf (aref volume axis) old)))))

(defun visit-sphere (f node center radius)
  (with-array (v center)
    (with-array (c center)
      (%visit-sphere f node c (float radius 0f0) v))))

(declaim (ftype (function (function node (simple-array single-float (3)) (simple-array single-float (3))))
                %visit-bbox))
(defun %visit-bbox (f node region-min region-max)
  (declare (optimize speed (safety 1)))
  (labels ((visit (node)
             (cond ((leaf-p node)
                    (funcall f node))
                   (T
                    (let ((axis (node-axis node))
                          (position (node-position node)))
                      (with-axis-dispatch ((element   :array-access)
                                           (component :vector-access))
                          axis
                        (cond ((<= (aref region-max axis) position)
                               (let ((near (node-near node)))
                                 (when (and (<= (component (node-bb-min near)) (element region-max))
                                            (<= (element region-min) (component (node-bb-max near))))
                                   (visit near))))
                              (T
                               (let ((min (element region-min))
                                     (max (element region-max))
                                     (near (node-near node))
                                     (far (node-far node)))
                                 (when (and (<= (component (node-bb-min near)) max)
                                            (<= min (component (node-bb-max near))))
                                   (visit near))
                                 (when (and (<= (component (node-bb-min far)) max)
                                            (<= min (component (node-bb-max far))))
                                   (visit far)))))))))))
    (visit node)))

(defun best-split-axis (dimension-count parent-axis bounding-box-min bounding-box-max)
  (let ((max-range 0.0f0) (max-axis -1) (other-axes ()))
    (declare (type single-float max-range))
    ;; Figure out widest spread axis
    (dotimes (axis dimension-count)
      (let ((range (with-axis-dispatch ((component :vector-access)) axis
                     (let* ((min (component bounding-box-min))
                            (max (component bounding-box-max)))
                       (- max min)))))
        (cond ;; If the new RANGE is larger or if we don't have a best
              ;; axis yet, remember new axis and push previous best
              ;; axis to fallback list
              ((or (< max-range range) (= max-axis -1))
               (unless (= max-axis -1)
                 (push max-axis other-axes))
               (setf max-range range
                     max-axis axis))
              ;; If the new RANGE is tied, check whether the current
              ;; MAX-AXIS is the same as the splitting axis of the
              ;; parent node. Prefer to split along a different axis
              ;; in the child.
              ((and (= max-range range) (= max-axis parent-axis))
               (push max-axis other-axes)
               (setf max-axis axis))
              ;; If the new RANGE is worse, remember the axis as
              ;; in the fallback list.
              (T
               (push axis other-axes)))))
    (values max-axis other-axes)))

;;; Returns median location component along AXIS of OBJECTS.
(declaim (ftype (function (object-info-vector axis-index)
                          (values single-float (simple-array single-float 1) &optional NIL))
                objects-split-position))
(defun objects-split-position (object-infos axis)
  (declare (optimize speed (safety 1)))
  (let* ((positions (with-axis-dispatch ((component :vector-access)) axis
                      (map '(vector single-float)
                           (lambda (object)
                             (component (object-info-bb-min object)))
                           object-infos)))
         (sorted (copy-seq positions)))
    (locally (declare (inline sort))
      (sort sorted #'<))
    (let* ((length (length sorted))
           (mid (truncate length 2))
           (minimum (aref sorted 0))
           (median (if (oddp length)
                       (aref sorted mid)
                       (* 0.5f0 (+ (aref sorted (+ mid 0))
                                   (aref sorted (+ mid 1))))))
           ;; Objects the position value of which is *strictly* less
           ;; than the computed plane position will go to the near
           ;; node. Consider values of POSITIONS like
           ;; #(1 1 1 1 2 2). In such cases, it is better to move the
           ;; splitting plane position away from the median to avoid
           ;; the minimum value so that objects can be associated with
           ;; the near node (this can fail the other way, that is
           ;; associating all objects with the near node but without
           ;; the adjustment, failure is certain).
           (plane-position (loop for i from mid below length
                                 for value = median then (aref sorted i)
                                 while (= value minimum)
                                 finally (return value))))
      (values plane-position positions))))

(defun partition-objects-into (near-objects far-objects all-objects axis)
  (multiple-value-bind (plane-position positions) (objects-split-position all-objects axis)
    (loop for object across all-objects
          for position across positions
          do (if (< position plane-position)
                 (vector-push-extend object near-objects)
                 (vector-push-extend object far-objects)))
    plane-position))

(defun split-objects (dimension-count parent-axis object-infos bb-min bb-max)
  (multiple-value-bind (best-axis other-axes)
      (best-split-axis dimension-count parent-axis bb-min bb-max)
    (let* ((length (length object-infos))
           (near-objects (make-array (floor length 2) :adjustable T :fill-pointer 0))
           (far-objects (make-array (floor length 2) :adjustable T :fill-pointer 0)))
      (labels ((try-axis (axis other-axes)
                 (let ((plane-position (partition-objects-into
                                near-objects far-objects object-infos axis)))
                   (cond ;; We split successfully, actually modify the node now.
                         ((and (plusp (length near-objects))
                               (plusp (length far-objects)))
                          (values near-objects far-objects axis plane-position))
                         ;; We failed to split and arrived at the
                         ;; initial state. Try another axis.
                         (other-axes
                          (setf (fill-pointer near-objects) 0
                                (fill-pointer far-objects) 0)
                          (try-axis (first other-axes) (rest other-axes)))
                         (T
                          NIL)))))
        (try-axis best-axis other-axes)))))

(defun %enter-all (object-infos dimension-count split-size max-depth)
  (labels ((rec (object-infos parent-axis depth)
             (multiple-value-bind (bb-min bb-max) (compute-bounds-for-objects object-infos)
               (cond  ((< (length object-infos) split-size)
                       (make-leaf-with-bounds object-infos bb-min bb-max))
                      ((not (< depth max-depth))
                       (warn "Unable to split a set of ~:d object~:p because ~
                              depth is at max depth (~d)"
                             (length object-infos) max-depth)
                       (make-leaf-with-bounds object-infos bb-min bb-max))
                      (t
                       (multiple-value-bind (near-objects far-objects axis position)
                           (split-objects dimension-count parent-axis
                                          object-infos bb-min bb-max)
                         (if near-objects
                             (let* ((near (rec near-objects axis (1+ depth)))
                                    (far (rec far-objects axis (1+ depth))))
                               (make-inner-node near far axis position bb-min bb-max))
                             (progn
                               (warn "Unable to split a set of ~:d object~:p ~
                                      because none of the candidate splitting ~
                                      planes separated the objects"
                                     (length object-infos))
                               (make-leaf-with-bounds object-infos bb-min bb-max)))))))))
    (rec object-infos 0 0)))

(declaim (ftype (function (leaf dimension-count axis-index)
                          (values node &optional nil))
                split-node))
(defun split-node (node dimension-count parent-axis)
  (declare (optimize speed (safety 1)))
  (let ((objects (node-objects node))
        (bb-min (node-bb-min node))
        (bb-max (node-bb-max node)))
    (multiple-value-bind (near-objects far-objects axis position)
        (split-objects dimension-count parent-axis objects bb-min bb-max)
      (if near-objects
          (let* ((near (make-leaf near-objects))
                 (far (make-leaf far-objects)))
            (make-inner-node near far axis position bb-min bb-max))
          (progn
            (warn "Unable to split a set of ~:d object~:p because none of ~
                   the candidate splitting planes separated the objects"
                  (length objects))
            (make-leaf-with-bounds objects bb-min bb-max))))))

(declaim (ftype (function (node dimension-count (unsigned-byte 8) (unsigned-byte 8))
                          (values node &optional))
                recompute-subtree))
(defun recompute-subtree (node dimension-count split-size max-depth)
  ;; First retrieve all `object-info's stored below NODE then build a
  ;; new node to replace NODE with.
  (let ((object-infos (make-object-info-vector)))
    (flet ((visit (object-info)
             (vector-push-extend object-info object-infos)))
      (declare (dynamic-extent #'visit))
      (%call-with-all #'visit node))
    (%enter-all object-infos dimension-count split-size max-depth)))

(defun kd-tree-insert (object tree)
  (declare (optimize speed (safety 1)))
  (let* ((dimension-count (kd-tree-dimensions tree))
         (max-depth (kd-tree-max-depth tree))
         (split-size (kd-tree-split-size tree))
         (info (make-object-info object)))
    (with-array (u (object-info-bb-min info))
      (labels ((visit (node parent-axis depth)
                 (declare (type (unsigned-byte 8) depth))
                 (cond ;; Reached a leaf. Push the object into the
                       ;; leaf, then split the leaf if
                       ;; necessary. `split-node' may return NIL in
                       ;; which case the surrounding `visit' call will
                       ;; call `recompute-subtree' to rebuild a larger
                       ;; subtree.
                       ((leaf-p node)
                        (let ((new-object-index (leaf-push-object info node)))
                          (cond ((< new-object-index (1- split-size))
                                 node)
                                ((not (< depth max-depth))
                                 (warn "Unable to split ~a because depth is at max depth (~d)"
                                       node depth)
                                 NIL)
                                (T
                                 (split-node node dimension-count parent-axis)))))
                       ;; If the minimal corner of OBJECT is below the
                       ;; splitting pane, continue in the "near"
                       ;; subtree. The recursive `visit' call returns
                       ;; a (possibly new) node or NIL.
                       ((< (aref u (node-axis node)) (node-position node))
                        (let ((child (visit (node-near node) (node-axis node) (1+ depth))))
                          (cond ((null child) ; tried to split CHILD but failed.
                                 (let ((max-depth (- max-depth depth)))
                                   (recompute-subtree node dimension-count split-size max-depth)))
                                (T ; extended child or new subtree, adjust bounding box
                                 (nexpand-bounds-for-node node child)
                                 (setf (node-near node) child)
                                 node))))
                       ;; Otherwise insert in "far" subtree using the same logic.
                       (T
                        (let ((child (visit (node-far node) (node-axis node) (1+ depth))))
                          (cond ((null child)
                                 (let ((max-depth (- max-depth depth)))
                                   (recompute-subtree node dimension-count split-size max-depth)))
                                (T
                                 (nexpand-bounds-for-node node child)
                                 (setf (node-far node) child)
                                 node)))))))
        (setf (kd-tree-root tree) (visit (kd-tree-root tree) 0 0))))))

(defun kd-tree-remove (object tree)
  (declare (optimize speed (safety 1)))
  (let ((split-size (kd-tree-split-size tree))
        (location (location object))
        (size (bsize object)))
    (with-array (u (v- location size))
      (labels ((merge-nodes (node other-node)
                 (let ((objects (node-objects node))
                       (other-objects (node-objects other-node)))
                   (loop for object across other-objects
                         do (vector-push-extend object objects))
                   (let ((merged (make-leaf-with-bounds
                                  objects (node-bb-min node) (node-bb-max node))))
                     (nexpand-bounds-for-node merged other-node)
                     merged)))
               (visit (node)
                 (cond ;; Reached a leaf. Delete OBJECT from the
                       ;; leaf. `leaf-delete-object' returns NIL
                       ;; if nothing had to be done.
                       ((leaf-p node)
                        (when (leaf-delete-object object node)
                          node))
                       ((< (aref u (node-axis node)) (node-position node))
                        ;; If the recursive `visit' call returns
                        ;; NIL, nothing had to be done. Otherwise,
                        ;; adjust bounding box.
                        (let ((child (visit (node-near node))))
                          (cond ((null child)
                                 NIL)
                                ((and (leaf-p child) (leaf-p (node-far node))
                                      (< (+ (length (node-objects child))
                                            (length (node-objects (node-far node))))
                                         split-size))
                                 (merge-nodes child (node-far node)))
                                (T
                                 (nexpand-bounds-for-node node child)
                                 (setf (node-near node) child)
                                 node))))
                       (T
                        (let ((child (visit (node-far node))))
                          (cond ((null child)
                                 NIL)
                                ((and (leaf-p child) (leaf-p (node-near node))
                                      (< (+ (length (node-objects (node-near node)))
                                            (length (node-objects child)))
                                         split-size))
                                 (merge-nodes child (node-near node)))
                                (T
                                 (nexpand-bounds-for-node node child)
                                 (setf (node-far node) child)
                                 node)))))))
        (declare (dynamic-extent #'visit))
        (let ((new-root (visit (kd-tree-root tree))))
          (when new-root
            (setf (kd-tree-root tree) new-root)))))))

;;; Protocol methods

(defmethod clear ((tree kd-tree))
  (setf (kd-tree-root tree) (make-leaf))
  tree)

(defmethod reoptimize ((tree kd-tree) &key)
  (let ((old-root (kd-tree-root tree))
        (max-depth (kd-tree-max-depth tree))
        (split-size (kd-tree-split-size tree))
        (dimension-count (kd-tree-dimensions tree)))
    (setf (kd-tree-root tree)
          (recompute-subtree old-root dimension-count split-size max-depth)))
  tree)

(defmethod enter (object (tree kd-tree))
  (kd-tree-insert object tree))

(defmethod leave (object (tree kd-tree))
  (kd-tree-remove object tree))

(declaim (ftype (function (function node) (values null &optional))
                %call-with-all))
(defun %call-with-all (function node)
  (declare (optimize speed (safety 1)))
  ;; TODO(jmoringe): everything else uses recursion
  (let ((stack (make-array 0 :adjustable T :fill-pointer T)))
    (declare (dynamic-extent stack))
    (vector-push-extend node stack)
    (loop for node = (vector-pop stack)
          do (cond ((leaf-p node)
                    (loop for info across (node-objects node)
                          for object = (object-info-object info)
                          do (funcall function info)))
                   (T
                    (vector-push-extend (node-near node) stack)
                    (vector-push-extend (node-far node) stack)))
          while (< 0 (length stack)))))

(defmethod call-with-all (function (tree kd-tree))
  (let ((function (ensure-function function)))
    (flet ((visit (info)
             (declare (type object-info info))
             (funcall function (object-info-object info))))
      (declare (dynamic-extent #'visit))
      (%call-with-all #'visit (kd-tree-root tree)))))

(defmethod call-with-candidates (function (container kd-tree) (region region))
  (declare (optimize speed (safety 1)))
  (let ((function (ensure-function function))
        (region-bb-min region)
        (region-bb-max (v+ region (region-size region))))
    (declare (dynamic-extent region-bb-min region-bb-max))
    (flet ((visit (node)
             ;; Test whether the bounding box of NODE overlaps REGION.
             (when (box-intersects-box-p (node-bb-min node) (node-bb-max node)
                                         region-bb-min region-bb-max)
               (loop for info across (node-objects node)
                     do (funcall function (object-info-object info))))))
      (declare (dynamic-extent #'visit))
      (with-array (region-us region-bb-min)
        (with-array (region-vs region-bb-max)
          (%visit-bbox #'visit (kd-tree-root container) region-us region-vs))))))

(defmethod call-with-overlapping (function (container kd-tree) (region region))
  (declare (optimize speed (safety 1)))
  (let ((function (ensure-function function))
        (region-bb-min region)
        (region-bb-max (v+ region (region-size region))))
    (declare (dynamic-extent region-bb-min region-bb-max))
    (flet ((visit (node)
             ;; First test whether the bounding box of NODE overlaps
             ;; REGION.
             (when (box-intersects-box-p (node-bb-min node) (node-bb-max node)
                                         region-bb-min region-bb-max)
               ;; The protocol states that objects which lie
               ;; completely outside of REGION must not be reported so
               ;; we perform a fine test for each object.
               (loop for info across (node-objects node)
                     when (box-intersects-box-p
                           (object-info-bb-min info) (object-info-bb-max info)
                           region-bb-min region-bb-max)
                       do (funcall function (object-info-object info))))))
      (declare (dynamic-extent #'visit))
      (with-array (region-us region-bb-min)
        (with-array (region-vs region-bb-max)
          (%visit-bbox #'visit (kd-tree-root container) region-us region-vs))))))

(defmethod call-with-overlapping (function (container kd-tree) (sphere sphere))
  (declare (optimize speed (safety 1)))
  (with-array (c sphere)
    (let ((function (ensure-function function))
          (v (make-array 3 :element-type 'single-float)))
      (declare (dynamic-extent v))
      (setf (aref v 0) (aref c 0))
      (setf (aref v 1) (aref c 1))
      (setf (aref v 2) (aref c 2))
      (flet ((visit (node)
               (loop for object across (node-objects node)
                     do (funcall function (object-info-object object)))))
        (declare (dynamic-extent #'visit))
        (%visit-sphere #'visit (kd-tree-root container) c (sphere-radius sphere) v)))))

(defmethod call-with-contained (function (container kd-tree) (region region))
  (declare (optimize speed (safety 1)))
  (let ((function (ensure-function function))
        (region-bb-min region)
        (region-bb-max (v+ region (region-size region))))
    (declare (dynamic-extent region-bb-min region-bb-max))
    (flet ((visit (node)
             ;; For objects to be entirely contained in REGION
             ;; or overlap REGION, the bounding box of the
             ;; containing node has to at least overlap the
             ;; region.
             (when (box-intersects-box-p (node-bb-min node) (node-bb-max node)
                                         region-bb-min region-bb-max)
               ;; The protocol states that objects which lie
               ;; completely outside of REGION must not be
               ;; reported so we perform a fine test for each
               ;; object.
               (loop for info across (node-objects node)
                     when (box-contains-box-p
                           (object-info-bb-min info) (object-info-bb-max info)
                           region-bb-min region-bb-max)
                       do (funcall function (object-info-object info))))))
      (declare (dynamic-extent #'visit))
      (with-array (region-us region-bb-min)
        (with-array (region-vs region-bb-max)
          (%visit-bbox #'visit (kd-tree-root container) region-us region-vs))))))

(defmethod call-with-intersecting (function (tree kd-tree) ray-origin ray-direction)
  (declare (optimize speed (safety 1)))
  (let* ((function (ensure-function function))
         (dimension-count (kd-tree-dimensions tree))
         ;; `ray-intersects-box-p' is sensitive to the rank of the ray
         ;; vectors.
         (ray-intersects-box-function
           (ecase dimension-count
             (2
              (check-type ray-origin vec2)
              (check-type ray-direction vec2)
              (locally (declare (type vec2 ray-origin ray-direction))
                (lambda (bb-min bb-max)
                  (declare (type vec3 bb-min bb-max))
                  (let* ((bb-min (vxy bb-min))
                         (bb-max (vxy bb-max)))
                    (ray-intersects-box-p ray-origin ray-direction bb-min bb-max)))))
             (3
              (check-type ray-origin vec3)
              (check-type ray-direction vec3)
              (locally (declare (type vec3 ray-origin ray-direction))
                (lambda (bb-min bb-max)
                  (declare (type vec3 bb-min bb-max))
                  (ray-intersects-box-p ray-origin ray-direction bb-min bb-max)))))))
    (with-array (d ray-direction)
      (labels ((visit (node segment-start)
                 (declare (type node node)
                          (type (or vec2 vec3) segment-start))
                 (cond ((leaf-p node)
                        (when (funcall ray-intersects-box-function
                                       (node-bb-min node) (node-bb-max node))
                          (map NIL (lambda (info)
                                     (when (funcall ray-intersects-box-function
                                                    (object-info-bb-min info)
                                                    (object-info-bb-max info))
                                       (funcall function (object-info-object info))))
                               (node-objects node))))
                       (T
                        (let ((axis (node-axis node))
                              (position (node-position node))
                              (near (node-near node))
                              (far (node-far node)))
                          (with-axis-dispatch ((element   :array-access)
                                               (component :vector-access))
                              axis
                            (let* ((start (component segment-start))
                                   (slope (element d))
                                   (s     (and (not (zerop slope))
                                               (/ (- position start) slope))))
                              (cond ;; ray penetrates splitting plane
                                    ((and s (plusp s))
                                     (cond ((< start position) ; ray start on near side
                                            (when (<= start (component (node-bb-max near)))
                                              (visit near segment-start))
                                            (let ((contact (v+ segment-start (v* ray-direction s))))
                                              (visit far contact)))
                                           (T ; ray start on far side
                                            (let ((contact (v+ segment-start (v* ray-direction s))))
                                              (visit near contact))
                                            (when (>= start (component (node-bb-min far)))
                                              (visit far segment-start)))))
                                    ;; Ray does not penetrate
                                    ;; splitting plane and is not
                                    ;; parallel to splitting plane
                                    (s
                                     (cond ((< start position) ; does not penetrate and on near side
                                            (when (<= start (component (node-bb-max near)))
                                              (visit near segment-start)))
                                           (T ; does not penetrate and on far side
                                            (when (<= start (component (node-bb-max near)))
                                              (visit near segment-start))
                                            (visit far segment-start))))
                                    ;; Ray direction is parallel to
                                    ;; splitting plane
                                    (T
                                     (when (<= start (component (node-bb-max near)))
                                       (visit near segment-start))
                                     #+maybe (let ((contact (v+ segment-start (v* ray-direction s))))
                                               (visit (node-far node) contact))
                                     (when (>= start (component (node-bb-min far)))
                                       (visit far segment-start)))))))))))
        (visit (kd-tree-root tree) ray-origin)))))

(defun kd-tree-call-with-nearest (function location tree)
  (declare (optimize speed (safety 1)))
  (let ((function (ensure-function function)))
    (with-array (v location)
      (with-array (c location)
        (let ((radius most-positive-single-float))
          (declare (type single-float radius))
          (labels ((visit (node)
                     (etypecase node
                       (leaf
                        (loop for info across (node-objects node)
                              for object = (object-info-object info)
                              for distance = (with-array (l (location object))
                                               (sqrdist c l))
                              do (when (< distance radius)
                                   (setf radius (funcall function object distance)))))
                       (inner-node
                        (let ((a (node-near node))
                              (b (node-far node))
                              (axis (node-axis node))
                              (position (node-position node)))
                          (when (< position (aref c axis))
                            (rotatef a b))
                          (visit a)
                          (let ((old (shiftf (aref v axis) position)))
                            (when (or (= radius most-positive-single-float)
                                      (< (sqrdist v c) (* radius radius)))
                              (visit b))
                            (setf (aref v axis) old)))))))
            (visit (kd-tree-root tree))))))))

(defun kd-tree-nearest (location tree &key reject max-radius)
  (declare (optimize speed (safety 1)))
  (let ((radius most-positive-single-float)
        (max-radius (float max-radius 0f0))
        (candidate NIL))
    (flet ((visit (object distance)
             (unless (eq object reject)
               (setf candidate object)
               (setf radius (the single-float distance)))
             radius))
      (declare (dynamic-extent #'visit))
      (kd-tree-call-with-nearest #'visit location tree)
      (when (<= radius max-radius)
        candidate))))

(defun kd-tree-k-nearest (k location tree &key test)
  (declare (optimize speed (safety 1)))
  (check-type k (and (integer 1) (unsigned-byte 32)))
  (let* ((max-i (1- k))
         (candidates (make-array k :element-type T :initial-element NIL))
         (distances (make-array k :element-type 'single-float :initial-element most-positive-single-float))
         (test (if (null test)
                   (constantly T)
                   (ensure-function test)))
         (found 0))
    (declare (type (unsigned-byte 32) max-i found))
    (flet ((visit (candidate distance)
             (declare (type single-float distance))
             (when (and (< distance (aref distances max-i))
                        (funcall test candidate))
               ;; Sorted insertion to ensure that we keep track of the k nearest.
               ;; TODO: I feel like this might be better if we had a doubly linked
               ;;       list instead, since then we could insert more efficiently?
               (incf found)
               (loop for i of-type (unsigned-byte 32) downfrom max-i above 0
                     do (cond ((< distance (aref distances (1- i)))
                               (setf (aref distances i) (aref distances (1- i))
                                     (aref candidates i) (aref candidates (1- i))))
                              (T
                               (setf (aref distances i) distance
                                     (aref candidates i) candidate)
                               (return)))
                     finally (setf (aref distances 0) distance
                                   (aref candidates 0) candidate)))
             (aref distances max-i)))
      (declare (dynamic-extent #'visit))
      (kd-tree-call-with-nearest #'visit location tree)
      (values candidates (min (length candidates) found)))))
