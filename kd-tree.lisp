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
            (:constructor %make-object-info (object group bb-min bb-max))
            (:copier NIL)
            (:predicate NIL))
  (object (error "required") :read-only T)
  (group (error "required") :read-only T)
  ;; Cached object bounding box. Always stored as `vec3'. For
  ;; dimensions < 3, only the first components are accessed.
  (bb-min (error "required") :type vec3)
  (bb-max (error "required") :type vec3))

(declaim (inline object-bounding-box object-bounding-box<-))
(defun object-bounding-box (object)
  (let* ((location (ensure-vec3 (location object)))
         (size/2 (ensure-vec3 (bsize object)))
         (bb-min (v- location size/2))
         (bb-max (v+ location size/2)))
    (values bb-min bb-max)))

(defun object-bounding-box<- (bb-min bb-max object)
  (let* ((location (ensure-vec3 (location object)))
         (size/2 (ensure-vec3 (bsize object))))
    (values (!v- bb-min location size/2)
            (!v+ bb-max location size/2))))

(declaim (inline make-object-info))
(defun make-object-info (object)
  (let ((group (group object)))
    (multiple-value-bind (bb-min bb-max) (object-bounding-box object)
      (%make-object-info object group bb-min bb-max))))

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
  (parent (error "required") :type (or null node))
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
            (:constructor %make-inner-node-with-bounds (parent near far axis position bb-min bb-max))
            (:copier NIL)
            (:predicate NIL))
  ;; Tree structure
  (near (error "required") :type node)
  (far (error "required") :type node)
  ;; Splitting axis
  (axis 0 :type axis-index)
  (position 0.0f0 :type single-float))

(defun make-inner-node (parent near far axis position bb-min bb-max)
  (%make-inner-node-with-bounds parent near far axis position bb-min bb-max))

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

;;; A "seen pairs" vector contains the following elements:
;;; 0       -> (unsigned-byte 32)  generation number
;;; 1       -> array-index         "fill pointer"
;;; 2...end -> node                "other" nodes of seen pairs
;;; The generation number is used to indicate the CALL-WITH-PAIRS call
;;; to which the data in a vector belongs. This way, CALL-WITH-PAIRS
;;; can reset the fill pointer when it encounters an outdated vector.
(deftype seen-pairs-vector ()
  `(simple-array T 1))

(declaim (inline make-seen-pairs-vector))
(defun make-seen-pairs-vector (size generation)
  (let ((array (make-array size :adjustable NIL)))
    (setf (aref array 0) generation)
    array))

(defstruct (leaf
            (:include node)
            (:conc-name node-)
            (:constructor %make-leaf (parent objects group bb-min bb-max))
            (:copier NIL))
  (objects (error "required") :type object-info-vector :read-only T)
  ;; The "seen pairs" vector is used by the call-with-pairs method to
  ;; check whether a given pair of leafs has already been
  ;; processed. After processing a pair, the "other" node of the pair
  ;; is pushed onto the vector of "this" node. Using 0 as the
  ;; generation marks the vector as outdated.
  (seen-pairs (make-seen-pairs-vector 16 0) :type seen-pairs-vector)
  (group (error "required")))

(defun common-group (object-infos)
  (loop with candidate = (object-info-group (aref object-infos 0))
        for i from 1 below (length object-infos)
        for group = (object-info-group (aref object-infos i))
        unless (eq group candidate)
          do (return NIL)
        finally (return candidate)))

(declaim (ftype (function ((or null inner-node) &optional object-info-vector) (values node &optional NIL))
                make-leaf))
(defun make-leaf (parent &optional object-infos)
  (multiple-value-bind (min max) (compute-bounds-for-objects object-infos)
    (if object-infos
        (let ((group (common-group object-infos)))
          (%make-leaf parent object-infos group min max))
        (%make-leaf parent (make-object-info-vector) NIL min max))))

(defun make-leaf-with-bounds (parent object-infos bb-min bb-max)
  (let ((group (if (plusp (length object-infos))
                   (common-group object-infos)
                   NIL)))
    (%make-leaf parent object-infos group bb-min bb-max)))

(defmethod print-object ((node leaf) stream)
  (print-unreadable-object (node stream :type T)
    (format stream "leaf (~d object~:p)" (length (node-objects node)))))

(defun leaf-push-object (object-info leaf)
  (let ((node-objects (node-objects leaf))
        (object-group (object-info-group object-info)))
    (cond ((zerop (length node-objects))
           (setf (node-group leaf) object-group))
          ((not (eq object-group (node-group leaf)))
           (setf (node-group leaf) NIL)))
    ;; Return the index of the stored info. The caller can use this to
    ;; decide whether the leaf should be split.
    (prog1
        (vector-push-extend object-info node-objects)
      (nexpand-bounds-for-object (node-bb-min leaf) (node-bb-max leaf) object-info))))

(defun leaf-delete-object (object leaf)
  (let* ((objects (node-objects leaf))
         (position (position object objects :key #'object-info-object)))
    (when position
      (loop for i from position below (1- (length objects))
            do (setf (aref objects i) (aref objects (1+ i))))
      (decf (fill-pointer objects))
      ;; After removing OBJECT, all remaining objects could have a
      ;; common group.
      (when (null (node-group leaf))
        (setf (node-group leaf) (common-group objects)))
      ;; TODO(jmoringe): could check whether object was part of the
      ;; support of the bounding box and skip the re-computation if it
      ;; was not.
      (multiple-value-bind (bb-min bb-max) (compute-bounds-for-objects objects)
        (v<- (node-bb-min leaf) bb-min)
        (v<- (node-bb-max leaf) bb-max))
      T)))

(declaim (inline node-seen-pairs*))
(defun node-seen-pairs* (node generation)
  (declare (type (unsigned-byte 32) generation))
  (declare (optimize speed (safety 1)))
  (let* ((vector (node-seen-pairs node))
         (vector-generation (aref vector 0)))
    (declare (type (unsigned-byte 32) vector-generation))
    ;; Clear the vector if its contents is from a previous
    ;; "generation" which means an earlier CALL-WITH-PAIRS call than
    ;; the current one.
    (when (/= vector-generation generation)
      (setf (aref vector 0) generation
            (aref vector 1) 2))
    vector))

(defun push-seen-node (other-node node generation)
  (declare (optimize speed (safety 1)))
  (let* ((vector (node-seen-pairs* node generation))
         (fill-pointer (aref vector 1)))
    (declare (type seen-pairs-vector vector)
             (type (unsigned-byte 32) fill-pointer)) ; arbitrary; large enough
    (when (= fill-pointer (length vector))
      (let* ((generation (aref vector 0))
             (new-vector (make-seen-pairs-vector (* 2 fill-pointer) generation)))
        (format *trace-output* "Generation ~D, resizing ~D -> ~D~%"
                generation fill-pointer (* 2 fill-pointer))
        (setf (subseq new-vector 2 fill-pointer) (subseq vector 2))
        (setf (node-seen-pairs node) new-vector
              vector new-vector)))
    (assert (>= fill-pointer 2))
    (setf (aref vector fill-pointer) other-node)
    (setf (aref vector 1) (1+ fill-pointer))))

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
  (object->node (make-hash-table :test #'eq) :type hash-table :read-only T)
  ;; GENERATION counts the CALL-WITH-PAIRS calls in order to reset the
  ;; "seen pairs" vectors in leaf nodes.
  (generation 0 :type (unsigned-byte 32)))

(defun make-kd-tree (&key (dimensions 3) (split-size 8) (max-depth 255))
  (check-type dimensions dimension-count)
  (assert (< 1 split-size))
  (assert (< max-depth 256))
  (%make-kd-tree dimensions split-size max-depth (make-leaf nil)))

(defmethod print-object ((tree kd-tree) stream)
  (print-unreadable-object (tree stream :type T)
    (format stream "~d (~d object~:p)"
            (kd-tree-dimensions tree)
            (object-count tree))))

(defmethod describe-object ((tree kd-tree) stream)
  (call-next-method)
  (format stream "~&~%-------------------------~%")
  (org.shirakumo.text-draw:tree
   (kd-tree-root tree)
   (lambda (node)
     (unless (leaf-p node)
       (list (node-near node) (node-far node))))
   :stream stream))

(defmethod object-count ((container kd-tree))
  (hash-table-count (kd-tree-object->node container)))

(defun next-generation (tree)
  (setf (kd-tree-generation tree)
        (mod (1+ (kd-tree-generation tree)) (ash 1 32))))

(defun %visit-sphere (f node center radius volume)
  (declare (optimize speed (safety 1)))
  (declare (type (simple-array single-float (3)) center volume))
  (declare (type single-float radius))
  (declare (type function f))
  (declare (type node node))
  (labels ((visit (node)
             (cond ((leaf-p node)
                    (funcall f node))
                   (T
                    (let ((axis (node-axis node))
                          (position (node-position node))
                          (near (node-near node))
                          (far (node-far node)))
                      (when (< position (aref center axis))
                        (rotatef near far))
                      (visit near)
                      (let ((old (shiftf (aref volume axis) position)))
                        ;; If the splitting axis is within the radius, also check
                        ;; the other side
                        (when (< (sqrdist volume center) (* radius radius))
                          (visit far))
                        (setf (aref volume axis) old)))))))
    (visit node)))

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

(declaim (inline register-object-infos))
(defun register-object-infos (tree object-infos node)
  (let ((object->node (kd-tree-object->node tree)))
    (loop for info across object-infos
          for object = (object-info-object info)
          do (setf (gethash object object->node) node)))
  node)

(defun %enter-all (object-infos tree dimension-count split-size max-depth)
  (labels ((rec (object-infos parent parent-axis depth)
             (multiple-value-bind (bb-min bb-max) (compute-bounds-for-objects object-infos)
               (cond  ((< (length object-infos) split-size)
                       (register-object-infos
                        tree object-infos
                        (make-leaf-with-bounds parent object-infos bb-min bb-max)))
                      ((not (< depth max-depth))
                       (warn "~S: Unable to split a set of ~:d object~:p because ~
                              depth is at max depth (~d)"
                             '%enter-all (length object-infos) max-depth)
                       (register-object-infos
                        tree object-infos
                        (make-leaf-with-bounds parent object-infos bb-min bb-max)))
                      (t
                       (multiple-value-bind (near-objects far-objects axis position)
                           (split-objects dimension-count parent-axis
                                          object-infos bb-min bb-max)
                         (if near-objects
                             (let* ((near (rec near-objects nil axis (1+ depth)))
                                    (far (rec far-objects nil axis (1+ depth)))
                                    (node (make-inner-node parent near far axis position bb-min bb-max)))
                               (setf (node-parent near) node
                                     (node-parent far) node)
                               node)
                             (progn
                               (warn "~S: Unable to split a set of ~:d object~:p ~
                                      because none of the candidate splitting ~
                                      planes separated the objects"
                                     '%enter-all (length object-infos))
                               (register-object-infos
                                tree object-infos
                                (make-leaf-with-bounds parent object-infos bb-min bb-max))))))))))
    (rec object-infos nil 0 0)))

(declaim (ftype (function (kd-tree (or null inner-node) leaf dimension-count axis-index)
                          (values node &optional nil))
                split-node))
(defun split-node (tree parent node dimension-count parent-axis)
  (declare (optimize speed (safety 1)))
  (let ((objects (node-objects node))
        (bb-min (node-bb-min node))
        (bb-max (node-bb-max node)))
    (multiple-value-bind (near-objects far-objects axis position)
        (split-objects dimension-count parent-axis objects bb-min bb-max)
      (if near-objects
          (let* ((near (register-object-infos
                        tree near-objects (make-leaf nil near-objects)))
                 (far (register-object-infos
                       tree far-objects (make-leaf nil far-objects)))
                 (node (make-inner-node parent near far axis position bb-min bb-max)))
            (setf (node-parent near) node
                  (node-parent far) node)
            node)
          (progn
            (warn "~S: Unable to split a set of ~:d object~:p because none of ~
                   the candidate splitting planes separated the objects"
                  'split-node (length objects))
            (register-object-infos
             tree objects
             (make-leaf-with-bounds parent objects bb-min bb-max)))))))

(declaim (ftype (function (node kd-tree dimension-count (unsigned-byte 8) (unsigned-byte 8))
                          (values node &optional))
                recompute-subtree))
(defun recompute-subtree (node tree dimension-count split-size max-depth)
  ;; First retrieve all `object-info's stored below NODE then build a
  ;; new node to replace NODE with.
  (let ((object-infos (make-object-info-vector)))
    (labels ((visit (node)
               (typecase node
                 (leaf
                  (loop for object-info across (node-objects node)
                        do (vector-push-extend object-info object-infos)))
                 (inner-node
                  (visit (node-near node))
                  (visit (node-far node))))))
      (declare (dynamic-extent #'visit))
      (visit node))
    (%enter-all object-infos tree dimension-count split-size max-depth)))

(defun %kd-tree-insert (object info tree)
  (declare (optimize speed (safety 1)))
  (let* ((dimension-count (kd-tree-dimensions tree))
         (max-depth (kd-tree-max-depth tree))
         (split-size (kd-tree-split-size tree)))
    (with-array (u (object-info-bb-min info))
      (labels ((visit (node parent parent-axis depth)
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
                                 (setf (gethash object (kd-tree-object->node tree)) node)
                                 node)
                                ((not (< depth max-depth))
                                 (warn "~A: Unable to split ~a because depth is at max depth (~d)"
                                       '%kd-tree-insert node depth)
                                 NIL)
                                (T
                                 (split-node tree parent node dimension-count parent-axis)))))
                       ;; If the minimal corner of OBJECT is below the
                       ;; splitting pane, continue in the "near"
                       ;; subtree. The recursive `visit' call returns
                       ;; a (possibly new) node or NIL.
                       ((< (aref u (node-axis node)) (node-position node))
                        (let ((child (visit (node-near node) node (node-axis node) (1+ depth))))
                          (cond ((null child) ; tried to split CHILD but failed.
                                 (let ((max-depth (- max-depth depth)))
                                   (when (< 1 max-depth)
                                     (recompute-subtree node tree dimension-count split-size max-depth))))
                                (T ; extended child or new subtree, adjust bounding box
                                 (nexpand-bounds-for-node node child)
                                 (setf (node-near node) child)
                                 node))))
                       ;; Otherwise insert in "far" subtree using the same logic.
                       (T
                        (let ((child (visit (node-far node) node (node-axis node) (1+ depth))))
                          (cond ((null child)
                                 (let ((max-depth (- max-depth depth)))
                                   (when (< 1 max-depth)
                                     (recompute-subtree node tree dimension-count split-size max-depth))))
                                (T
                                 (nexpand-bounds-for-node node child)
                                 (setf (node-far node) child)
                                 node)))))))
        (setf (kd-tree-root tree) (visit (kd-tree-root tree) nil 0 0))))))

(defun kd-tree-insert (object tree)
  (declare (optimize speed (safety 1)))
  (let ((info (make-object-info object)))
    (%kd-tree-insert object info tree)))

(defun %kd-tree-remove (object leaf tree)
  (declare (optimize speed (safety 1)))
  (assert (leaf-delete-object object leaf))
  (remhash object (kd-tree-object->node tree))
  (labels ((merge-nodes (parent node other-node)
             (let ((objects (node-objects node))
                   (other-objects (node-objects other-node)))
               (loop for object across other-objects
                     do (vector-push-extend object objects))
               (let ((merged (make-leaf-with-bounds
                              parent objects (node-bb-min node) (node-bb-max node))))
                 (nexpand-bounds-for-node merged other-node)
                 (register-object-infos tree objects merged))))
           (visit (node child which)
             (let* ((parent (node-parent node))
                    (other (ecase which
                             (:near (node-far node))
                             (:far  (node-near node))))
                    (child-object-count (when (leaf-p child)
                                          (length (node-objects child))))
                    (other-object-count (when (leaf-p other)
                                          (length (node-objects other)))))
               ;; If the combined object counts of the two children of
               ;; NODE are below the split size, NODE should be
               ;; replaced with a new leaf node that is the result of
               ;; merging the two children. Otherwise updating the
               ;; bounding box of NODE and replacing one of its
               ;; children is sufficient.
               (cond ((eql child-object-count 0)
                      (setf (node-parent other) parent)
                      (visit-parent parent node other))
                     ((eql other-object-count 0)
                      (setf (node-parent child) parent)
                      (visit-parent parent node child))
                     ((and child-object-count other-object-count
                           (< (+ child-object-count other-object-count)
                              (kd-tree-split-size tree)))
                      (let ((new-node (merge-nodes parent child other)))
                        (visit-parent parent node new-node)))
                     (T
                      (v<- (node-bb-min node) (node-bb-min other))
                      (v<- (node-bb-max node) (node-bb-max other))
                      (nexpand-bounds-for-node node child)
                      (ecase which
                        (:near (setf (node-near node) child))
                        (:far (setf (node-far node) child)))
                      (visit-parent parent node node)))))
           (visit-parent (parent old-child new-child)
             (if parent
                 (let ((which (if (eq old-child (node-near parent)) :near :far)))
                   (visit parent new-child which))
                 (setf (kd-tree-root tree) new-child))))
    (visit-parent (node-parent leaf) leaf leaf)))

(defun kd-tree-remove (object tree)
  (declare (optimize speed (safety 1)))
  (let ((leaf (gethash object (kd-tree-object->node tree))))
    (when leaf
      (%kd-tree-remove object leaf tree))))

(defun kd-tree-update (object tree)
  (declare (optimize speed (safety 1)))
  (let* ((object->node (kd-tree-object->node tree))
         (leaf         (gethash object object->node)))
    (if (null leaf)
        (kd-tree-insert object tree)
        (let* ((info   (find object (node-objects leaf)
                             :test #'eq :key #'object-info-object))
               (bb-min (object-info-bb-min info))
               (bb-max (object-info-bb-max info)))
          (object-bounding-box<- bb-min bb-max object)
          (when (not (and (v<= (node-bb-min leaf) bb-min)
                          (v<= bb-max (node-bb-max leaf))))
            ;; TODO(jmoringe): walk up from current node until
            ;; contained in bounding box, then walk down
            (%kd-tree-remove object leaf tree)
            (%kd-tree-insert object info tree))))))

;;; Protocol methods

(defmethod clear ((tree kd-tree))
  (clrhash (kd-tree-object->node tree))
  (setf (kd-tree-root tree) (make-leaf nil))
  tree)

(defmethod reoptimize ((tree kd-tree) &key)
  (let ((old-root (kd-tree-root tree))
        (max-depth (kd-tree-max-depth tree))
        (split-size (kd-tree-split-size tree))
        (dimension-count (kd-tree-dimensions tree)))
    (setf (kd-tree-root tree)
          (recompute-subtree old-root tree dimension-count split-size max-depth)))
  tree)

(defmethod enter (object (tree kd-tree))
  ;; We must allow the same object to be inserted multiple
  ;; times. `kd-tree-update' has the correct behavior.
  (kd-tree-update object tree))

(defmethod leave (object (tree kd-tree))
  (kd-tree-remove object tree))

(defmethod update (object (tree kd-tree))
  (kd-tree-update object tree))

(defmethod call-with-all (function (tree kd-tree))
  (let ((function (ensure-function function)))
    (maphash (lambda (object node)
               (declare (ignore node))
               (funcall function object))
             (kd-tree-object->node tree))))

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

(declaim (ftype (function (function kd-tree vec3 vec3))
                %call-with-nodes-overlapping-region))
(defun %call-with-nodes-overlapping-region (function container region-bb-min region-bb-max)
  (declare (optimize speed (safety 1)))
  (flet ((visit (node)
           ;; First test whether the bounding box of NODE overlaps
           ;; REGION.
           (when (box-intersects-box-p (node-bb-min node) (node-bb-max node)
                                       region-bb-min region-bb-max)
             (funcall function node))))
    (declare (dynamic-extent #'visit))
    (with-array (region-us region-bb-min)
      (with-array (region-vs region-bb-max)
        (%visit-bbox #'visit (kd-tree-root container) region-us region-vs)))))

(defmethod call-with-overlapping (function (container kd-tree) (region region))
  (declare (optimize speed (safety 1)))
  (let ((function (ensure-function function))
        (region-bb-min region)
        (region-bb-max (v+ region (region-size region))))
    (declare (dynamic-extent region-bb-min region-bb-max))
    (flet ((visit-node (node)
             ;; The protocol states that objects which lie completely
             ;; outside of REGION must not be reported so we perform a
             ;; fine test for each object.
             (loop for info across (node-objects node)
                   when (box-intersects-box-p
                         (object-info-bb-min info) (object-info-bb-max info)
                         region-bb-min region-bb-max)
                     do (funcall function (object-info-object info)))))
      (declare (dynamic-extent #'visit-node))
      (%call-with-nodes-overlapping-region
       #'visit-node container region-bb-min region-bb-max))))

(defmethod call-with-overlapping (function (container kd-tree) (region sphere))
  (declare (optimize speed (safety 1)))
  (let ((function (ensure-function function))
        (radius (sphere-radius region)))
    (flet ((visit-node (node)
             ;; For objects to overlap REGION, the bounding box of the
             ;; containing node has to at least overlap the region.
             (when (sphere-intersects-box-p (node-bb-min node) (node-bb-max node)
                                            region radius)
               ;; The protocol states that objects which lie
               ;; completely outside of REGION must not be reported so
               ;; we perform a fine test for each object.
               (loop for info across (node-objects node)
                     when (sphere-intersects-box-p
                           (object-info-bb-min info) (object-info-bb-max info)
                           region radius)
                       do (funcall function (object-info-object info))))))
      (declare (dynamic-extent #'visit-node))
      (with-array (c region)
        (let ((v (make-array 3 :element-type 'single-float)))
          (declare (dynamic-extent v))
          (setf (aref v 0) (aref c 0))
          (setf (aref v 1) (aref c 1))
          (setf (aref v 2) (aref c 2))
          (%visit-sphere #'visit-node (kd-tree-root container) c radius v))))))

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

(defmethod call-with-contained (function (container kd-tree) (region sphere))
  (declare (optimize speed (safety 1)))
  (let ((function (ensure-function function))
        (radius (sphere-radius region)))
    (flet ((visit (node)
             ;; For objects to be entirely contained in REGION
             ;; or overlap REGION, the bounding box of the
             ;; containing node has to at least overlap the
             ;; region.
             (when (sphere-intersects-box-p (node-bb-min node) (node-bb-max node)
                                            region radius)
               ;; The protocol states that objects which lie
               ;; completely outside of REGION must not be
               ;; reported so we perform a fine test for each
               ;; object.
               (loop for info across (node-objects node)
                     when (sphere-intersects-box-p
                           (object-info-bb-min info) (object-info-bb-max info)
                           region radius)
                     do (funcall function (object-info-object info))))))
      (declare (dynamic-extent #'visit))
      (with-array (c region)
        (let ((v (make-array 3 :element-type 'single-float)))
          (declare (dynamic-extent v))
          (setf (aref v 0) (aref c 0))
          (setf (aref v 1) (aref c 1))
          (setf (aref v 2) (aref c 2))
          (%visit-sphere #'visit (kd-tree-root container) c radius v))))))

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
                    (declare (dynamic-extent bb-min bb-max))
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
                                              (visit near segment-start))
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

(defmethod call-with-pairs (function (container kd-tree))
  (declare (optimize speed (safety 1)))
  (let ((function (ensure-function function))
        (generation (next-generation container)))
    (labels ((visit (node)
               (labels ((visit-pairs (info other-node start)
                          (loop with objects = (node-objects other-node)
                                for i from start below (length objects)
                                for other-info = (aref objects i)
                                when (and (not (eq info other-info))
                                          (not (and (object-info-group info)
                                                    (eq (object-info-group info)
                                                        (object-info-group other-info))))
                                          (box-intersects-box-p
                                           (object-info-bb-min info)
                                           (object-info-bb-max info)
                                           (object-info-bb-min other-info)
                                           (object-info-bb-max other-info)))
                                  do (funcall function
                                              (object-info-object info)
                                              (object-info-object other-info))))
                        (visit-overlapping (other-node)
                          (declare (type leaf other-node))
                          (unless (eq other-node node)
                            (let ((group (node-group node))
                                  (other-group (node-group other-node)))
                              (unless (or (and group (eq group other-group))
                                          (let ((seen (node-seen-pairs* node generation)))
                                            (find other-node seen
                                                  :start 2 :end (aref seen 1) :test #'eq)))
                                (push-seen-node node other-node generation)
                                (loop for info across (node-objects node)
                                      when (box-intersects-box-p
                                            (object-info-bb-min info)
                                            (object-info-bb-max info)
                                            (node-bb-min other-node)
                                            (node-bb-max other-node))
                                        do (visit-pairs info other-node 0)))))))
                 (declare (dynamic-extent #'visit-pairs #'visit-overlapping))
                 (typecase node
                   (leaf
                    ;; Objects within NODE.
                    (unless (node-group node) ; common group means no pairs
                      (loop for i of-type fixnum from 0
                            for info across (node-objects node)
                            do (visit-pairs info node (1+ i))))
                    ;; Objects in other nodes.
                    (%call-with-nodes-overlapping-region
                     #'visit-overlapping container (node-bb-min node) (node-bb-max node))
                    nil) ; fixed return type for SBCL
                   (inner-node
                    (visit (node-near node))
                    (visit (node-far node)))))))
      (declare (dynamic-extent #'visit))
      (visit (kd-tree-root container)))))

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

(defun kd-tree-nearest (location tree &key (test (constantly T)) max-radius)
  (declare (optimize speed (safety 1)))
  (declare (type (function (T) (values boolean &optional)) test))
  (let ((radius most-positive-single-float)
        (max-radius (float (or max-radius most-positive-single-float) 0f0))
        (candidate NIL))
    (flet ((visit (object distance)
             (when (funcall test object)
               (setf candidate object)
               (setf radius (the single-float distance)))
             radius))
      (declare (dynamic-extent #'visit))
      (kd-tree-call-with-nearest #'visit location tree)
      (when (<= radius max-radius)
        candidate))))

;; TODO: allow K to be an array
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
    (declare (dynamic-extent distances))
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

(defun check-consistency (tree)
  (let ((object->node (kd-tree-object->node tree))
        (nodes        (make-hash-table :test #'eq))
        (object-count 0))
    (labels ((check-node (node)
               (assert (not (gethash node nodes)))
               (setf (gethash node nodes) t)
               (etypecase node
                 (leaf
                  (let ((objects (node-objects node)))
                    (incf object-count (length objects))
                    (loop for info across objects
                          for object = (object-info-object info)
                          do (assert (eq (gethash object object->node) node)))))
                 (inner-node
                  (let ((near (node-near node))
                        (far  (node-far node)))
                    (assert (eq node (node-parent near)))
                    (assert (eq node (node-parent far)))
                    (check-node near)
                    (check-node far))))))
      (check-node (kd-tree-root tree)))
    (assert (= (hash-table-count object->node) object-count))))
