(in-package #:org.shirakumo.fraf.trial.space.bsp)

(defstruct cc-face
  "A face in the CC-MESH"
  ;; Verts produced by this face.
  (verts (make-array 0 :element-type '(unsigned-byte 32) :fill-pointer t :adjustable t) :type (vector (unsigned-byte 32)))
  (px (error "missing cc-face-px") :type single-float)
  (py (error "missing cc-face-py") :type single-float)
  (pz (error "missing cc-face-pz") :type single-float)
  (pd (error "missing cc-face-pd") :type single-float)
  (user-data (error "missing cc-face-user-data") :type (unsigned-byte 32)))

;; Note:
;; The slowest part of BSP building is constructing a CC-MESH for each
;; leaf node. The arrays on this struct (and on CC-FACE) are not
;; simple arrays, meaning we get a dynamic dispatch for each array
;; access. Converting these to simple arrays using a similar technique
;; to that in TRI-BUCKET may result in performance improvements to
;; building
(defstruct cc-mesh
  "Represent a connected convex mesh."
  ;; 3 floats per vert
  (verts (make-array 0 :element-type 'single-float :fill-pointer t :adjustable t) :type (vector single-float))
  ;; 2 vert-indices per edge. Edges are not doubled.
  (edges (make-array 0 :element-type '(unsigned-byte 32) :fill-pointer t :adjustable t) :type (vector (unsigned-byte 32)))
  ;; 2 face-indices per edge. Not doubled.
  (edge-faces (make-array 0 :element-type '(unsigned-byte 32) :fill-pointer t :adjustable t) :type (vector (unsigned-byte 32)))
  (faces (make-array 0 :element-type 'cc-face :fill-pointer t :adjustable t) :type (vector cc-face)))

(defstruct mesh-input-data
  (mesh (error "Missing field MESH-INPUT-DATA-MESH") :type mesh)
  ;; Extra arbitrary user data which will be passed through to the BSP leaves.
  (user-data nil :type t))

(defstruct bsp-build-state
  ;; List of MESH-INPUT-DATA
  (meshes (list)))

(defstruct (bsp
            (:include container)
            (:constructor %make-bsp)
            (:copier nil)
            (:predicate nil))
  ;; The epsilon this bsp was build with, used for queries
  (eps (error "bsp-eps missing") :type single-float)
  ;; Used during building, nil when deserializing
  (build-state (make-bsp-build-state) :type (or null bsp-build-state))
  ;; User data stripped from the meshes in an array which is referenced by the BSP nodes after building
  (user-data-array (make-array 0) :type (simple-array t))
  (root nil :type (or null bsp-node)))

(defstruct bsp-node
  ;; Plane should be ignored for leaf nodes
  (px 0.0 :type single-float)
  (py 0.0 :type single-float)
  (pz 0.0 :type single-float)
  (pd 0.0 :type single-float)
  (front nil :type (or bsp-node null))
  (behind nil :type (or bsp-node null))
  (parent nil :type (or bsp-node null))
  ;; Used during building to compute bevelling planes; not serialized.
  (cc-mesh nil :type (or cc-mesh null))
  ;; CC-MESH converted to a tri mesh - this IS serialized and will
  ;; always be non-NIL after bsp building
  (tri-mesh nil :type (or mesh null))
  ;; Ignore when not a leaf
  (solid-p nil :type boolean)
  ;; Used during serialization, ignore otherwise
  (table-index 0 :type fixnum)
  ;; For leaf nodes, this is an array of extra user data from the mesh *per face*. This is the length of
  ;; (/ (mesh-faces tri-mesh) 3). Each index references a value in BSP-USER-DATA-ARRAY.
  ;;
  ;; For branch nodes, this is the user data of the plane corresponding to that node.
  (user-data nil :type (or null (unsigned-byte 32) (simple-array (unsigned-byte 32)))))

(defmethod print-object ((b bsp-node) str)
  (print-unreadable-object (b str :type t :identity t)
    (if (bsp-node-leaf-p b)
        (format str "LEAF ~s" (bsp-node-solid-p b))
        (format str "~d ~d ~d ~d" (bsp-node-px b) (bsp-node-py b) (bsp-node-pz b) (bsp-node-pd b)))))

(defun bsp-node-leaf-p (bsp-node)
  (and
   (not (bsp-node-front bsp-node))
   (not (bsp-node-behind bsp-node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Math helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (single-float single-float single-float
                           single-float single-float single-float)
                          single-float)
                dot)
         (inline dot))
(defun dot (x0 y0 z0 x1 y1 z1)
  (declare (type single-float x0 y0 z0 x1 y1 z1))
  (+ (* x0 x1) (* y0 y1) (* z0 z1)))

(declaim (ftype (function (single-float single-float single-float
                           single-float single-float single-float)
                          (values single-float single-float single-float))
                cross)
         (inline cross))
(defun cross (x0 y0 z0 x1 y1 z1)
  (declare (type single-float x0 y0 z0 x1 y1 z1))
  (values
   (- (* y0 z1) (* z0 y1))
   (- (* z0 x1) (* x0 z1))
   (- (* x0 y1) (* y0 x1))))

(defun tri-normal (x0 y0 z0 x1 y1 z1 x2 y2 z2 eps)
  "Compute the normal of a tri with counter-clockwise winding. Returns
(VALUES NX NY NZ) or (VALUES 0 0 0) if tri is degenerate (the cross-product vector is smaller than eps)."
  (let* ((dx2 (- x2 x0))
         (dy2 (- y2 y0))
         (dz2 (- z2 z0))
         (dx1 (- x1 x0))
         (dy1 (- y1 y0))
         (dz1 (- z1 z0))
         (cx (- (* dy1 dz2) (* dz1 dy2)))
         (cy (- (* dz1 dx2) (* dx1 dz2)))
         (cz (- (* dx1 dy2) (* dy1 dx2)))
         (len (sqrt (+ (* cx cx) (* cy cy) (* cz cz)))))
    (if (< (abs len) eps)
        (values 0.0 0.0 0.0)
        (values (/ cx len) (/ cy len) (/ cz len)))))

(defun intersect-ray-plane-t (x y z dx dy dz nx ny nz d)
  "Intersect a ray ((X, Y, Z), (DX, DY, DZ)) against a (normalized) plane NX NY NZ D

DX, DY, DZ does not need to be normalized

Returns (VALUES TIME DIST) (or (VALUES NIL DIST) if the line and plane are parallel)

TIME is the time that the ray intersects
DIST is the signed distance of the ray origin from the plane"
  (let* ((l-len (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))
         ;; normalize d
         (dx (/ dx l-len))
         (dy (/ dy l-len))
         (dz (/ dz l-len))
         (d-dot-n (dot dx dy dz nx ny nz)) ;; denom
         (signed-dis-plane (- d (dot x y z nx ny nz)))) ;; signed distance from (x, y, z) to plane
    ;; When line + plane are parallel, return NIL
    (if (< (abs d-dot-n) 0.0000001)
        (values nil signed-dis-plane)
        (let ((intersect-t (/ signed-dis-plane d-dot-n)))
          (values intersect-t signed-dis-plane)))))

(defun intersect-line-plane (x0 y0 z0 x1 y1 z1 nx ny nz d)
  "Intersect a line (l0, l1) against a (normalized) plane NX NY NZ D

Returns (VALUES X Y Z). The intersection might be outside of (l0, l1)."
  (declare (type single-float x0 y0 z0 x1 y1 z1 nx ny nz d))
  (let* ((lx (- x1 x0))
         (ly (- y1 y0))
         (lz (- z1 z0))
         (l-len (sqrt (+ (* lx lx) (* ly ly) (* lz lz))))
         (d (/ (intersect-ray-plane-t x0 y0 z0 lx ly lz nx ny nz d) l-len)))
    (values (+ x0 (* d lx)) (+ y0 (* d ly)) (+ z0 (* d lz)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tri-bucket
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct tri-bucket
  "This is basically a resizable float array with extra functions for
operating on triangles. This avoids using an adjustable array with a
fill pointer, which lisp types as a VECTOR rather than SIMPLE-ARRAY -
this inhibits a bunch of compiler optimizations like inlining AREF."
  (data (make-array 0 :element-type 'single-float) :type (simple-array single-float))
  ;; Length of DATA in floats. The real length of the DATA array will
  ;; typically be larger than this.
  (len 0 :type fixnum)
  ;; Extra data associated with this tri bucket, which will be passed
  ;; down to the leaf node and used to construct the USER-DATA array.
  (user-data #xffffffff :type (unsigned-byte 32)))

(defun tri-bucket-append-tri (bucket x0 y0 z0 x1 y1 z1 x2 y2 z2)
  "Append a single tri to a TRI-BUCKET. Allocates more memory if needed."
  (tri-bucket-ensure-n-floats bucket (+ (tri-bucket-len bucket) 9))
  (let ((arr (tri-bucket-data bucket))
        (len (1- (tri-bucket-len bucket))))
    (setf (aref arr (incf len)) x0)
    (setf (aref arr (incf len)) y0)
    (setf (aref arr (incf len)) z0)
    (setf (aref arr (incf len)) x1)
    (setf (aref arr (incf len)) y1)
    (setf (aref arr (incf len)) z1)
    (setf (aref arr (incf len)) x2)
    (setf (aref arr (incf len)) y2)
    (setf (aref arr (incf len)) z2)
    (setf (tri-bucket-len bucket) (+ 1 len))))

(defun tri-bucket-append-bucket (bucket other)
  "Appends one TRI-BUCKET to another. Appends OTHER to the end of
BUCKET. Allocates more memory if needed."
  (let ((len (tri-bucket-len bucket))
        (other-len (tri-bucket-len other)))
    (tri-bucket-ensure-n-floats bucket (+ len other-len))
    (replace (tri-bucket-data bucket) (tri-bucket-data other) :start1 len :start2 0 :end1 (+ len other-len) :end2 other-len)
    (setf (tri-bucket-len bucket) (+ len other-len))))

(defun tri-bucket-ensure-n-floats (bucket n)
  "Ensure there is space in a TRI-BUCKET for N floats total. Allocates
a new array and copies the old one over if there is not."
  (let ((new-len (length (tri-bucket-data bucket))))
    (loop while (< new-len n)
          do (setf new-len (max (* 9 32) (floor (* new-len 1.5)))))
    (when (< (length (tri-bucket-data bucket)) new-len )
      (let ((arr (make-array n :element-type 'single-float)))
        (replace arr (tri-bucket-data bucket) :end1 (tri-bucket-len bucket) :end2 (tri-bucket-len bucket))
        (setf (tri-bucket-data bucket) arr)))))

(declaim (ftype (function (tri-bucket)) tri-bucket-clear))
(defun tri-bucket-clear (bucket)
  "Clear the tri bucket, retaining the previous allocation."
  (setf (tri-bucket-len bucket) 0))

(declaim (ftype (function (tri-bucket) fixnum) tri-bucket-len-tris))
(defun tri-bucket-len-tris (bucket)
  "Get the length of a TRI-BUCKET *in tris*, rather than floats or verts.."
  (declare (type tri-bucket bucket))
  (/ (tri-bucket-len bucket) 9))

(defun mesh-to-tri-bucket (mesh)
  "Convert a MESH to a TRI-BUCKET - this goes through the mesh and
expands out the faces into a single array of tris."
  (loop
    with ret = (let ((bucket (make-tri-bucket)))
                 (tri-bucket-ensure-n-floats bucket (* 3 (length (mesh-faces mesh))))
                 bucket)
    with verts = (mesh-vertices mesh)
    with faces = (mesh-faces mesh)
    for ii below (/ (length faces) 3)
    for f0 = (aref faces (+ 0 (* ii 3)))
    for f1 = (aref faces (+ 1 (* ii 3)))
    for f2 = (aref faces (+ 2 (* ii 3)))
    for x0 = (aref verts (+ 0 (* 3 f0)))
    for y0 = (aref verts (+ 1 (* 3 f0)))
    for z0 = (aref verts (+ 2 (* 3 f0)))
    for x1 = (aref verts (+ 0 (* 3 f1)))
    for y1 = (aref verts (+ 1 (* 3 f1)))
    for z1 = (aref verts (+ 2 (* 3 f1)))
    for x2 = (aref verts (+ 0 (* 3 f2)))
    for y2 = (aref verts (+ 1 (* 3 f2)))
    for z2 = (aref verts (+ 2 (* 3 f2)))
    do (tri-bucket-append-tri ret x0 y0 z0 x1 y1 z1 x2 y2 z2)
    finally (return ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BSP Building
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun triangulate-convex-ngon (in-vert in-off in-len out-vert out-off)
  "Triangulate the ngon in IN-VERT into OUT-VERT starting at OUT-OFF.

IN-VERT, OUT-VERT are simple arrays of single-float, one vert is 3
SINGLE-FLOATs.

IN-LEN is the number of floats in IN-VERT, should be a multiple of 3.

Writes (* (* (- (/ IN-LEN 3) 2) 3) 3) floats to OUT-VERT.

Returns (* (* (- (/ IN-LEN 3) 2) 3) 3)
"
  (declare (type (simple-array single-float) in-vert out-vert)
           (type fixnum out-off))
  (loop with out-ix = out-off
        for ii from 1 below (- (/ in-len 3) 1)
        do (setf (aref out-vert (1- (incf out-ix))) (aref in-vert (+ in-off 0)))
           (setf (aref out-vert (1- (incf out-ix))) (aref in-vert (+ in-off 1)))
           (setf (aref out-vert (1- (incf out-ix))) (aref in-vert (+ in-off 2)))
        do (setf (aref out-vert (1- (incf out-ix))) (aref in-vert (+ in-off (* 3 ii) 0)))
           (setf (aref out-vert (1- (incf out-ix))) (aref in-vert (+ in-off (* 3 ii) 1)))
           (setf (aref out-vert (1- (incf out-ix))) (aref in-vert (+ in-off (* 3 ii) 2)))
        do (setf (aref out-vert (1- (incf out-ix))) (aref in-vert (+ in-off (* 3 ii) 3)))
           (setf (aref out-vert (1- (incf out-ix))) (aref in-vert (+ in-off (* 3 ii) 4)))
           (setf (aref out-vert (1- (incf out-ix))) (aref in-vert (+ in-off (* 3 ii) 5))))
  (* (* (- (/ in-len 3) 2) 3) 3))

(declaim (ftype (function (single-float single-float single-float
                           single-float single-float single-float
                           single-float single-float single-float
                           single-float single-float single-float
                           single-float single-float
                           (simple-array single-float)
                           (simple-array single-float))
                          (values fixnum fixnum))
                split-tri-with-plane))
(defun split-tri-with-plane (x0 y0 z0 x1 y1 z1 x2 y2 z2 nx ny nz d e out-poly-front out-poly-behind)
  "Split a triangle, where one of the points is assumed to be on the 'other
side' of the plane. If the plane is defined a point P with normal N,
then D is (dot P N)

E is epsilon (thickness of plane)

Outputs 2 split polys into OUT-POLY-FRONT and OUT-POLY-BEHIND. These are both
(SIMPLE-ARRAY SINGLE-FLOAT) that must be at least 15 elements
long (5 vec3s). This function writes up to the first 15 elements in either
array.

Generally this function writes 3 verts into 1, and 5 verts into the
other - producing a tri and a pentagon from the split.

RETURNS (VALUES N M) where N and M are the number of *floats* written to
OUT-POLY-FRONT and OUT-POLY-BEHIND respectively.

In the case that a line or point is returned (e.g. N or M is < 9),
treat this as no polygon.
"
  (declare (type single-float x0 y0 z0 x1 y1 z1 x2 y2 z2 nx ny nz d e)
           (type (simple-array single-float) out-poly-front out-poly-behind))
  (let ((out-poly-front-ix 0)
        (out-poly-behind-ix 0))
    (declare (type fixnum out-poly-front-ix out-poly-behind-ix))
    (labels ((output-behind-poly (x y z)
               (setf (aref out-poly-behind (1- (incf out-poly-behind-ix))) x)
               (setf (aref out-poly-behind (1- (incf out-poly-behind-ix))) y)
               (setf (aref out-poly-behind (1- (incf out-poly-behind-ix))) z))
             (output-front-poly (x y z)
               (setf (aref out-poly-front (1- (incf out-poly-front-ix))) x)
               (setf (aref out-poly-front (1- (incf out-poly-front-ix))) y)
               (setf (aref out-poly-front (1- (incf out-poly-front-ix))) z))
             (do-edge (x0 y0 z0 x1 y1 z1 p0-class p1-class)
               (cond
                 ((and (eq p0-class :front) (eq p1-class :front))
                  (output-front-poly x1 y1 z1))
                 ((and (eq p0-class :on) (eq p1-class :front))
                  (output-front-poly x1 y1 z1))
                 ((and (eq p0-class :behind) (eq p1-class :front))
                  (multiple-value-bind (ix iy iz) (intersect-line-plane x0 y0 z0 x1 y1 z1 nx ny nz d)
                    (output-front-poly ix iy iz)
                    (output-front-poly x1 y1 z1)
                    (output-behind-poly ix iy iz)))
                 ((and (eq p0-class :front) (eq p1-class :on))
                  (output-front-poly x1 y1 z1))
                 ((and (eq p0-class :on) (eq p1-class :on))
                  (output-front-poly x1 y1 z1))
                 ((and (eq p0-class :behind) (eq p1-class :on))
                  (output-front-poly x1 y1 z1)
                  (output-behind-poly x1 y1 z1))
                 ((and (eq p0-class :front) (eq p1-class :behind))
                  (multiple-value-bind (ix iy iz) (intersect-line-plane x0 y0 z0 x1 y1 z1 nx ny nz d)
                    (output-front-poly ix iy iz)
                    (output-behind-poly ix iy iz)
                    (output-behind-poly x1 y1 z1)))
                 ((and (eq p0-class :on) (eq p1-class :behind))
                  (output-behind-poly x0 y0 z0)
                  (output-behind-poly x1 y1 z1))
                 ((and (eq p0-class :behind) (eq p1-class :behind))
                  (output-behind-poly x1 y1 z1)))))
      (let ((p0-class (classify-point-to-plane x0 y0 z0 nx ny nz d e))
            (p1-class (classify-point-to-plane x1 y1 z1 nx ny nz d e))
            (p2-class (classify-point-to-plane x2 y2 z2 nx ny nz d e)))
        (do-edge x0 y0 z0 x1 y1 z1 p0-class p1-class)
        (do-edge x1 y1 z1 x2 y2 z2 p1-class p2-class)
        (do-edge x2 y2 z2 x0 y0 z0 p2-class p0-class)))
    (values out-poly-front-ix out-poly-behind-ix)))


(deftype point-plane-class () '(member :front :behind :on))
(deftype tri-plane-class () '(member :front :behind :split :coplanar))

(declaim (ftype (function (single-float single-float single-float
                           single-float single-float single-float single-float
                           single-float)
                          point-plane-class)
                classify-point-to-plane)
         (inline classify-point-to-plane))
(defun classify-point-to-plane (x y z nx ny nz d e)
  "Classify a point (X Y Z) with respect to the plane (NX NY NZ D)
with epsilon E. If the plane is defined a point P with normal N,
then D is (dot P N)

Returns :FRONT, :BEHIND, or :ON depending on whether the point is in
front, behind, or on the plane (!!)"
  (declare (type single-float x y z nx ny nz d e)
           (optimize speed))
  (let ((dist (- (dot nx ny nz x y z) d)))
    (cond
      ((< e dist) :front)
      ((< dist (- e)) :behind)
      (t :on))))

(declaim (ftype (function (single-float
                           single-float
                           single-float
                           single-float
                           single-float
                           single-float
                           single-float
                           single-float
                           single-float
                           single-float
                           single-float
                           single-float
                           single-float
                           single-float)
                          tri-plane-class)
                classify-tri-to-plane))
(defun classify-tri-to-plane (x0 y0 z0 x1 y1 z1 x2 y2 z2 nx ny nz d e)
  "Classify a tri with respect to the plane (NX NY NZ D)
  with epsilon E. If the plane is defined a point P with normal N,
  then D is (dot P N)

  Returns :FRONT, :BEHIND, :COPLANAR, or :SPLIT depending on whether the tri is entirely
  in front, behind, coplanar, or split by the plane (!!)"
  (let ((p0 (classify-point-to-plane x0 y0 z0 nx ny nz d e))
        (p1 (classify-point-to-plane x1 y1 z1 nx ny nz d e))
        (p2 (classify-point-to-plane x2 y2 z2 nx ny nz d e)))
    (cond
      ((and (eq p0 :front) (eq p1 :front) (eq p2 :front)) :front)
      ((and (eq p0 :behind) (eq p1 :behind) (eq p2 :behind)) :behind)
      ((and (eq p0 :on) (eq p1 :on) (eq p2 :on)) :coplanar)
      (t :split))))


(defun tri-bucket-split-on-plane (bucket pnx pny pnz pd out-front out-behind tmp-bucket-0 tmp-bucket-1 tmp-bucket-2 ignore-tri-ix eps)
  "Split BUCKET into 2 other buckets, OUT-FRONT and OUT-BEHIND. Splits all tris
in BUCKET based on their position relative to the plane given by PNX
PNY PNZ PD. Tris which are straddling PLANE will be split into 2
polys, those 2 polys will be decomposed into tris, and those tris
added to OUT-FRONT / OUT-BEHIND.

Coplanar triangles are removed.

IGNORE-TRI-IX can be used to ignore a tri - e.g. when the dividing
plane is produced from a tri, you don't want to include that tri in
the nested buckets.

TMP-BUCKET-0, TMP-BUCKET-1,and TMP-BUCKET-2 are used by this function
to split the triangles. They should be empty TRI-BUCKETs.

Does not modify BUCKET."
  (declare (type tri-bucket bucket out-front out-behind tmp-bucket-0 tmp-bucket-1 tmp-bucket-2)
           (type single-float pnx pny pnz pd)
           (type (or null fixnum) ignore-tri-ix))
  (loop with data = (tri-bucket-data bucket)
        for ii of-type fixnum below (tri-bucket-len-tris bucket)
        for x0 of-type single-float = (aref data (+ 0 (the fixnum (* ii 9))))
        for y0 of-type single-float = (aref data (+ 1 (the fixnum (* ii 9))))
        for z0 of-type single-float = (aref data (+ 2 (the fixnum (* ii 9))))
        for x1 of-type single-float = (aref data (+ 3 (the fixnum (* ii 9))))
        for y1 of-type single-float = (aref data (+ 4 (the fixnum (* ii 9))))
        for z1 of-type single-float = (aref data (+ 5 (the fixnum (* ii 9))))
        for x2 of-type single-float = (aref data (+ 6 (the fixnum (* ii 9))))
        for y2 of-type single-float = (aref data (+ 7 (the fixnum (* ii 9))))
        for z2 of-type single-float = (aref data (+ 8 (the fixnum (* ii 9))))
        for class of-type tri-plane-class = (classify-tri-to-plane x0 y0 z0 x1 y1 z1 x2 y2 z2 pnx pny pnz pd eps)
        unless (= ii ignore-tri-ix)
        do (case class
             (:front (tri-bucket-append-tri out-front x0 y0 z0 x1 y1 z1 x2 y2 z2))
             (:behind (tri-bucket-append-tri out-behind x0 y0 z0 x1 y1 z1 x2 y2 z2))
             (:split
              (tri-bucket-clear tmp-bucket-0)
              (tri-bucket-clear tmp-bucket-1)
              (tri-bucket-ensure-n-floats tmp-bucket-0 15)
              (tri-bucket-ensure-n-floats tmp-bucket-1 15)
              (multiple-value-bind (front-len behind-len)
                  (split-tri-with-plane x0 y0 z0 x1 y1 z1 x2 y2 z2 pnx pny pnz pd eps
                                        (tri-bucket-data tmp-bucket-0) (tri-bucket-data tmp-bucket-1))
                (setf (tri-bucket-len tmp-bucket-0) front-len)
                (setf (tri-bucket-len tmp-bucket-1) behind-len)
                (cond ((< 9 front-len)
                       (tri-bucket-clear tmp-bucket-2)
                       (tri-bucket-ensure-n-floats tmp-bucket-2 (the fixnum (* (* (- (truncate front-len 3) 2) 3) 3)))
                       (triangulate-convex-ngon (tri-bucket-data tmp-bucket-0) 0 front-len (tri-bucket-data tmp-bucket-2) 0)
                       (setf (tri-bucket-len tmp-bucket-2) (the fixnum (* (* (- (truncate front-len 3) 2) 3) 3)))
                       (tri-bucket-append-bucket out-front tmp-bucket-2))
                      ((= front-len 9)
                       (tri-bucket-append-bucket out-front tmp-bucket-0)))
                (cond ((< 9 behind-len)
                       (tri-bucket-clear tmp-bucket-2)
                       (tri-bucket-ensure-n-floats tmp-bucket-2 (the fixnum (* (* (- (truncate behind-len 3) 2) 3) 3)))
                       (triangulate-convex-ngon (tri-bucket-data tmp-bucket-1) 0 behind-len (tri-bucket-data tmp-bucket-2) 0)
                       (setf (tri-bucket-len tmp-bucket-2) (the fixnum (* (* (- (truncate behind-len 3) 2) 3) 3)))
                       (tri-bucket-append-bucket out-behind tmp-bucket-2))
                      ((= behind-len 9)
                       (tri-bucket-append-bucket out-behind tmp-bucket-1))))))))

(defun pick-splitting-plane (tri-buckets eps)
  "Given a list of triangle buckets, pick a splitting plane to split them by.

Returns (VALUES PX PY PZ PD BUCKET-IX TRI-IX EXTRA-DATA-IX)

    PX PY PZ PD define the splitting plane. BUCKET-IX and TRI-IX point
    to the tri that was selected to produce the splitting plane from -
    this is useful for callers to explicitly exclude that tri from
    child nodes when splitting on this plane. EXTRA-DATA-IX is the
    index of the extra data associated with that tri.


Returns NIL if all tris are degenerate"
  ;; Pick first tri (TODO we can do better here)
  (assert tri-buckets)
  (loop for bucket in tri-buckets
        for bucket-ix from 0
        do (loop for ii below (tri-bucket-len-tris bucket) do
          (let* ((x0 (aref (tri-bucket-data bucket) (+ 0 (* 9 ii))))
                 (y0 (aref (tri-bucket-data bucket) (+ 1 (* 9 ii))))
                 (z0 (aref (tri-bucket-data bucket) (+ 2 (* 9 ii))))
                 (x1 (aref (tri-bucket-data bucket) (+ 3 (* 9 ii))))
                 (y1 (aref (tri-bucket-data bucket) (+ 4 (* 9 ii))))
                 (z1 (aref (tri-bucket-data bucket) (+ 5 (* 9 ii))))
                 (x2 (aref (tri-bucket-data bucket) (+ 6 (* 9 ii))))
                 (y2 (aref (tri-bucket-data bucket) (+ 7 (* 9 ii))))
                 (z2 (aref (tri-bucket-data bucket) (+ 8 (* 9 ii)))))
            ;; Check if degen, if one of the verts matches another don't use this normal
            (unless (or
                     (and (< (abs (- x0 x1)) eps)
                          (< (abs (- y0 y1)) eps)
                          (< (abs (- z0 z1)) eps))
                     (and (< (abs (- x0 x2)) eps)
                          (< (abs (- y0 y2)) eps)
                          (< (abs (- z0 z2)) eps))
                     (and (< (abs (- x1 x2)) eps)
                          (< (abs (- y1 y2)) eps)
                          (< (abs (- z1 z2)) eps)))
              (multiple-value-bind (nx ny nz)
                  (tri-normal x0 y0 z0 x1 y1 z1 x2 y2 z2 1e-8)
                (unless (and (= nx 0.0) (= ny 0.0) (= nz 0.0))
                  (let ((d (dot x0 y0 z0 nx ny nz)))
                    (return-from pick-splitting-plane (values nx ny nz d bucket-ix ii (tri-bucket-user-data bucket))))))))))
  nil)

(declaim (ftype (function (boolean) bsp-node) make-bsp-node-leaf))
(defun make-bsp-node-leaf (solid-p)
  (make-bsp-node :front nil :behind nil :solid-p solid-p))

(declaim (ftype (function (list boolean single-float) (or null bsp-node)) bsp-node-from-tri-buckets))
(defun bsp-node-from-tri-buckets (tri-buckets solid-p eps)
  "Partition triangles into a tree, and return the root BSP-NODE.

If TRI-BUCKETS is empty or there are only degenerate tris, then this
returns a leaf node with SOLID-P."
  (if tri-buckets
      (let ((out-front-buckets (loop for bucket in tri-buckets collect (make-tri-bucket :user-data (tri-bucket-user-data bucket))))
            (out-behind-buckets (loop for bucket in tri-buckets collect (make-tri-bucket :user-data (tri-bucket-user-data bucket))))
            (tmp-bucket-0 (make-tri-bucket))
            (tmp-bucket-1 (make-tri-bucket))
            (tmp-bucket-2 (make-tri-bucket)))
        (multiple-value-bind (x y z d bucket-ix-to-ignore tri-ix-to-ignore extra-data-ix)
            (pick-splitting-plane tri-buckets eps)
          (when (null x)
            ;; Leaf node
            (return-from bsp-node-from-tri-buckets
              (make-bsp-node-leaf solid-p)))
          (loop for bucket in tri-buckets
                for bucket-ix from 0
                for front-bucket in out-front-buckets
                for behind-bucket in out-behind-buckets
                do (tri-bucket-split-on-plane bucket x y z d front-bucket behind-bucket
                                              tmp-bucket-0 tmp-bucket-1 tmp-bucket-2
                                              (when (= bucket-ix bucket-ix-to-ignore) tri-ix-to-ignore)
                                              eps))
          (let ((front (bsp-node-from-tri-buckets (loop for b in out-front-buckets when (< 0 (tri-bucket-len-tris b)) collect b) nil eps))
                (behind (bsp-node-from-tri-buckets (loop for b in out-behind-buckets when (< 0 (tri-bucket-len-tris b)) collect b) t eps)))
            (let ((ret (make-bsp-node :px x :py y :pz z :pd d :front front :behind behind :solid-p solid-p :user-data extra-data-ix)))
              (setf (bsp-node-parent front) ret)
              (setf (bsp-node-parent behind) ret)
              ret))))
      (make-bsp-node-leaf solid-p)))

(defun add-bevelling-plane (leaf x y z d)
  "Insert the plane X Y Z D above LEAF in the bsp tree. Create a new
empty node for the other side. Assumes the leaf is solid."
  (declare (type single-float x y z d)
           (type bsp-node leaf))
  (assert (bsp-node-solid-p leaf))
  (let* ((parent (bsp-node-parent leaf))
         (new-empty (make-bsp-node-leaf nil))
         (new-bevel (make-bsp-node :px x :py y :pz z :pd d :front new-empty :behind leaf :parent parent)))
    (setf (bsp-node-parent leaf) new-bevel)
    (if (eq leaf (bsp-node-front parent))
        (setf (bsp-node-front parent) new-bevel)
        (setf (bsp-node-behind parent) new-bevel))))

(defun call-with-solid-leaves (bsp-node callback)
  (cond
    ((and bsp-node (bsp-node-leaf-p bsp-node) (bsp-node-solid-p bsp-node))
     (funcall callback bsp-node))
    (bsp-node
     (call-with-solid-leaves (bsp-node-front bsp-node) callback)
     (call-with-solid-leaves (bsp-node-behind bsp-node) callback))
    (t nil)))

(defmacro do-solid-leaves ((bsp-node leaf) &body body)
  `(call-with-solid-leaves ,bsp-node (lambda (,leaf) ,@body)))

(defun find-plane-through-point (nx ny nz x y z)
  "Given a plane normal NX NY NZ and a point X Y Z, return the value
of D for the plane such that the plane rests on the point.

NX NY NZ must be normalized. "
  (+ (* nx x) (* ny y) (* nz z)))

(defun plane-faces-into-mesh-p (x y z d cc-mesh eps)
  "Helper for MAKE-BSP, checks if an edge-bevelling plane faces into
or out of a mesh."
  (loop for vert-ix below (/ (length (cc-mesh-verts cc-mesh)) 3)
        for vx = (aref (cc-mesh-verts cc-mesh) (+ 0 (* 3 vert-ix)))
        for vy = (aref (cc-mesh-verts cc-mesh) (+ 1 (* 3 vert-ix)))
        for vz = (aref (cc-mesh-verts cc-mesh) (+ 2 (* 3 vert-ix)))
        for class = (classify-point-to-plane vx vy vz x y z d eps)
        if (eq class :front) do (return t)))

(defun make-bsp (&key (eps 1e-5))
  "EPS - the epsilon to use when building the BSP tree, this is
      effectively the 'thickness' of the planes"
  (%make-bsp :eps eps))

(defun bsp-build (bsp)
  "Construct a BSP from the list of MESH objects in the BSP-BUILD-STATE."
  (let* ((mesh-input-datas (bsp-build-state-meshes (bsp-build-state bsp)))
         (tri-buckets (loop for mesh-input-data in mesh-input-datas
                            for ii from 0
                            for bucket = (mesh-to-tri-bucket (mesh-input-data-mesh mesh-input-data))
                            do (setf (tri-bucket-user-data bucket) ii)
                            collect bucket))
         (eps (bsp-eps bsp)))
    ;; Store all the user data onto the BSP
    (setf (bsp-user-data-array bsp) (make-array (length mesh-input-datas)))
    (loop for mesh-input-data in mesh-input-datas
          for ii from 0
          do (setf (aref (bsp-user-data-array bsp) ii) (mesh-input-data-user-data mesh-input-data)))
    ;; Partition all the tris into a BSP node
    (setf (bsp-root bsp) (bsp-node-from-tri-buckets tri-buckets nil eps))
    ;; We want to go through all the leaves and insert bevelling
    ;; planes for AABB queries, and for that we need to build a
    ;; CC-MESH for each leaf
    (do-solid-leaves ((bsp-root bsp) leaf)
      ;; Build the CC mesh
      (setf (bsp-node-cc-mesh leaf) (make-cc-mesh-from-planes (planes-for-leaf-node leaf) eps))
      ;; Also convert to tri-mesh and store on the leaf for queries later
      (multiple-value-bind (mesh user-data-array) (cc-mesh-to-tri-mesh (bsp-node-cc-mesh leaf))
        (setf (bsp-node-tri-mesh leaf) mesh)
        (setf (bsp-node-user-data leaf) user-data-array))
      ;; Then use the CC-MESH to insert bevelling planes for AABB testing
      ;; We need to insert 6 bevelling planes for verts, the most
      ;; extreme along the axis, then another for each edge x edge
      ;; cross product separating axis.
      ;; First, verts:
      (loop for (x-axis y-axis z-axis) in '((-1.0 0.0 0.0) (1.0 0.0 0.0)
                                            (0.0 -1.0 0.0) (0.0 1.0 0.0)
                                            (0.0 0.0 -1.0) (0.0 0.0 1.0))
            do (let ((d (coerce
                         (loop for ii below (/ (length (cc-mesh-verts (bsp-node-cc-mesh leaf))) 3)
                               for x = (aref (cc-mesh-verts (bsp-node-cc-mesh leaf)) (+ 0 (* ii 3)))
                               for y = (aref (cc-mesh-verts (bsp-node-cc-mesh leaf)) (+ 1 (* ii 3)))
                               for z = (aref (cc-mesh-verts (bsp-node-cc-mesh leaf)) (+ 2 (* ii 3)))
                               for dot of-type single-float = (dot x y z x-axis y-axis z-axis)
                               maximizing dot)
                         'single-float)))
                 (add-bevelling-plane leaf x-axis y-axis z-axis d)))
      ;; Then edges
      (loop for edge-ix below (/ (length (cc-mesh-edges (bsp-node-cc-mesh leaf))) 2)
            for v0-ix = (aref (cc-mesh-edges (bsp-node-cc-mesh leaf)) (+ 0 (* edge-ix 2)))
            for v1-ix = (aref (cc-mesh-edges (bsp-node-cc-mesh leaf)) (+ 1 (* edge-ix 2)))
            for x0 = (aref (cc-mesh-verts (bsp-node-cc-mesh leaf)) (+ 0 (* v0-ix 3)))
            for y0 = (aref (cc-mesh-verts (bsp-node-cc-mesh leaf)) (+ 1 (* v0-ix 3)))
            for z0 = (aref (cc-mesh-verts (bsp-node-cc-mesh leaf)) (+ 2 (* v0-ix 3)))
            for x1 = (aref (cc-mesh-verts (bsp-node-cc-mesh leaf)) (+ 0 (* v1-ix 3)))
            for y1 = (aref (cc-mesh-verts (bsp-node-cc-mesh leaf)) (+ 1 (* v1-ix 3)))
            for z1 = (aref (cc-mesh-verts (bsp-node-cc-mesh leaf)) (+ 2 (* v1-ix 3)))
            for e-len = (sqrt (+ (* (- x1 x0) (- x1 x0)) (* (- y1 y0) (- y1 y0)) (* (- z1 z0) (- z1 z0))))
            for ex = (/ (- x1 x0) e-len)
            for ey = (/ (- y1 y0) e-len)
            for ez = (/ (- z1 z0) e-len) do
              ;; For this edge, find the cross products with aabb
              ;; edges (the unit axes) which are the bevelling plane
              ;; normals, then compute the plane offset before adding
              ;; the bevelling plane
              (loop for (x-axis y-axis z-axis) in '((-1.0 0.0 0.0) (1.0 0.0 0.0)
                                                    (0.0 -1.0 0.0) (0.0 1.0 0.0)
                                                    (0.0 0.0 -1.0) (0.0 0.0 1.0))
                    do (multiple-value-bind (cx cy cz) (cross ex ey ez x-axis y-axis z-axis)
                         ;; Find the point at which the cross product intersects this edge - this is d
                         (unless (and (zerop cx) (zerop cy) (zerop cz))
                           (let ((clen (sqrt (+ (* cx cx) (* cy cy) (* cz cz)))))
                             (setf cx (/ cx clen) cy (/ cy clen) cz (/ cz clen)))
                           (let ((d (find-plane-through-point cx cy cz x0 y0 z0)))
                             ;; CX CY CZ is the separating axis for this edge and the AABB edge, aka the
                             ;; bevelling plane normal. We need to discard this if it points 'into' the
                             ;; mesh rather than out of it
                             (unless (plane-faces-into-mesh-p cx cy cz d (bsp-node-cc-mesh leaf) eps)
                               (add-bevelling-plane leaf cx cy cz d))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BSP Queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bsp-query-point (bsp x y z)
  "Query a point (X Y Z) in the bsp, returning T if the point is
inside a solid node, and NIL otherwise.

This isn't required by the 3D-SPACES protocol, but is a useful function
for testing."
  (let ((node (bsp-root bsp))
        (eps (bsp-eps bsp)))
    (loop while (and node (not (bsp-node-leaf-p node)))
          for px = (bsp-node-px node)
          for py = (bsp-node-py node)
          for pz = (bsp-node-pz node)
          for pd = (bsp-node-pd node)
          for class = (classify-point-to-plane x y z px py pz pd eps)
          if (eq class :front)
            do (setf node (bsp-node-front node))
          else
            do (setf node (bsp-node-behind node)))
    (when (and node (bsp-node-solid-p node)) node)))

(defun bsp-query-aabb-node (node x y z hx hy hz eps callback)
  "Recursive helper for BSP-QUERY-AABB."
  (cond
    ((and (bsp-node-leaf-p node) (bsp-node-solid-p node)) (funcall callback node))
    ((not (bsp-node-leaf-p node))
     (let* ((px (bsp-node-px node))
            (py (bsp-node-py node))
            (pz (bsp-node-pz node))
            (pd (bsp-node-pd node))
            (r (+ (* (abs px) hx) (* (abs py) hy) (* (abs pz) hz)))
            (class-expanded (classify-point-to-plane x y z px py pz (+ pd r) eps))
            (class-contracted (classify-point-to-plane x y z px py pz (- pd r) eps)))
       ;; Traverse both if the expanded query changed the result, so that we can find *all* leaves
       (cond
         ((and (eq class-expanded :behind) (eq class-contracted :front))
          (bsp-query-aabb-node (bsp-node-front node) x y z hx hy hz eps callback)
          (bsp-query-aabb-node (bsp-node-behind node) x y z hx hy hz eps callback))
         ((eq class-expanded :front)
          (bsp-query-aabb-node (bsp-node-front node) x y z hx hy hz eps callback))
         (t (bsp-query-aabb-node (bsp-node-behind node) x y z hx hy hz eps callback)))))))

(defun bsp-query-aabb (bsp x y z hx hy hz callback)
  "Query the BSP with the aabb at center X, Y, Z with extents HX, HY, HZ.

Calls CALLBACK with overlapping leaf nodes."
  (bsp-query-aabb-node (bsp-root bsp) x y z hx hy hz (bsp-eps bsp) callback))

(defun bsp-node-query-ray (node x y z dx dy dz eps &optional (tmin 0.0) (tmax 1000000.0))
  (cond
    ((null node) nil)
    ((and (bsp-node-leaf-p node) (bsp-node-solid-p node)) (values tmin node))
    ((bsp-node-leaf-p node) nil)
    (t
     (let* ((px (bsp-node-px node))
            (py (bsp-node-py node))
            (pz (bsp-node-pz node))
            (pd (bsp-node-pd node)))
       ;; Split the ray on the BSP node plane, and recursively query those two halves
       ;; Don't actually split the ray, just keep track of tmin/tmax for
       ;; the split rays, to avoid accumulating FP error
       (multiple-value-bind (intersect-t intersect-dist)
           (intersect-ray-plane-t x y z dx dy dz px py pz pd)
         (let* ((near-node (if (< eps intersect-dist) (bsp-node-behind node) (bsp-node-front node)))
                (far-node (if (< eps intersect-dist) (bsp-node-front node) (bsp-node-behind node))))
           (cond
             ;; Ray doesn't intersect plane, visit near side
             ((or (< intersect-t 0.0) (< (- tmax eps) intersect-t))
              (bsp-node-query-ray near-node x y z dx dy dz eps tmin tmax))
             ((<= (+ tmin eps) intersect-t)
              ;; Ray intersects, visit the near side first and far side after
              ;; (If near intersects, use that - otherwise, use far)
              (multiple-value-bind (final-t final-node) (bsp-node-query-ray near-node x y z dx dy dz eps tmin intersect-t)
                (if final-t
                    (values final-t final-node)
                    (bsp-node-query-ray far-node x y z dx dy dz eps intersect-t tmax))))
             ;; Start of ray intersects, but it was already clipped
             ;; by a previous plane - check the far plane instead
             (t (bsp-node-query-ray far-node x y z dx dy dz eps tmin tmax)))))))))

(defun bsp-query-ray (bsp x y z dx dy dz &optional (tmin 0.0) (tmax 1000000.0))
  (bsp-node-query-ray (bsp-root bsp) x y z dx dy dz (bsp-eps bsp) tmin tmax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CC-MESH
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline find-3-plane-intersection))
(defun find-3-plane-intersection (px0 py0 pz0 pd0 px1 py1 pz1 pd1 px2 py2 pz2 pd2)
  "Find the point which is the intersection of the planes P0, P1, P2.

Returns (VALUES X Y Z), or NIL if one of the planes is parallel to another."
  ;; p, p1, p2 all form a vertex - figure out where that is:
  ;; Plane equations:
  ;; px0 x + py0 y + pz0 z = pd0
  ;; px1 x + py1 y + pz1 z = pd1
  ;; px2 x + py2 y + pz2 z = pd2
  ;; When solved (via wolfram alpha):
  ;; x = -( pd0 py1 pz2 - pd0 py2 pz1 - pd1 py0 pz2 + pd1 py2 pz0 + pd2 py0 pz1 - pd2 py1 pz0) / (-px0 py1 pz2 + px0 py2 pz1 + px1 py0 pz2 - px1 py2 pz0 - px2 py0 pz1 + px2 py1 pz0)
  ;; y = -(-pd0 px1 pz2 + pd0 px2 pz1 + pd1 px0 pz2 - pd1 px2 pz0 - pd2 px0 pz1 + pd2 px1 pz0) / (-px0 py1 pz2 + px0 py2 pz1 + px1 py0 pz2 - px1 py2 pz0 - px2 py0 pz1 + px2 py1 pz0)
  ;; z = -(-pd0 px1 py2 + pd0 px2 py1 + pd1 px0 py2 - pd1 px2 py0 - pd2 px0 py1 + pd2 px1 py0) / ( px0 py1 pz2 - px0 py2 pz1 - px1 py0 pz2 + px1 py2 pz0 + px2 py0 pz1 - px2 py1 pz0)
  (declare (type single-float px0 py0 pz0 pd0 px1 py1 pz1 pd1 px2 py2 pz2 pd2)
           (optimize speed))
  (let ((x-denom (+ (- (* px0 py1 pz2)) (* px0 py2 pz1) (* px1 py0 pz2) (- (* px1 py2 pz0)) (- (* px2 py0 pz1)) (* px2 py1 pz0)))
        (y-denom (+ (- (* px0 py1 pz2)) (* px0 py2 pz1) (* px1 py0 pz2) (- (* px1 py2 pz0)) (- (* px2 py0 pz1)) (* px2 py1 pz0)))
        (z-denom (+ (* px0 py1 pz2) (- (* px0 py2 pz1)) (- (* px1 py0 pz2)) (* px1 py2 pz0) (* px2 py0 pz1) (- (* px2 py1 pz0)))))
    (cond
      ((or (zerop x-denom) (zerop y-denom) (zerop z-denom)) nil)
      (t (let ((x (- (/ (+ (* pd0 py1 pz2) (- (* pd0 py2 pz1)) (- (* pd1 py0 pz2)) (* pd1 py2 pz0) (* pd2 py0 pz1) (- (* pd2 py1 pz0)))
                        x-denom)))
               (y (- (/ (+ (- (* pd0 px1 pz2)) (* pd0 px2 pz1) (* pd1 px0 pz2) (- (* pd1 px2 pz0)) (- (* pd2 px0 pz1)) (* pd2 px1 pz0))
                        y-denom)))
               (z (- (/ (+ (- (* pd0 px1 py2)) (* pd0 px2 py1) (* pd1 px0 py2) (- (* pd1 px2 py0)) (- (* pd2 px0 py1)) (* pd2 px1 py0))
                        z-denom))))
           (values x y z))))))

(defun planes-for-leaf-node (leaf-node)
  "Walk back up the tree to return the set of planes which describe
this leaf node. Some planes may not contribute.

Returns a list of conses, where the first item of each cons is the
PLANE, and the second item is the extra user data associated with that
plane"
  (declare (type bsp-node leaf-node))
  (loop with node = leaf-node
        while node
        for parent = (bsp-node-parent node)
        when parent
          collect (let ((sign (if (eq node (bsp-node-front parent)) 1.0 -1.0)))
                    (cons
                     (plane (* sign (bsp-node-px parent))
                            (* sign (bsp-node-py parent))
                            (* sign (bsp-node-pz parent))
                            (* sign (bsp-node-pd parent)))
                     (bsp-node-user-data parent)))
        do (setf node parent)))

(defun %cc-mesh-remove-face (cc-mesh ii)
  "Remove a face. Face must have no verts associated."
  (assert (zerop (length (cc-face-verts (aref (cc-mesh-faces cc-mesh) ii)))))
  ;; Swap + pop
  (if (zerop (length (cc-mesh-faces cc-mesh)))
      (setf (fill-pointer (cc-mesh-faces cc-mesh)) 0)
      (let ((end-face (vector-pop (cc-mesh-faces cc-mesh))))
        (setf (aref (cc-mesh-faces cc-mesh) ii) end-face))))

(defun %cc-mesh-remove-vert (cc-mesh ii)
  "Remove a vert and fixup all face indices

This is called during cc-mesh building, and is called *before*
edges have been inserted."
  (assert (zerop (length (cc-mesh-edges cc-mesh))))
  ;; Swap + pop
  (cond ((< 3 (length (cc-mesh-verts cc-mesh)))
         (setf (aref (cc-mesh-verts cc-mesh) (+ (* ii 3) 2)) (vector-pop (cc-mesh-verts cc-mesh)))
         (setf (aref (cc-mesh-verts cc-mesh) (+ (* ii 3) 1)) (vector-pop (cc-mesh-verts cc-mesh)))
         (setf (aref (cc-mesh-verts cc-mesh) (+ (* ii 3) 0)) (vector-pop (cc-mesh-verts cc-mesh))))
        (t (setf (fill-pointer (cc-mesh-verts cc-mesh)) 0)))
  ;; Fixup other indices
  (loop with faces = (cc-mesh-faces cc-mesh)
        for face across faces do
          (loop with face-verts = (cc-face-verts face)
                for jj from 0
                while (and (< jj (length face-verts)) (< 0 (length face-verts)))
                if (= (aref face-verts jj) ii) do ;; remove indices to the removed vert
                  ;; Swap + pop
                  (setf (aref face-verts jj) (aref face-verts (1- (length face-verts))))
                  (vector-pop face-verts)
                  (decf jj)
                  ;; Fixup indices to the old swap-n-popped vert
                else if (= (aref face-verts jj) (/ (length (cc-mesh-verts cc-mesh)) 3)) do
                  (setf (aref face-verts jj) ii))))

(defun cc-mesh-find-existing-vert (cc-mesh x y z eps)
  "Find the index of the first vert in CC-MESH which is the same as X
Y Z, to some given epsilon EPS."
  (loop for ii below (/ (length (cc-mesh-verts cc-mesh)) 3)
        for vx = (aref (cc-mesh-verts cc-mesh) (+ 0 (* ii 3)))
        for vy = (aref (cc-mesh-verts cc-mesh) (+ 1 (* ii 3)))
        for vz = (aref (cc-mesh-verts cc-mesh) (+ 2 (* ii 3)))
        for len2 = (+ (* (- x vx) (- x vx)) (* (- y vy) (- y vy)) (* (- z vz) (- z vz)))
        when (< len2 eps) do (return ii)))

(defun cc-mesh-find-existing-face (cc-mesh x y z d eps)
  "Find the index of the first vert in CC-MESH which is the same as X
Y Z D, to some given epsilon EPS."
  (loop for face across (cc-mesh-faces cc-mesh)
        for face-ix from 0
        when (and
              (< (abs (- (cc-face-px face) x)) eps)
              (< (abs (- (cc-face-py face) y)) eps)
              (< (abs (- (cc-face-pz face) z)) eps)
              (< (abs (- (cc-face-pd face) d)) eps))
          do (return face-ix)))

(defun %cc-mesh-add-face (cc-mesh px py pz pd eps user-data)
  "Add a face to CC-MESH, possibly pruning other faces and pruning/introducing edges, verts"
  (declare (type cc-mesh cc-mesh)
           (type single-float px py pz pd eps)
           (type t user-data))
  ;; Add new verts
  (cond
    ((cc-mesh-find-existing-face cc-mesh px py pz pd eps) nil)
    (t (let ((new-verts (make-array 0 :element-type '(unsigned-byte 32) :fill-pointer t :adjustable t)))
         (loop with faces = (cc-mesh-faces cc-mesh)
               with verts = (cc-mesh-verts cc-mesh)
               for face-1 across faces
               for face-ix-1 of-type fixnum from 0
               for px1 = (cc-face-px face-1)
               for py1 = (cc-face-py face-1)
               for pz1 = (cc-face-pz face-1)
               for pd1 = (cc-face-pd face-1) do
                 (loop for face-ix-2 of-type fixnum from (+ 1 face-ix-1) below (length faces)
                       for face-2 = (aref faces face-ix-2)
                       for px2 = (cc-face-px face-2)
                       for py2 = (cc-face-py face-2)
                       for pz2 = (cc-face-pz face-2)
                       for pd2 = (cc-face-pd face-2) do
                         (multiple-value-bind (x y z)
                             (find-3-plane-intersection px py pz pd px1 py1 pz1 pd1 px2 py2 pz2 pd2)
                           (when x
                             ;; Check if this vert can be added, or whether it is outside another face
                             (unless (loop for face-3 across faces
                                           for face-ix-3 from 0
                                           for px3 = (cc-face-px face-3)
                                           for py3 = (cc-face-py face-3)
                                           for pz3 = (cc-face-pz face-3)
                                           for pd3 = (cc-face-pd face-3)
                                           when (and
                                                 (/= face-ix-3 face-ix-1) (/= face-ix-3 face-ix-2)
                                                 (eq :behind (classify-point-to-plane x y z px3 py3 pz3 pd3 eps)))
                                             do (return t))
                               (let ((new-vert-ix (or (cc-mesh-find-existing-vert cc-mesh x y z eps)
                                                      (let ((ret (/ (length verts) 3)))
                                                        (vector-push-extend x verts)
                                                        (vector-push-extend y verts)
                                                        (vector-push-extend z verts)
                                                        ret))))
                                 (vector-push-extend new-vert-ix new-verts)
                                 (vector-push-extend new-vert-ix (cc-face-verts face-1))
                                 (vector-push-extend new-vert-ix (cc-face-verts face-2))))))))
         ;; Add the new face
         (vector-push-extend (make-cc-face :verts new-verts :px px :py py :pz pz :pd pd :user-data user-data) (cc-mesh-faces cc-mesh))
         ;; Prune verts outside the shape based on the new face
         (unless (zerop (length (cc-mesh-verts cc-mesh)))
           ;; Prune other verts
           (loop with faces = (cc-mesh-faces cc-mesh)
                 with verts = (cc-mesh-verts cc-mesh)
                 for ii of-type fixnum from 0
                 while (and (< ii (the fixnum (truncate (length verts) 3))) (< 0 (length verts)))
                 for x = (aref verts (+ 0 (* 3 ii)))
                 for y = (aref verts (+ 1 (* 3 ii)))
                 for z = (aref verts (+ 2 (* 3 ii)))
                 when (loop for face across faces
                            for face-ix from 0
                            for px = (cc-face-px face)
                            for py = (cc-face-py face)
                            for pz = (cc-face-pz face)
                            for pd = (cc-face-pd face)
                            when (eq :behind (classify-point-to-plane x y z px py pz pd eps))
                              do (return t))
                   do (%cc-mesh-remove-vert cc-mesh ii)
                      (decf ii)))))))

;; Note: there is a potentially faster algorithm for this, see:
;; A Pivoting Algorithm for Convex Hulls and Vertex Enumeration of Arrangements and Polyhedra
;; Avis and Fukuda
(defun make-cc-mesh-from-planes (planes eps)
  ;; First insert all the faces into the cc-mesh, which will also add verts
  (let ((cc-mesh (loop with ret = (make-cc-mesh)
                       for (plane . user-data) in planes
                       do (%cc-mesh-add-face ret (vx3 plane) (vy3 plane) (vz3 plane) (plane-distance plane) eps user-data)
                       finally (return ret))))
    ;; Remove planes which have no verts, which means they don't
    ;; contribute to the final mesh
    (loop for face-ix from 0
          while (< face-ix (length (cc-mesh-faces cc-mesh)))
          for face = (aref (cc-mesh-faces cc-mesh) face-ix)
          when (zerop (length (cc-face-verts face)))
            do (%cc-mesh-remove-face cc-mesh face-ix)
               (decf face-ix))
    (let ((vert-face-map (make-array (/ (length (cc-mesh-verts cc-mesh)) 3) :element-type 'list :initial-element (list))))
      ;; Produce a reverse map of vert ix -> face ix. This will be used
      ;; to construct edges: if 2 verts share 2 faces, then there must
      ;; be an edge between them.
      (loop for face-ix below (length (cc-mesh-faces cc-mesh))
            for face = (aref (cc-mesh-faces cc-mesh) face-ix) do
              (loop for vert-ix across (cc-face-verts face) do
                (push face-ix (aref vert-face-map vert-ix))))
      ;; Now produce edges from this map
      (loop for vert0-ix below (1- (length vert-face-map))
            for faces0 = (aref vert-face-map vert0-ix) do
              (loop for vert1-ix from (+ 1 vert0-ix) below (length vert-face-map)
                    for faces1 = (aref vert-face-map vert1-ix) do
                      (let* ((shared-faces
                               (loop with count = 0
                                     for face0 in faces0
                                     while (< count 2)
                                     when (find face0 faces1 :test #'=)
                                       collect face0)))
                        (when (<= 2 (length shared-faces))
                          (vector-push-extend vert0-ix (cc-mesh-edges cc-mesh))
                          (vector-push-extend vert1-ix (cc-mesh-edges cc-mesh))
                          (vector-push-extend (first shared-faces) (cc-mesh-edge-faces cc-mesh))
                          (vector-push-extend (second shared-faces) (cc-mesh-edge-faces cc-mesh))
                          )))))
    cc-mesh))

(defun compute-sorted-face-verts (cc-mesh face-ix)
  "Returns a vector of face vert indices, but sorted to match edges"
  (cond
    ((<= (length (cc-face-verts (aref (cc-mesh-faces cc-mesh) face-ix))) 2)
     (cc-face-verts (aref (cc-mesh-faces cc-mesh) face-ix)))
    (t (let* ((face (aref (cc-mesh-faces cc-mesh) face-ix))
              (centroid (loop with sum-x = 0.0
                              with sum-y = 0.0
                              with sum-z = 0.0
                              for vert-ix across (cc-face-verts face)
                              for x = (aref (cc-mesh-verts cc-mesh) (+ 0 (* vert-ix 3)))
                              for y = (aref (cc-mesh-verts cc-mesh) (+ 1 (* vert-ix 3)))
                              for z = (aref (cc-mesh-verts cc-mesh) (+ 2 (* vert-ix 3)))
                              do (incf sum-x x)
                                 (incf sum-y y)
                                 (incf sum-z z)
                              finally (return (let ((len (length (cc-face-verts face))))
                                                (vec3 (/ sum-x len) (/ sum-y len) (/ sum-z len))))))
              ;; project all verts onto the face plane where `centroid` is
              ;; the origin and compute angles for sorting against the origin
              (plane-normal
                (let* ((x0 (aref (cc-mesh-verts cc-mesh) (+ 0 (* 3 (aref (cc-face-verts face) 0)))))
                       (y0 (aref (cc-mesh-verts cc-mesh) (+ 1 (* 3 (aref (cc-face-verts face) 0)))))
                       (z0 (aref (cc-mesh-verts cc-mesh) (+ 2 (* 3 (aref (cc-face-verts face) 0)))))
                       (x1 (aref (cc-mesh-verts cc-mesh) (+ 0 (* 3 (aref (cc-face-verts face) 1)))))
                       (y1 (aref (cc-mesh-verts cc-mesh) (+ 1 (* 3 (aref (cc-face-verts face) 1)))))
                       (z1 (aref (cc-mesh-verts cc-mesh) (+ 2 (* 3 (aref (cc-face-verts face) 1)))))
                       (x2 (aref (cc-mesh-verts cc-mesh) (+ 0 (* 3 (aref (cc-face-verts face) 2)))))
                       (y2 (aref (cc-mesh-verts cc-mesh) (+ 1 (* 3 (aref (cc-face-verts face) 2)))))
                       (z2 (aref (cc-mesh-verts cc-mesh) (+ 2 (* 3 (aref (cc-face-verts face) 2)))))
                       (dx01 (- x1 x0))
                       (dy01 (- y1 y0))
                       (dz01 (- z1 z0))
                       (dx02 (- x2 x0))
                       (dy02 (- y2 y0))
                       (dz02 (- z2 z0)))
                  (multiple-value-bind (nx ny nz) (cross dx01 dy01 dz01 dx02 dy02 dz02)
                    (vec3 nx ny nz))))
              (plane-x-axis (let* ((x0 (aref (cc-mesh-verts cc-mesh) (+ 0 (* 3 (aref (cc-face-verts face) 0)))))
                                   (y0 (aref (cc-mesh-verts cc-mesh) (+ 1 (* 3 (aref (cc-face-verts face) 0)))))
                                   (z0 (aref (cc-mesh-verts cc-mesh) (+ 2 (* 3 (aref (cc-face-verts face) 0)))))
                                   (v (vec3 (- x0 (vx3 centroid)) (- y0 (vy3 centroid)) (- z0 (vz3 centroid)))))
                              (when (and (zerop (vx3 v)) (zerop (vy3 v)) (zerop (vz3 v)))
                                ;; degenerated, centroid cannot be the same as a vert unless face is concave
                                (return-from compute-sorted-face-verts nil))
                              (vunit v)))
              (x-nor-cross (vc plane-x-axis plane-normal))
              ;; Generate if cross is length 0
              (plane-y-axis (if (zerop (vlength x-nor-cross))
                                (return-from compute-sorted-face-verts nil)
                                (vunit x-nor-cross)))
              (angle-indices (make-array (length (cc-face-verts face))))
              (ret (make-array (length (cc-face-verts face)) :element-type '(unsigned-byte 32))))
         (loop for vert-ix across (cc-face-verts face)
               for ii from 0
               for init-x = (- (aref (cc-mesh-verts cc-mesh) (+ 0 (* vert-ix 3))) (vx3 centroid))
               for init-y = (- (aref (cc-mesh-verts cc-mesh) (+ 1 (* vert-ix 3))) (vy3 centroid))
               for init-z = (- (aref (cc-mesh-verts cc-mesh) (+ 2 (* vert-ix 3))) (vz3 centroid))
               for x = (dot init-x init-y init-z (vx3 plane-x-axis) (vy3 plane-x-axis) (vz3 plane-x-axis))
               for y = (dot init-x init-y init-z (vx3 plane-y-axis) (vy3 plane-y-axis) (vz3 plane-y-axis))
               for angle = (atan y x)
               do (setf (aref angle-indices ii) (cons angle vert-ix)))
         (setf angle-indices (sort angle-indices #'< :key #'car))
         (loop for item across angle-indices
               for ii from 0 do
                 (setf (aref ret ii) (cdr item)))
         ret))))

(defun cc-mesh-to-tri-mesh (cc-mesh)
  "Convert a CC-MESH to a ORG.SHIRAKUMO.FRAF.TRIAL.SPACE:MESH, plus a per-tri user data array. Returns (VALUES MESH USER-DATA-ARRAY)"
  ;; Note: this is probably faster in 2 passes; one to compute the size
  ;; of the faces array, and another to fill in a preallocated
  ;; simple-array, rather than adjustable + fill-pointer.
  (let ((faces (make-array 0 :element-type '(unsigned-byte 32) :adjustable t :fill-pointer t))
        (user-data-array (make-array 0 :element-type '(unsigned-byte 32) :adjustable t :fill-pointer t)))
    (loop for face-ix below (length (cc-mesh-faces cc-mesh))
          for sorted-face-verts = (compute-sorted-face-verts cc-mesh face-ix)
          do (loop for ii from 1 below (- (length sorted-face-verts) 1) do
            (vector-push-extend (aref sorted-face-verts 0) faces)
            (vector-push-extend (aref sorted-face-verts ii) faces)
            (vector-push-extend (aref sorted-face-verts (+ 1 ii)) faces)
            (vector-push-extend (cc-face-user-data (aref (cc-mesh-faces cc-mesh) face-ix)) user-data-array)))
    (assert (= 0 (mod (length faces) 3)))
    ;; Convert FACES to a simple array
    (values
     (mesh
      (make-array (length (cc-mesh-verts cc-mesh)) :element-type 'single-float :initial-contents (cc-mesh-verts cc-mesh))
      (make-array (length faces) :element-type '(unsigned-byte 32) :initial-contents faces))
     (make-array (length user-data-array) :element-type '(unsigned-byte 32) :initial-contents user-data-array))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Serialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BSP Format
;;
;; All numbers little endian.
;;
;; Intended to be read/written once into memory, rather than seeked
;; through
;;
;; # Header
;;
;; magic		u8[4]		"BSP3"
;; version		u32		0x00000001
;; node-count		u32		Number of entries in the node table
;; mesh-count		u32		Number of meshes in the mesh table
;; user-data-count	u32		Number of user data entries
;; eps			f32		The epsilon this BSP was built with
;;
;; # Node table
;;
;; Nodes refer to eachother by *index* in this table (not byte
;; offset). Node entries are not all the same size.
;;
;; Leaf and branch entries are both mixed together in the table, and
;; don't come in any particular order.
;;
;; First 2 bytes of each entry is always the bitflags, which can be
;; used to determine the type of node.
;;
;; ## Branch node
;;
;; flags		u16
;; px			f32
;; py			f32
;; pz			f32
;; pd			f32
;; front		u32		Index into the node table
;; behind		u32		Index into the node table

;; ## Leaf node
;;
;; flags		u16
;; mesh-index		u32		Index into the mesh table
;;
;; ## Flags
;;
;; leaf-p		0x0001
;; solid-p		0x0002
;;
;; # Mesh table
;;
;; Solid leaf nodes have a mesh that defines their extents. The mesh
;; is made of only tris. The mesh is always convex.
;;
;; ## Mesh table entry
;;
;; num-verts		u32
;; num-tris		u32
;; verts		f32[num-verts * 3]	3 floats per vert
;; indices		u32[num-tris * 3]	Each references a vert, up to num-verts
;; user-data		u32[num-tris]		Each references an entry in the user data table, one per tri
;;
;; # User data
;;
;; User data is an extra arbitrary string, serialized as UTF-8. During
;; serialization/deserialization, an extra function is provided to
;; serialize/deserialize these strings further into a lisp object
;; stored directly on the BSP.
;;
;; # User data entry
;;
;; size			u32		Size of the UTF-8 string in *bytes*
;; data			u8[size]

(defun bsp-count-nodes (bsp-node)
  (if bsp-node
    (+ 1 (bsp-count-nodes (bsp-node-front bsp-node)) (bsp-count-nodes (bsp-node-behind bsp-node)))
    0))

(defun bsp-count-solid-leaves (bsp-node)
  (cond
    ((and bsp-node (bsp-node-leaf-p bsp-node) (bsp-node-solid-p bsp-node)) 1)
    ((and bsp-node (bsp-node-leaf-p bsp-node)) 0)
    (bsp-node (+ (bsp-count-solid-leaves (bsp-node-front bsp-node)) (bsp-count-solid-leaves (bsp-node-behind bsp-node))))
    (t 0)))

(defun %bsp-visit-nodes (bsp-node callback)
  "Always visits nodes in a consistent order (depth first, front first)"
  (when bsp-node
    (funcall callback bsp-node)
    (%bsp-visit-nodes (bsp-node-front bsp-node) callback)
    (%bsp-visit-nodes (bsp-node-behind bsp-node) callback)))

(defmacro bsp-visit-nodes ((node bsp) &body body)
  "Always visits nodes in a consistent order (depth first, front first)"
  `(%bsp-visit-nodes (bsp-root ,bsp) (lambda (,node) ,@body)))

(defun bsp-serialize (bsp s object->id)
  ;; Magic "BSP3"
  (write-byte (char-code #\B) s)
  (write-byte (char-code #\S) s)
  (write-byte (char-code #\P) s)
  (write-byte (char-code #\3) s)
  (nibbles:write-ub32/le 1 s) ;; version
  (let ((node-count (bsp-count-nodes (bsp-root bsp)))
        (mesh-count (bsp-count-solid-leaves (bsp-root bsp))))
    (nibbles:write-ub32/le node-count s)
    (nibbles:write-ub32/le mesh-count s)
    ;; user-data-count
    (nibbles:write-ub32/le (length (bsp-user-data-array bsp)) s))
  (nibbles:write-ieee-single/le (bsp-eps bsp) s)
  ;; Node table
  ;; First, assign every node an index. BSP-VISIT-NODES always
  ;; visits in the same order.
  (let ((node-index 0))
    (bsp-visit-nodes (node bsp)
      (setf (bsp-node-table-index node) node-index)
      (incf node-index)))
  ;; Then we can write the nodes. We write the mesh index afterwards
  (let ((mesh-index 0))
    (bsp-visit-nodes (node bsp)
      (cond
        ((bsp-node-leaf-p node)
         (nibbles:write-ub16/le (if (bsp-node-solid-p node) #x0003 #x0001) s) ;; flags
         (cond ((bsp-node-solid-p node)
                (nibbles:write-ub32/le mesh-index s)
                (incf mesh-index))
               (t (nibbles:write-ub32/le 0 s))))
        (t
         (nibbles:write-ub16/le #x0000 s) ;; flags
         (nibbles:write-ieee-single/le (bsp-node-px node) s)
         (nibbles:write-ieee-single/le (bsp-node-py node) s)
         (nibbles:write-ieee-single/le (bsp-node-pz node) s)
         (nibbles:write-ieee-single/le (bsp-node-pd node) s)
         (nibbles:write-ub32/le (bsp-node-table-index (bsp-node-front node)) s)
         (nibbles:write-ub32/le (bsp-node-table-index (bsp-node-behind node)) s)))))
  ;; Mesh table
  ;; Again, BSP-VISIT-NODES walks in the same order, so mesh-index will match up
  (bsp-visit-nodes (node bsp)
    (when (and (bsp-node-leaf-p node) (bsp-node-solid-p node))
      (let ((verts (mesh-vertices (bsp-node-tri-mesh node)))
            (indices (mesh-faces (bsp-node-tri-mesh node)))
            (user-data (bsp-node-user-data node))
            )
        (assert (= 0 (mod (length verts) 3)))
        (assert (= 0 (mod (length indices) 3)))
        (assert (= (length user-data) (/ (length indices) 3)))
        (nibbles:write-ub32/le (/ (length verts) 3) s)
        (nibbles:write-ub32/le (/ (length indices) 3) s)
        (nibbles:write-ieee-single/le-sequence verts s)
        (nibbles:write-ub32/le-sequence indices s)
        (nibbles:write-ub32/le-sequence user-data s))))
  ;; User data table
  (loop for user-data across (bsp-user-data-array bsp)
        for serialized-str = (funcall object->id user-data)
        for utf8-str = (babel:string-to-octets serialized-str :encoding :utf-8)
        do (nibbles:write-ub32/le (length utf8-str) s)
           (write-sequence utf8-str s)))

(defun check-byte (expected byte)
  "Helper for BSP-DESERIALIZE, error if byte does not match"
  (when (/= expected byte)
    (error "Expected ~d found ~d." expected byte)))

(defun bsp-deserialize (s id->object)
  ;; Magic "BSP3"
  (check-byte (char-code #\B) (read-byte s))
  (check-byte (char-code #\S) (read-byte s))
  (check-byte (char-code #\P) (read-byte s))
  (check-byte (char-code #\3) (read-byte s))
  (let ((version (nibbles:read-ub32/le s)))
    (when (/= version 1)
      (error "Trying to load BSP version ~d, only version 1 is supported." version)))
  (let* ((node-count (nibbles:read-ub32/le s))
         (mesh-count (nibbles:read-ub32/le s))
         (user-data-count (nibbles:read-ub32/le s))
         (eps (nibbles:read-ieee-single/le s))
         (nodes (make-array node-count :element-type '(or null bsp-node) :initial-element nil))
         (mesh-indices (make-array node-count :element-type '(unsigned-byte 32) :initial-element #xffffffff))
         (front-children (make-array node-count :element-type '(unsigned-byte 32) :initial-element #xffffffff))
         (behind-children (make-array node-count :element-type '(unsigned-byte 32) :initial-element #xffffffff))
         (meshes (make-array mesh-count :element-type '(or null mesh) :initial-element nil))
         (mesh-user-datas (make-array mesh-count :element-type '(or null (simple-array (unsigned-byte 32))) :initial-element nil))
         (user-data (make-array user-data-count)))
    ;; Node table
    (loop for ii below node-count
          for flags = (nibbles:read-ub16/le s)
          if (not (zerop (logand #x0001 flags))) do
            ;; Leaf
            (let ((solid-p (not (zerop (logand #x0002 flags))))
                  (mesh-index (nibbles:read-ub32/le s)))
              (setf (aref mesh-indices ii) mesh-index)
              (setf (aref nodes ii) (make-bsp-node-leaf solid-p)))
          else do
            ;; Branch
            (let* ((px (nibbles:read-ieee-single/le s))
                   (py (nibbles:read-ieee-single/le s))
                   (pz (nibbles:read-ieee-single/le s))
                   (pd (nibbles:read-ieee-single/le s))
                   (front-ix (nibbles:read-ub32/le s))
                   (behind-ix (nibbles:read-ub32/le s)))
              (setf (aref front-children ii) front-ix)
              (setf (aref behind-children ii) behind-ix)
              (setf (aref nodes ii) (make-bsp-node :px px :py py :pz pz :pd pd))))
    ;; Mesh table
    (loop for ii below mesh-count
          for num-verts = (nibbles:read-ub32/le s)
          for num-tris = (nibbles:read-ub32/le s)
          for verts = (make-array (* num-verts 3) :element-type 'single-float)
          for tris = (make-array (* num-tris 3) :element-type '(unsigned-byte 32))
          for user-datas = (make-array num-tris :element-type '(unsigned-byte 32))
          do (nibbles:read-ieee-single/le-into-sequence verts s)
             (nibbles:read-ub32/le-into-sequence tris s)
             (nibbles:read-ub32/le-into-sequence user-datas s)
             (setf (aref meshes ii) (mesh verts tris))
             (setf (aref mesh-user-datas ii) user-datas))
    ;; User data table
    (loop for ii below user-data-count
          for data-len = (nibbles:read-ub32/le s)
          for utf8-data = (make-array data-len :element-type '(unsigned-byte 8))
          for string = (progn (read-sequence utf8-data s) (babel:octets-to-string utf8-data :encoding :utf-8))
          for object = (funcall id->object string)
          do (setf (aref user-data ii) object))
    ;; Connect everything
    ;; First mesh indices and user datas
    (loop for ii below node-count
          for mesh-index = (aref mesh-indices ii)
          for node = (aref nodes ii)
          when (/= #xffffffff mesh-index)
            do (setf (bsp-node-tri-mesh node) (aref meshes mesh-index))
               (setf (bsp-node-user-data node) (aref mesh-user-datas mesh-index)))
    ;; Then parent/children
    (loop for ii below node-count
          for node = (aref nodes ii)
          for front = (aref front-children ii)
          for behind = (aref behind-children ii)
          when (/= front #xffffffff) do
            (assert (/= behind #xffffffff))
            (setf (bsp-node-front node) (aref nodes front))
            (setf (bsp-node-behind node) (aref nodes behind))
            (setf (bsp-node-parent (aref nodes front)) node)
            (setf (bsp-node-parent (aref nodes behind)) node))
    ;; Check there is only 1 root
    (assert (= 1 (loop for node across nodes
                       unless (bsp-node-parent node)
                         sum 1)))
    ;; Find the root, return the BSP
    (let ((root (find-if-not #'bsp-node-parent nodes)))
      (%make-bsp :eps eps :build-state nil :root root :user-data-array user-data))))

(defun %check-bsp-equal-mesh (mesh0 mesh1)
  "Helper for CHECK-BSP-EQUAL, mesh equality"
  (assert (= (length (mesh-vertices mesh0)) (length (mesh-vertices mesh1))))
  (assert (= (length (mesh-faces mesh0)) (length (mesh-faces mesh1))))
  (loop for ii below (length (mesh-vertices mesh0)) do
    (assert (= (aref (mesh-vertices mesh0) ii) (aref (mesh-vertices mesh1) ii))))
  (loop for ii below (length (mesh-faces mesh0)) do
    (assert (= (aref (mesh-faces mesh0) ii) (aref (mesh-faces mesh1) ii)))))

(defun %check-bsp-equal-node (bsp-node0 bsp-node1)
  "Helper for CHECK-BSP-EQUAL, recursive bsp node comparison"
  (assert (eql (bsp-node-leaf-p bsp-node0) (bsp-node-leaf-p bsp-node1)))
  (cond ((bsp-node-leaf-p bsp-node0)
         (assert (eql (bsp-node-solid-p bsp-node0) (bsp-node-solid-p bsp-node1)))
         (when (bsp-node-solid-p bsp-node0)
           (assert (and (bsp-node-tri-mesh bsp-node0) (bsp-node-tri-mesh bsp-node1)))
           (%check-bsp-equal-mesh (bsp-node-tri-mesh bsp-node0) (bsp-node-tri-mesh bsp-node1))))
        (t
         (assert (= (bsp-node-px bsp-node0) (bsp-node-px bsp-node1)))
         (assert (= (bsp-node-py bsp-node0) (bsp-node-py bsp-node1)))
         (assert (= (bsp-node-pz bsp-node0) (bsp-node-pz bsp-node1)))
         (assert (= (bsp-node-pd bsp-node0) (bsp-node-pd bsp-node1)))
         (%check-bsp-equal-node (bsp-node-front bsp-node0) (bsp-node-front bsp-node1))
         (%check-bsp-equal-node (bsp-node-behind bsp-node0) (bsp-node-behind bsp-node1)))))

(defun check-bsp-equal (bsp0 bsp1)
  "Compare two BSP, asserting if they are not equal. This is for
testing serialization, and nothing else. Because this is for testing
ser, exact FP comparisons are used."
  (assert (= (bsp-eps bsp0) (bsp-eps bsp1)))
  (%check-bsp-equal-node (bsp-root bsp0) (bsp-root bsp1)))

(defun test-serialize (bsp id->object object->id)
  "Ser + deser a bsp, return the new bsp"
  (with-open-file (stream #P"test.bsp" :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (serialize bsp stream object->id))
  (with-open-file (stream #P"test.bsp" :direction :input :element-type '(unsigned-byte 8))
    (let ((new-bsp (%make-bsp :eps 0.0)))
      (deserialize new-bsp stream id->object)
      (check-bsp-equal new-bsp bsp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct ray-result
  (leaf-mesh (error "missing ray-result-leaf-mesh") :type mesh)
  (time (error "missing ray-result-time") :type single-float)
  (x (error "missing ray-result-x") :type single-float)
  (y (error "missing ray-result-y") :type single-float)
  (z (error "missing ray-result-z") :type single-float))
(declaim (inline make-ray-result))

(defmethod enter (mesh-input-data (bsp bsp))
  "MESH-INPUT-DATA is of type MESH-INPUT-DATA - it contains the mesh
*and* some arbitrary user data which is stored on the BSP and can be
fed back during query. The user data must be serializable, see
SERIALIZE's OBJECT->ID function parameter."
  (push mesh-input-data (bsp-build-state-meshes (bsp-build-state bsp))))

(defmethod reoptimize ((bsp bsp) &key)
  "Build the BSP from the meshes added with ENTER. If a BSP is
serialized and deserialized, the deserialized BSP does not store the
old meshes entered into it, so calling REOPTIMIZE on the deserialized
mesh will effectively erase that data."
  (bsp-build bsp))

(defmethod serialize ((bsp bsp) file object->id)
  (bsp-serialize bsp file object->id))

(defmethod deserialize ((bsp bsp) file id->object)
  (let ((new-bsp (bsp-deserialize file id->object)))
    (setf (bsp-root bsp) (bsp-root new-bsp))
    (setf (bsp-eps bsp) (bsp-eps new-bsp))
    (setf (bsp-user-data-array bsp) (bsp-user-data-array new-bsp))))

(defmethod call-with-all (function (bsp bsp))
  (labels ((recurse (node)
             (when node
               (when (and node (bsp-node-leaf-p node) (bsp-node-solid-p node))
                 (funcall function (bsp-node-tri-mesh node)))
               (recurse (bsp-node-front node))
               (recurse (bsp-node-behind node)))))
    (recurse (bsp-root bsp))))

(defmethod call-with-overlapping (function (bsp bsp) (region region))
  (let ((x (+ (/ (vx3 (region-size region)) 2.0) (vx3 region)))
        (y (+ (/ (vy3 (region-size region)) 2.0) (vy3 region)))
        (z (+ (/ (vz3 (region-size region)) 2.0) (vz3 region)))
        (hx (/ (vx3 (region-size region)) 2.0))
        (hy (/ (vy3 (region-size region)) 2.0))
        (hz (/ (vz3 (region-size region)) 2.0))
        (callback (lambda (node) (funcall function (bsp-node-tri-mesh node)))))
    (bsp-query-aabb bsp x y z hx hy hz callback)))

(defmethod call-with-contained (function (bsp bsp) (region region))
  (let ((x0 (vx3 region))
        (y0 (vy3 region))
        (z0 (vz3 region))
        (x1 (+ (vx3 (region-size region)) (vx3 region)))
        (y1 (+ (vy3 (region-size region)) (vy3 region)))
        (z1 (+ (vz3 (region-size region)) (vz3 region))))
    (do-overlapping (mesh bsp region)
      ;; Check that all verts are inside this region
      (when (loop for ii below (/ (length (mesh-vertices mesh)) 3)
                  for x = (aref (mesh-vertices mesh) (+ 0 (* ii 3)))
                  for y = (aref (mesh-vertices mesh) (+ 1 (* ii 3)))
                  for z = (aref (mesh-vertices mesh) (+ 2 (* ii 3)))
                  unless (and (<= x0 x) (<= x x1) (<= y0 y) (<= y y1) (<= z0 z) (<= z z1))
                    do (return nil)
                  finally (return t))
        (funcall function mesh)))))

(defmethod call-with-intersecting (function (bsp bsp) ray-origin ray-direction)
  (multiple-value-bind (time node)
      (bsp-query-ray bsp
                     (vx3 ray-origin) (vy3 ray-origin) (vz3 ray-origin)
                     (vx3 ray-direction) (vy3 ray-direction) (vz3 ray-direction))
    (when node
      (let ((result (make-ray-result :leaf-mesh (bsp-node-tri-mesh node) :time time
                                     :x (+ (vx3 ray-origin) (* time (vx3 ray-direction)))
                                     :y (+ (vy3 ray-origin) (* time (vy3 ray-direction)))
                                     :z (+ (vz3 ray-origin) (* time (vz3 ray-direction))))))
        (declare (dynamic-extent result))
        (funcall function result)))))
