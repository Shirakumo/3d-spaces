(defpackage #:org.shirakumo.fraf.trial.space
  (:use #:cl #:org.shirakumo.fraf.math)
  (:local-nicknames
   (#:sequences #:org.shirakumo.trivial-extensible-sequences))
  (:intern
   #:ensure-function
   #:clamp
   #:box-intersects-box-p
   #:box-contains-box-p
   #:sphere-intersects-box-p
   #:ray-intersects-box-p)
  (:export
   #:location
   #:bsize
   #:radius
   #:bounding-box
   #:oriented-bounding-box
   #:bounding-sphere
   #:group
   #:ensure-region
   #:check
   #:clear
   #:reoptimize
   #:object-count
   #:enter
   #:leave
   #:update
   #:call-with-all
   #:call-with-candidates
   #:call-with-overlapping
   #:call-with-contained
   #:call-with-intersecting
   #:call-with-pairs
   #:container
   #:container-p
   #:region
   #:region-size
   #:sphere
   #:sphere-radius
   #:plane
   #:plane-distance
   #:mesh
   #:mesh-vertices
   #:mesh-faces
   #:do-all
   #:do-candidates
   #:do-contained
   #:do-overlapping
   #:do-intersecting
   #:do-pairs
   #:find-region
   #:region-overlaps-p
   #:region-contains-p
   #:serialize
   #:deserialize))

(macrolet ((defpackage* (name &rest args)
             `(defpackage ,name
                ,@args
                (:local-nicknames
                 (#:sequences #:org.shirakumo.trivial-extensible-sequences))
                (:export ,@(loop for s being the external-symbols of '#:org.shirakumo.fraf.trial.space
                                 collect s)))))
  (defpackage* #:org.shirakumo.fraf.trial.space.bvh2
      (:use #:cl #:org.shirakumo.fraf.math #:org.shirakumo.fraf.trial.space)
    (:import-from #:org.shirakumo.fraf.trial.space
     #:ensure-function)
    (:export
     #:bvh
     #:make-bvh
     #:bvh-insert
     #:bvh-remove
     #:bvh-update
     #:bvh-lines))

  (defpackage* #:org.shirakumo.fraf.trial.space.grid3
      (:use #:cl #:org.shirakumo.fraf.math #:org.shirakumo.fraf.trial.space)
    (:import-from #:org.shirakumo.fraf.trial.space
     #:clamp
     #:ensure-function)
    (:export
     #:grid
     #:make-grid
     #:grid-resize
     #:grid-move
     #:grid-insert
     #:grid-remove
     #:grid-update))

  (defpackage* #:org.shirakumo.fraf.trial.space.quadtree
      (:use #:cl #:org.shirakumo.fraf.math #:org.shirakumo.fraf.trial.space)
    (:import-from #:org.shirakumo.fraf.trial.space
     #:ensure-function)
    (:export
     #:quadtree
     #:make-quadtree
     #:make-quadtree-at
     #:quadtree-insert
     #:quadtree-remove
     #:quadtree-update
     #:quadtree-find-all
     #:quadtree-find-overlaps
     #:quadtree-find-overlaps-in
     #:quadtree-find-contained
     #:quadtree-find-contained-in
     #:quadtree-find-for
     #:quadtree-lines))

  (defpackage* #:org.shirakumo.fraf.trial.space.kd-tree
      (:use #:cl #:org.shirakumo.fraf.math #:org.shirakumo.fraf.trial.space)
    (:import-from #:org.shirakumo.fraf.trial.space
     #:ensure-function
     #:box-intersects-box-p
     #:box-contains-box-p
     #:sphere-intersects-box-p
     #:ray-intersects-box-p)
    (:export
     #:kd-tree
     #:make-kd-tree
     #:kd-tree-insert
     #:kd-tree-remove
     #:kd-tree-nearest
     #:kd-tree-k-nearest
     #:kd-tree-call-with-nearest)))
