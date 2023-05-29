#|
 This file is a part of 3d-spaces
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.trial.space
  (:use #:cl #:org.shirakumo.flare.vector #:org.shirakumo.flare.matrix)
  (:local-nicknames
   (#:sequences #:org.shirakumo.trivial-extensible-sequences))
  (:intern #:describe-tree)
  (:export
   #:location
   #:bsize
   #:radius
   #:ensure-region
   #:check
   #:clear
   #:reoptimize
   #:object-count
   #:enter
   #:leave
   #:update
   #:call-with-all
   #:call-with-contained
   #:call-with-overlapping
   #:call-with-intersecting
   #:container
   #:container-p
   #:region
   #:region-size
   #:sphere
   #:sphere-radius
   #:do-all
   #:do-contained
   #:do-overlapping
   #:do-intersecting
   #:find-region
   #:region-overlaps-p
   #:region-contains-p))

(macrolet ((defpackage* (name &rest args)
             `(defpackage ,name
                ,@args
                (:local-nicknames
                 (#:sequences #:org.shirakumo.trivial-extensible-sequences))
                (:import-from #:org.shirakumo.fraf.trial.space #:describe-tree)
                (:export ,@(loop for s being the external-symbols of '#:org.shirakumo.fraf.trial.space
                                 collect s)))))
  (defpackage* #:org.shirakumo.fraf.trial.space.bvh2
    (:use #:cl #:org.shirakumo.flare.vector #:org.shirakumo.fraf.trial.space)
    (:export
     #:bvh
     #:make-bvh
     #:bvh-insert
     #:bvh-remove
     #:bvh-update
     #:bvh-lines))

  (defpackage* #:org.shirakumo.fraf.trial.space.grid3
    (:use #:cl #:org.shirakumo.flare.vector #:org.shirakumo.fraf.trial.space)
    (:export
     #:grid
     #:make-grid
     #:grid-resize
     #:grid-move
     #:grid-insert
     #:grid-remove
     #:grid-update))

  (defpackage* #:org.shirakumo.fraf.trial.space.quadtree
    (:use #:cl #:org.shirakumo.flare.vector #:org.shirakumo.fraf.trial.space)
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
      (:use #:cl #:org.shirakumo.flare.vector #:org.shirakumo.fraf.trial.space)
    (:export
     #:kd-tree
     #:make-kd-tree
     #:kd-tree-insert
     #:kd-tree-remove
     #:kd-tree-nearest
     #:kd-tree-k-nearest
     #:kd-tree-call-with-nearest)))
