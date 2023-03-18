(defmethod staple:packages ((system (eql (asdf:find-system :3d-spaces))))
  '(#:org.shirakumo.fraf.trial.space
    #:org.shirakumo.fraf.trial.space.bvh2
    #:org.shirakumo.fraf.trial.space.quadtree
    #:org.shirakumo.fraf.trial.space.grid3))
