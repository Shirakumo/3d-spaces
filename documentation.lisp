#|
 This file is a part of 3d-spaces
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.space)

(docs:define-docs
  (function location
    "Returns a vec2 or vec3 representing the center of the object.

Users should extend this function with methods to provide the correct
value for their own object representations.

See BSIZE
See RADIUS")
  
  (function bsize
    "Returns a vec2 or vec3 representing the  half-size of the object's axis-aligned bounding box.

Users should extend this function with methods to provide the correct
value for their own object representations.

See LOCATION
See RADIUS")
  
  (function radius
    "Returns a single-float representing the radius of the object's bounding sphere.

Users should extend this function with methods to provide the correct
value for their own object representations.

If no method is provided it is computed automatically from the BSIZE.

See LOCATION
See BSIZE")
  
  (function ensure-region
    "Coerces the object to a REGION instance.

If the optional region is passed, the data in that region is updated
instead of creating a new region instance.

See REGION (type)")
  
  (function check
    "Checks the container for validity.

Signals an error if there are problems with the container's internal
invariants.

See CONTAINER (type)")
  
  (function clear
    "Clears the container and removes all objects from it.

See CONTAINER (type)")
  
  (function reoptimize
    "Reoptimizes the container to fit its objects better.

Each container type may accept specific arguments to control the
opitimization.

See CONTAINER (type)")
  
  (function enter
    "Adds objects to the container.

You may pass an object or a sequence of objects.
Passing a sequence may be more efficient that passing the objects
individually.

Entering the same object twice is safe.

Every object must implement a method for at least LOCATION and BSIZE.

See CONTAINER (type)")
  
  (function leave
    "Removes an object from the container.

You may pass an object or a sequence of objects.
Passing a sequence may be more efficient that passing the objects
individually.

Removing the same object twice is safe.

See CONTAINER (type)")
  
  (function update
    "Updates the objects in the container.

You may pass an object or a sequence of objects.
Passing a sequence may be more efficient that passing the objects
individually.

You should call this function whenever the position or bounding size
of the object changes. If you do not notify the container of changes
like this, range queries may not give correct results.

Calling UPDATE on an object that is not currently in the container has
undefined consequences.

See CONTAINER (type)")
  
  (function call-with-all
    "Calls FUNCTION with every object contained in the container.

See DO-ALL
See CONTAINER (type)")
  
  (function call-with-contained
    "Calls FUNCTION with every object contained in REGION.

The region is coerced to a region via ENSURE-REGION.
The function *may* be called with objects that overlap the specified
region, but *will not* be called with objects that lie entirely
outside the region.

See DO-CONTAINED
See ENSURE-REGION
See REGION (type)
See CONTAINER (type)")
  
  (function call-with-overlapping
    "Calls FUNCTION with every object contained in REGION.

The region is coerced to a region via ENSURE-REGION.
The function *will* be called with all objects that overlap the
specified region, and *will not* be called with objects that lie
entirely outside the region.

See DO-OVERLAPPING
See ENSURE-REGION
See REGION (type)
See CONTAINER (type)")

  (function serialize
    "Serialise CONTAINER to FILE.

OBJECT->ID is called for every object that was added to the container
during the serialisation process. Value returned from the function
must be a string.

Consequences are undefined if the function returns the same (EQUAL)
ID string for two different (EQ) objects, or returns different (EQUAL)
ID strings for the same (EQ) object.

See DESERIALIZE
See CONTAINER (type)")

  (function deserialize
    "Deserialize CONTAINER from FILE.

ID->OBJECT is called for every object that was added to the container
that was used to serialize to the given file. The function should
accept the strings that were previously returned by OBJECT->ID and
return the corresponding object.

It is expected that the size and location of the returned objects is
the same as it was when SERIALIZE was called. Or in other words, it is
still up to you to ensure object properties are persisted separately
or that the container is updated appropriately after deserialization.

Consequences are undefined if the function returns NIL, or the same
(EQ) object for two different (EQUAL) IDs, or returning different
objects for the same (EQUAL) IDS.

See SERIALIZE
See CONTAINER (type)")
  
  (type container
    "Supertype for all containers.

All container types must implement the following functions in addition
to implementing the iteration protocol of FOR (meaning you can use
FOR:OVER to iterate over a container as well).

See CONTAINER-P
See CHECK
See CLEAR
See REOPTIMIZE
See ENTER
See LEAVE
See UPDATE
See CALL-WITH-ALL
See CALL-WITH-CONTAINED
See CALL-WITH-OVERLAPPING")
  
  (function container-p
    "Returns true if the given object is a container instance.

See CONTAINER (type)")
  
  (type region
    "Encompasses an axis-aligned region.

This is a VEC3, wherein the coordinates designate the \"bottom left\"
corner of the region and its SIZE designates the width height and
depth of the region.

NOTE: The region, unlike is required of other objects, does not keep
the data as center and half-size. However, it does implement LOCATION
and BSIZE to provide those quantities as defined.

See 3D-VECTORS:VEC3 (type)
See REGION-SIZE
See FIND-REGION
See REGION-OVERLAPS-P
See REGION-CONTAINS-P")
  
  (function region-size
    "Accesses the region's size.

NOTE: unlike BSIZE, this is the *full size* and not a half-size.

See 3D-VECTORS:VEC3 (type)
See REGION (type)")
  
  (function do-all
    "Convenience macro to iterate over all objects in the container.

Returns the RETURN value, and executes BODY in a BLOCK NIL context.

See CALL-WITH-ALL")
  
  (function do-contained
    "Convenience macro to iterate over objects contained in the region.

Returns the RETURN value, and executes BODY in a BLOCK NIL context.

See CALL-WITH-CONTAINED")
  
  (function do-overlapping
    "Convenience macro to iterate over objects overlapping the region.

Returns the RETURN value, and executes BODY in a BLOCK NIL context.

See CALL-WITH-OVERLAPPING")
  
  (function find-region
    "Returns a region that encompasses all objects passed as tightly as possible.

Every object must implement a method for at least LOCATION and BSIZE.

See REGION (type)")
  
  (function region-overlaps-p
    "Returns true if the region overlaps with the object's axis aligned bounding box.

The object must implement a method for at least LOCATION and BSIZE.

See REGION (type)")
  
  (function region-contains-p
    "Returns true if the region entirely contains the object's axis aligned bounding box.

The object must implement a method for at least LOCATION and BSIZE.

See REGION (type)"))

(in-package #:org.shirakumo.fraf.trial.space.bvh2)

(docs:define-docs
  (type bvh
    "A binary Bounding Volume Hierarchy in 2D.

Each node in the tree is represented by an axis aligned bounding box
and may either contain a single object or two child nodes.

The tree does not automatically rebalance when objects move and may
not be optimal after all objects are inserted. Calling REOPTIMIZE will
attempt to shuffle the tree around for better search traversal
time. It is recommended to call REOPTIMIZE with :ROUNDS 10 or similar,
to optimise the tree after first building it.

There is no limit to the area that the tree can span and no canonical
center to it. The tree will automatically expand and contract as
needed to fit all objects.

See ORG.SHIRAKUMO.FRAF.TRIAL.SPACE:CONTAINER
See MAKE-BVH
See BVH-INSERT
See BVH-REMOVE
See BVH-UPDATE
See BVH-LINES")
  
  (function make-bvh
    "Creates a fresh BVH2.

See BVH (type)")
  
  (function bvh-insert
    "Fast track for ENTER.

See BVH (type)
See ORG.SHIRAKUMO.FRAF.TRIAL.SPACE:ENTER")
  
  (function bvh-remove
    "Fast track for LEAVE.

See BVH (type)
See ORG.SHIRAKUMO.FRAF.TRIAL.SPACE:LEAVE")
  
  (function bvh-update
    "Fast track for UPDATE.

See BVH (type)
See ORG.SHIRAKUMO.FRAF.TRIAL.SPACE:UPDATE")
  
  (function bvh-lines
    "Debug function.

Returns a list of lists where every two entries constitute a
line. Each entry is made up of a VEC3 for its position and a VEC4 for
its color.

The lines draw up the BVH nodes with the colour corresponding to the
node depth.

See BVH (type)"))

(in-package #:org.shirakumo.fraf.trial.space.quadtree)

(docs:define-docs
  (type quadtree
    "A spatial tree data structure in 2D similar to BVH2.

Each node of a quad-tree splits space into four equal sections that
intersect in the middle of the node's space. Each of these quarters
may be represented by a child-node or nothing each of which tries to
hold as few items as possible. If an item overlaps with the space of
multiple child nodes, it is not passed to any of them and kept in the
parent node.

The node automatically splits into sub-sections when an item count
threshold is met upon insertion and checks the need for rebalancing at
removal. Calling REOPTIMIZE manually will shuffle the tree around for
better search travelsal, but it is not recommended as the searches are
should remain fast enough. Calling UPDATE will also rebalance the tree
automatically in previous and current space relative to the passed
object.

Like with BVH2, there is no limit to the area that the tree can span
and it may cover any arbitrary area. The tree will also automatically
expand as needed to fit all objects, but it will not contract.

See ORG.SHIRAKUMO.FRAF.TRIAL.SPACE:CONTAINER
See MAKE-QUADTREE
See MAKE-QUADTREE-AT
See QUADTREE-INSERT
See QUADTREE-REMOVE
See QUADTREE-UPDATE
See QUADTREE-FIND-ALL
See QUADTREE-FIND-OVERLAPS
See QUADTREE-FIND-OVERLAPS-IN
See QUADTREE-FIND-CONTAINED
See QUADTREE-FIND-CONTAINED-IN
See QUADTREE-FIND-FOR
See QUADTREE-LINES")
  
  (function make-quadtree
    "Creates an empty QUADTREE for [0,0] to [100,100] coordinate space
with 1 as the minimum length of an edge and 1 as the threshold number
of items before the node is split.

See QUADTREE (type)
See MAKE-QUADTREE-AT")
  
  (function make-quadtree-at
    "Creates an empty QUADTREE for the defined section of space with
wanted minimum node edge size (defaults to 1) and the item count
threshold before a node splits (defaults to 1).

See QUADTREE (type)
See MAKE-QUADTREE")
  
  (function quadtree-insert
    "Fast track for ENTER. If the passed object already exists in the
tree its holding node is updated.

See QUADTREE (type)
See QUADTREE-UPDATE
See ORG.SHIRAKUMO.FRAF.TRIAL.SPACE:ENTER")
  
  (function quadtree-remove
    "Fast track for LEAVE. The holder node's state is updated if
necessary.

See QUADTREE (type)
See ORG.SHIRAKUMO.FRAF.TRIAL.SPACE:LEAVE")
  
  (function quadtree-update
    "Fast track for UPDATE. Checks if the passed object's holding node
in the tree should be changed and updates it accordingly.

See QUADTREE (type)
See QUADTREE-INSERT
See QUADTREE-REMOVE
See ORG.SHIRAKUMO.FRAF.TRIAL.SPACE:UPDATE")
  
  (function quadtree-find-all
    "Finds every object in the tree and pushes them into the passed
vector which is then returned.

See QUADTREE (type)")
  
  (function quadtree-find-overlaps
    "Finds every object in the tree that overlaps with the passed
region and pushes them into the vector which is then returned.

The region is defined by a VEC4 where VX and VY represent the top-left
point of the region, and VZ and VW the bottom-right point of the
region.

See QUADTREE (type)
See QUADTREE-FIND-OVERLAPS-IN
See QUADTREE-FIND-CONTAINED")
  
  (function quadtree-find-overlaps-in
    "Finds every object in the tree that overlaps with the passed
region and pushes them into the vector which is then returned.

The region is defined by a VEC2 where VX and VY represent the top-left
point of the region, and a second VEC2 where VX and VY are the width
and height respectively of the region.

See QUADTREE (type)
See QUADTREE-FIND-OVERLAPS
See QUADTREE-FIND-CONTAINED-IN")
  
  (function quadtree-find-contained
    "Finds every object in the tree that is completely contained by
the passed region and pushes them into the vector which is then
returned.

The region is defined by a VEC4 where VX and VY represent the top-left
point of the region, and VZ and VW the bottom-right point of the
region.

See QUADTREE (type)
See QUADTREE-FIND-CONTAINED-IN
See QUADTREE-FIND-OVERLAPS")
  
  (function quadtree-find-contained-in
    "Finds every object in the tree that is completely contained by
the passed region and pushes them into the vector which is then
returned.

The region is defined by a VEC2 where VX and VY represent the top-left
point of the region, and a second VEC2 where VX and VY are the width
and height respectively of the region.

See QUADTREE (type)
See QUADTREE-FIND-CONTAINED
See QUADTREE-FIND-OVERLAPS-IN")
  
  (function quadtree-find-for
    "Finds every object in the tree that overlaps with the passed
object that holds location and size information to it.

The object must implement a method for at least LOCATION and BSIZE.

See QUADTREE (type)
See QUADTREE-FIND-OVERLAPS-IN")
  
  (function quadtree-lines
    "Debug function.

Returns a list of lists where every two entries constitute a
line. Each entry is made up of a VEC3 for its position and a VEC4 for
its color.

The lines draw up the quadtree nodes with the colour corresponding to
the node depth.

See QUADTREE (type)"))

(in-package #:org.shirakumo.fraf.trial.space.grid3)

(docs:define-docs
  (type grid
    "A uniform bounded grid in 3D.

This is one of the simplest possible spatial structures, with
constant-time insertion, and in the general case constant-time removal
and update.

However, the structure does not automatically tune itself and ensuring
good performance requires user input. Specifically, the search time
performance is very sensitive to the cell size. Too coarse and too many
objects need to be searched. Too fine and a lot of space is wasted and
objects may be missed for fine searches.

If an object is bigger than the cell size, it may be missed for
certain searches, as the search only guarantees finding the objects in
the immediate neighbourhood of a cell.

The implementation here uses a dense array, meaning it requires
at least W*H*D+N storage.

You can update all properties of the grid, with REOPTIMIZE or
GRID-RESIZE and GRID-MOVE. Doing so is equivalent to creating a new
grid and inserting all previous entities into it. If REOPTIMIZE is
called without any arguments, the grid will recenter and refit itself
to the computed ideal bounds for all contained objects and the cell
size will adjust itself to be as big as the largest object (if any).

Thus it can be a valid strategy to just create a grid, insert all your
objects, and then call REOPTIMIZE to determine the best parameters.

Objects that are outside the grid's limits will simply be clamped to
the nearest cell within the grid. Beware of degenerating performance
if your objects do not fit within the grid's size.

See QUADTREE (type)")
  
  (function make-grid
    "Creates a new grid.

If no LOCATION is passed, it is centered at the origin. If no BSIZE is
passed, it is sized to a half-size of 100 in every direction.

See GRID (type)")
  
  (function grid-resize
    "Resizes the grid to the specified half-size and cell size.

See GRID (type)")
  
  (function grid-move
    "Moves the grid's center to the specified location.

Note that this will not change the grid's half-size or cell size.

See GRID (type)")
  
  (function grid-insert
    "Fast track for ENTER.

See GRID (type)
See ORG.SHIRAKUMO.FRAF.TRIAL.SPACE:ENTER")
  
  (function grid-remove
    "Fast track for LEAVE.

See GRID (type)
See ORG.SHIRAKUMO.FRAF.TRIAL.SPACE:LEAVE")
  
  (function grid-update
    "Fast track for UPDATE.

See GRID (type)
See ORG.SHIRAKUMO.FRAF.TRIAL.SPACE:UPDATE"))
