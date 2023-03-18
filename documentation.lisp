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
