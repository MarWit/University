module RayTracer.Primitive where

import Control.Monad
import Debug.Trace(trace)

import RayTracer.Vector
import RayTracer.Ray
import RayTracer.AABB(AABB(..))

data Primitive =
    Polygon [Vector] String       | -- Polygon [Vertices] <material>
    Sphere Vector Scalar String   | -- Sphere <vertex> <radius> <material>
    Plane Vector Vector String      -- Plane <vertex> <normal> <material>
    deriving Show

intersect :: Primitive -> Ray -> Maybe TraceResult
intersect p@(Sphere center radius _) ray = do
    let origin = getOrigin ray
    let direction = getDirection ray

    let l = center <-> origin
    let adj = l <.> direction
    let d2 = (l <.> l) - (adj ** 2)
    let rad2 = radius ** 2

    guard $ d2 <= rad2

    let thc = sqrt $ rad2 - d2
    let t0 = adj - thc
    let t1 = adj + thc

    guard $ (t0 >= 0.0) || (t1 >= 0.0)

    let len = min t0 t1
    let hit = origin <+> (len <**> direction)
    let normal = normalized $ hit <-> center

    return $ TraceResult p len normal

intersect p@(Plane center normal _) ray = do
    let origin = getOrigin ray
    let direction = getDirection ray

    let x = direction <.> normal
    guard $ (abs x) > 0.0001 -- Can be pararell to plane

    let t = ((center <-> origin) <.> normal) / x
    guard $ t > 0.0001 -- Maybe change this to some constant?

    return $ TraceResult p t normal

getMaterial :: Primitive -> String

getMaterial (Sphere _ _ m) = m
getMaterial (Plane _ _ m) = m

getAABB :: Primitive -> Maybe AABB

getAABB (Sphere origin radius _) =
    let vmin = origin <-> (Vector radius radius radius) in
    let vmax = origin <+> (Vector radius radius radius) in
    Just $ AABB vmin vmax

getAABB (Plane _ _ _) = Nothing

data TraceResult = TraceResult Primitive Scalar Vector deriving Show

getPrimitive :: TraceResult -> Primitive
getPrimitive (TraceResult p _ _)  = p

getDistance :: TraceResult -> Scalar
getDistance (TraceResult _ d _) = d

getNormal :: TraceResult -> Vector
getNormal (TraceResult _ _ n) = n

getHitPoint :: TraceResult -> Ray -> Vector
getHitPoint (TraceResult _ d _) ray =
    let origin = getOrigin ray in
    let direction = getDirection ray in
    origin <+> (d <**> direction)
