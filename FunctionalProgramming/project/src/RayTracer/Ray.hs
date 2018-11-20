module RayTracer.Ray where

import RayTracer.Vector

data Ray = Ray Vector Vector Int deriving Show-- Ray origin direction depth

createRay :: Vector -> Vector -> Ray
createRay o d = Ray o d 0

nextRay :: Ray -> Vector -> Vector -> Ray
nextRay (Ray _ _ d) origin direction = Ray origin direction (d + 1)

getOrigin :: Ray -> Vector
getOrigin (Ray o _ _) = o

getDirection :: Ray -> Vector
getDirection (Ray _ d _) = d

getRayVector :: Ray -> Scalar -> Vector
getRayVector (Ray o d _) dist = o <+> (dist <**> d)

getRayDepth :: Ray -> Int
getRayDepth (Ray _ _ d) = d

getMaxRayDepth :: Int
getMaxRayDepth = 8
