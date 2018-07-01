module RayTracer.AABB where

import Control.Monad
import Data.Default

import qualified RayTracer.Vector as V
import RayTracer.Ray

data AABB = AABB V.Vector V.Vector deriving Show

instance Default AABB where
    def = AABB def def

-- Extends boundary box
-- NOTE: We can probably simplify this
extend :: AABB -> AABB -> AABB
extend (AABB min1 max1) (AABB min2 max2) =
    let xmin = min (V.fst min1) (V.fst min2) in
    let xmax = max (V.fst max1) (V.fst max2) in
    let ymin = min (V.snd min1) (V.snd min2) in
    let ymax = max (V.snd max1) (V.snd max2) in
    let zmin = min (V.trd min1) (V.trd min2) in
    let zmax = max (V.trd max1) (V.trd max2) in
    let vmin = V.Vector xmin ymin zmin in
    let vmax = V.Vector xmax ymax zmax in
    AABB vmin vmax

-- Calculates center of AABB
getCenter :: AABB -> V.Vector
getCenter (AABB vmin vmax) =
    let x = V.fst vmin + (V.fst vmax - V.fst vmin) / 2.0 in
    let y = V.snd vmin + (V.snd vmax - V.snd vmin) / 2.0 in
    let z = V.trd vmin + (V.trd vmax - V.trd vmin) / 2.0 in
    V.Vector x y z

--   ___________
--  /____/_____/|
-- /____/_____/||
-- |    |     |||
-- |____|____ |/|
-- |    |     ||/
-- |____|_____|/
--
-- Splits boundary box on 8 smaller boxes
splitEight :: AABB -> [AABB]
splitEight aabb@(AABB vmin@(V.Vector xmin ymin zmin) vmax@(V.Vector xmax ymax zmax)) =
    let center@(V.Vector xcen ycen zcen) = getCenter aabb in
    AABB vmin center :
    AABB (V.Vector xcen ymin zmin) (V.Vector xmax ycen zcen) :
    AABB (V.Vector xmin ymin zcen) (V.Vector xcen ycen zmax) :
    AABB (V.Vector xcen ymin zcen) (V.Vector xmax ycen zmax) :
    AABB (V.Vector xmin ycen zmin) (V.Vector xcen ymax zcen) :
    AABB (V.Vector xcen ycen zmin) (V.Vector xmax ymax zcen) :
    AABB (V.Vector xmin ycen zcen) (V.Vector xcen ymax zmax) :
    AABB center vmax : []

-- Checks if point is in boundary box
contains :: AABB -> V.Vector -> Bool
contains (AABB (V.Vector xmin ymin zmin) (V.Vector xmax ymax zmax)) (V.Vector x y z) =
    (x >= xmin) && (x <= xmax) &&
    (y >= ymin) && (y <= ymax) &&
    (z >= zmin) && (z <= zmax)

-- Checks if first AABB is fully contained in the second one
fullyContains :: AABB -> AABB -> Bool
fullyContains (AABB vmin vmax) aabb =
    contains aabb vmin && contains aabb vmax

-- Checks if ray intersects with AABB
boxIntersection :: AABB -> Ray -> Maybe V.Scalar
boxIntersection (AABB (V.Vector xmin ymin zmin) (V.Vector xmax ymax zmax)) ray = do
    let (V.Vector ox oy oz) = getOrigin ray
    let (V.Vector dx dy dz) = getDirection ray

    let t1 = (xmin - ox) / dx
    let t2 = (xmax - ox) / dx
    let t3 = (ymin - oy) / dy
    let t4 = (ymax - oy) / dy
    let t5 = (zmin - oz) / dz
    let t6 = (zmax - oz) / dz

    let tmin = max (max (min t1 t2) (min t3 t4)) (min t5 t6)
    let tmax = min (min (max t1 t2) (max t3 t4)) (max t5 t6)

    guard $ tmax >= 0.0 && tmax >= tmin

    return tmin
