module RayTracer.Camera where

import Data.Default

import RayTracer.Vector

-- Camera position <u, v, w> fov
data Camera = Camera Vector (VecT Vector) Scalar deriving Show

instance Default Camera where
    def = Camera def (Vector def def def) def

createCamera :: Vector -> Vector -> Scalar -> Camera
createCamera pos lookAt fov =
    let upVec = Vector 0.0 0.0 (-1.0) in
    let w = normalized $ pos <-> lookAt in
    let u = normalized $ upVec >< w in
    let v = w >< u in
    Camera pos (Vector u v w) (tan $ pi * fov / 360.0)

worldToCamera :: Camera -> Vector -> Vector
worldToCamera (Camera _ (Vector u v w) _)
              (Vector x y z) =
    (x <**> u) <+> (y <**> v) <+> (z <**> w)

getFov :: Camera -> Scalar
getFov (Camera _ _ fov) = fov

getPosition :: Camera -> Vector
getPosition (Camera p _ _) = p
