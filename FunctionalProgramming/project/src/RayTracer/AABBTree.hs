module RayTracer.AABBTree where

import Debug.Trace(trace)

import Control.Monad
import qualified Data.List as List
import Data.Maybe

import qualified RayTracer.Vector as V
import RayTracer.AABB
import RayTracer.Primitive
import RayTracer.Ray

data AABBNode = Node AABB V.Vector [Primitive] (Maybe [AABBNode]) deriving Show
data AABBTree = Root AABBNode [Primitive] deriving Show

-- We will construct tree from this..
type AABBPrecalc = (AABB, [Primitive], [Primitive]) -- (AABB, Primitives w/ AABB, Primitives w/o AABB)

updatePrecalc :: AABBPrecalc -> Primitive -> AABBPrecalc
updatePrecalc (aabb, primit1, primit2) primitive =
    case getAABB primitive of
        Just paabb -> (extend paabb aabb, primitive : primit1, primit2)
        Nothing -> (aabb, primit1, primitive : primit2)

createTree :: AABBPrecalc -> AABBTree
createTree (aabb, primit1, primit2) = Root (aux aabb primit1) primit2
    where   aux :: AABB -> [Primitive] -> AABBNode
            aux aabb primitives =
                if List.length primitives <= 16 then
                    Node aabb (getCenter aabb) primitives Nothing
                else
                    let splits = splitEight aabb in
                    let (mapped, rest) = mapAccum (\aabb primitives ->
                                let (inside, outside) = filterRemain (\e -> fullyContains (maybe undefined id (getAABB e)) aabb) primitives in
                                (aux aabb inside, outside) ) splits primitives in
                    Node aabb (getCenter aabb) rest (Just mapped)

filterRemain :: (a -> Bool) -> [a] -> ([a], [a])
filterRemain f l = aux f l ([], [])
    where aux :: (a -> Bool) -> [a] -> ([a], [a]) -> ([a], [a])
          aux _ [] a = a
          aux f (x : xs) (good, bad)
                | f x = aux f xs (x : good, bad)
                | otherwise = aux f xs (good, x : bad)

mapAccum :: (a -> t -> (b, t)) -> [a] -> t -> ([b], t)
mapAccum f l d = aux f l d []
    where aux f [] d a = (a, d)
          aux f (x : xs) d a =
            let (mapped, rest) = f x d in
            aux f xs rest (mapped : a)

nodeIntersection :: AABBNode -> Ray -> V.Scalar -> Maybe TraceResult
nodeIntersection (Node aabb center primitives nodes) ray minimumDistance = do
    dist <- boxIntersection aabb ray
    guard $ dist < minimumDistance
    let intersections = mapMaybe (\p -> intersect p ray) primitives
    case intersections of
        [] ->
            let nodeIntersections = mapMaybe (\n -> nodeIntersection n ray minimumDistance) (maybe [] id nodes) in
            if List.null nodeIntersections then Nothing
            else Just $ List.minimumBy (\x y -> compare (getDistance x) (getDistance y)) nodeIntersections
        otherwise ->
            let closest = List.minimumBy (\x y -> compare (getDistance x) (getDistance y)) intersections in
            let nodeIntersections = mapMaybe (\n -> nodeIntersection n ray (getDistance closest)) (maybe [] id nodes) in
            if List.null nodeIntersections then Just closest
            else Just $ List.minimumBy (\x y -> compare (getDistance x) (getDistance y)) $ closest : nodeIntersections

treeIntersection :: AABBTree -> Ray -> Maybe TraceResult
treeIntersection (Root node primitives) ray =
    let normalIntersections = mapMaybe (\p -> intersect p ray) primitives in
    if List.null normalIntersections then
        nodeIntersection node ray infinity
    else
        let best = List.minimumBy (\x y -> compare (getDistance x) (getDistance y)) normalIntersections in
        maybe (Just best) Just (nodeIntersection node ray (getDistance best))
    where infinity = 1/0
