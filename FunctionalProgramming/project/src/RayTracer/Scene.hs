module RayTracer.Scene where

import Debug.Trace(trace)
import Control.Monad
import Data.Maybe
import qualified Data.List as List
import Data.Word
import Data.Default
import qualified Data.Map.Strict as Map

import RayTracer.Light(Light(..))
import RayTracer.Material
import RayTracer.Primitive
import RayTracer.Camera
import RayTracer.Ray
import RayTracer.Vector
import RayTracer.AABBTree

type Dimensions = (Int, Int)
data Scene = Scene Camera Dimensions Vector (Either AABBPrecalc AABBTree) [Light] MaterialMap deriving Show

instance Default Scene where
    def = Scene def def def (Left def) [] def

getDimensions :: Scene -> Dimensions
getDimensions (Scene _ d _ _ _ _) = d

clampColor :: Vector -> Vector
clampColor = fmap (\c -> min 1.0 (max 0.0 c))

renderPixel :: Scene -> Scalar -> Scalar -> Vector
renderPixel (Scene cam size background (Right aabbTree) lights materials) x y =
    let width = (fromIntegral . Prelude.fst $ size) in
    let height = (fromIntegral . Prelude.snd $ size) in
    let origin = getPosition cam in
    let fov = getFov cam in
    let aspect = width / height in
    let world = normalized $ Vector (((-1.0) + 2.0 * (x + 0.5) / width) * aspect * fov)
                                    (((-1.0) + 2.0 * (y + 0.5) / height) * fov)
                                    (-1.0) in
    let direction = worldToCamera cam world in
    let ray = createRay origin direction in
    case traceRay ray aabbTree lights materials of
        Just color -> clampColor color
        Nothing -> background
