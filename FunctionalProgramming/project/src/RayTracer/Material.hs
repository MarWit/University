module RayTracer.Material where

import Debug.Trace(trace)
import Data.Default
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe
import Control.Monad

import RayTracer.Vector
import RayTracer.Light
import RayTracer.Primitive
import RayTracer.Ray
import RayTracer.Light
import RayTracer.AABBTree

data Material = Phong Vector Scalar Scalar Scalar                       | -- Phong <r,g,b> diffuse specular specular_exponent
                Reflective Vector Material Scalar                       | -- Reflective <r,g,b> base_material reflectivity
                Transparent Vector Material Scalar Scalar Scalar          -- Transparent <r,g,b> base_material reflectivity refraction transmission
                deriving Show

type MaterialMap = Map.Map String Material

traceRay :: Ray -> AABBTree -> [Light] -> MaterialMap -> Maybe Vector
traceRay ray aabbTree lights materials = do
    traceResult <- treeIntersection aabbTree ray
    let material = materials Map.! (getMaterial . getPrimitive $ traceResult)
    return $ calculateRadiance material aabbTree lights materials ray traceResult

obstacleBetween :: Vector -> Vector -> AABBTree -> Bool
obstacleBetween pointA pointB aabbTree =
    let direction = normalized $ (pointB <-> pointA) in
    let distance = norm $ (pointB <-> pointA) in
    let ray = createRay (pointA <+> (0.0000001 <**> direction)) direction in
    case treeIntersection aabbTree ray of
        Just traceResult ->
            let abDistance = (getDistance traceResult) in
            abDistance > 0.0 && abDistance < distance
        Nothing -> False

calculatePhongFactor :: Vector -> Vector -> Vector -> Scalar -> Scalar
calculatePhongFactor lightDir normal camDir exponent =
    let reflected = reflect lightDir normal in
    let angle = reflected <.> camDir in
    if angle <= 0.000001 then
        0.0
    else
        angle ** exponent

calculateTransmissionDirection :: Vector -> Vector -> Scalar -> Scalar -> Scalar -> Vector
calculateTransmissionDirection direction normal eta refractionCoefficent cosineAngle =
    let normal_ = if cosineAngle < 0.000001 then normal else vneg normal in
    let cosineAngle_ = if cosineAngle < 0.000001 then -cosineAngle else cosineAngle in

    normalized $ ((1.0 / eta) <**> direction) <+> ((cosineAngle / eta - (sqrt refractionCoefficent)) <**> normal_)

calculateRadiance :: Material -> AABBTree -> [Light] -> MaterialMap -> Ray -> TraceResult -> Vector

-- calculateRadiance template
-- calculateRadiance (MyNewMaterial color)
--                     aabbTree lights materials ray tr =
--     List.foldl (<+>) (Vector 0.0 0.0 0.0) $ flip mapMaybe lights $ \light -> do
--         -- light = Light source
--         -- Return: Maybe Color
--         return color

calculateRadiance (Phong color diffuse specular specularExp)
                    aabbTree lights materials ray tr =
    List.foldl (<+>) (Vector 0.0 0.0 0.0) $ flip mapMaybe lights $ \light -> do
        let lightPos = getPosition light
        let hitPoint = getHitPoint tr ray

        guard . not $ obstacleBetween hitPoint lightPos aabbTree

        let lightDir = normalized $ lightPos <-> hitPoint
        let factor = lightDir <.> getNormal tr

        guard $ factor >= 0.0

        let diffuseColor = vmult color $ getColor light hitPoint
        let phongFactor = calculatePhongFactor lightDir (getNormal tr) (vneg . getDirection $ ray) specularExp
        let phongColor = (phongFactor * specular) <**> color

        return $ (diffuse * factor) <**> diffuseColor <+> phongColor

calculateRadiance (Reflective color phong reflectivity)
                    aabbTree lights materials ray tr =
    List.foldl (<+>) (Vector 0.0 0.0 0.0) $ flip mapMaybe lights $ \light -> do
        guard $ getRayDepth ray < getMaxRayDepth

        let phongShader = calculateRadiance phong aabbTree lights materials ray tr

        let rayDirection = getDirection ray
        let objectNormal = getNormal tr
        let reflectionDir = normalized $ reflect (vneg rayDirection) objectNormal
        let reflectionRay = nextRay ray ((0.0000001 <**> reflectionDir) <+> getHitPoint tr ray) reflectionDir
        let reflectionColor = maybe def id $ traceRay reflectionRay aabbTree lights materials

        return $ phongShader <+> (reflectivity <**> (vmult color reflectionColor))

calculateRadiance (Transparent color phong reflectivity refraction transmission)
                    aabbTree lights materials ray tr =
    List.foldl (<+>) (Vector 0.0 0.0 0.0) $ flip mapMaybe lights $ \light -> do
        guard $ getRayDepth ray < getMaxRayDepth

        let phongShader = calculateRadiance phong aabbTree lights materials ray tr

        -- Reflection (almost copy-paste from Reflective material..)
        let rayDirection = getDirection ray
        let objectNormal = getNormal tr
        let reflectionDir = normalized $ reflect (vneg rayDirection) objectNormal
        let reflectionRay = nextRay ray ((0.0000001 <**> reflectionDir) <+> getHitPoint tr ray) reflectionDir
        let reflectedColor = maybe def id $ traceRay reflectionRay aabbTree lights materials

        -- Refraction
        let cosineAngle = (min 1.0 . max (-1.0)) $ objectNormal <.> (vneg rayDirection)
        let eta = if cosineAngle > 0.0 then refraction else 1.0 / refraction
        let refractionCoefficent = 1.0 - (1.0 - cosineAngle ** 2) / (eta ** 2)

        -- Total internal reflection; it will behave like reflective material
        if refractionCoefficent < 0.0 then
            return $ phongShader <+> reflectedColor
        else
            let transmissionDir = calculateTransmissionDirection rayDirection objectNormal eta refractionCoefficent cosineAngle in
            let transmissionRay = nextRay ray ((0.0000001 <**> transmissionDir) <+> getHitPoint tr ray) transmissionDir in
            let transmitedColor = maybe def id $ traceRay transmissionRay aabbTree lights materials in

            let reflectionColor = reflectivity <**> color in
            let transmissionColor = (transmission / (eta ** 2)) <**> Vector 1.0 1.0 1.0 in

            return $ phongShader <+> (vmult reflectionColor reflectedColor) <+> (vmult transmissionColor transmitedColor)
