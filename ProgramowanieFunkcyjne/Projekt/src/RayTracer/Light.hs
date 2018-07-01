module RayTracer.Light where

import RayTracer.Vector

data Light = Point Vector Vector Scalar -- Sun <pos> <color> <power>
             deriving Show

getPosition :: Light -> Vector
getPosition (Point p _ _) = p

getColor :: Light -> Vector -> Vector
getColor (Point pos color power) objectPos
    | power >= 0.0 = (power / (4.0 * pi * abs (objectPos <.> pos))) <**> color
    | otherwise = color
