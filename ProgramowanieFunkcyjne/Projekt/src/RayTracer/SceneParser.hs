module RayTracer.SceneParser where

import Text.Parsec (try)
import Text.Parsec.Char (string, digit, char, letter, space, newline)
import Text.Parsec.Combinator (many1, sepEndBy1, choice, option)
import Text.Parsec.String
import Text.Parsec.Error (ParseError)

import Control.Applicative ((<*>), (<|>), (<$>), many)
import Control.Monad (void)
import Data.Default
import qualified Data.Map.Strict as Map

import RayTracer.Material as Material
import qualified RayTracer.Light as L
import qualified RayTracer.Primitive as P
import qualified RayTracer.Vector as V
import RayTracer.Scene
import RayTracer.Camera
import RayTracer.AABBTree

data Expression =
    EScene Dimensions             |
    EBackground V.Vector          |
    ECamera Camera                |
    EMaterial String Material     |
    EPrimitive P.Primitive        |
    ELight L.Light

num :: Parser Int
num = do
    n <- many1 digit
    return $ read n

float :: Parser V.Scalar
float = fmap read $ (++) <$> (plus <|> minus <|> number) <*> decimal
    where plus = char '+' *> number
          minus = (:) <$> char '-' <*> number
          decimal = option "" $ (:) <$> char '.' <*> number
          number = many1 digit

word :: Parser String
word = many1 letter

whitespaces :: Parser ()
whitespaces = void $ many (char ' ')

lexeme :: Parser p -> Parser p
lexeme p = do
    whitespaces
    r <- p
    return r

vector :: Parser V.Vector
vector = do
    x <- float
    space
    y <- float
    space
    z <- float
    return $ V.Vector x y z

loadSceneFromFile :: String -> IO (Either ParseError Scene)
loadSceneFromFile = parseFromFile parseScene

parseScene :: Parser Scene
parseScene = do
    expr <- sepEndBy1 parseCommand (many1 newline)
    return $ updateScene expr def

parseCommand :: Parser Expression
parseCommand = parseCamera     <|>
               parseBackground <|>
               parseDimensions <|>
               parseMaterial   <|>
               parsePrimitive  <|>
               parseLight

parseCamera :: Parser Expression
parseCamera = do
    try . string $ "camera"
    pos    <- lexeme vector
    lookAt <- lexeme vector
    fov    <- lexeme float
    return $ ECamera $ createCamera pos lookAt fov

parseBackground :: Parser Expression
parseBackground = do
    try . string $ "background"
    color <- lexeme vector
    return $ EBackground color

parseDimensions :: Parser Expression
parseDimensions = do
    try . string $ "scene"
    width  <- lexeme num
    height <- lexeme num
    return $ EScene (width, height)

parseMaterial :: Parser Expression
parseMaterial = parseMaterialPhong <|> parseMaterialReflective <|> parseMaterialTransparent

parseMaterialPhong :: Parser Expression
parseMaterialPhong = do
    try . string $ "matphong"
    name        <- lexeme word
    color       <- lexeme vector
    diffuse     <- lexeme float
    specular    <- lexeme float
    specularExp <- lexeme float
    return $ EMaterial name $ Phong color diffuse specular specularExp

parseMaterialReflective :: Parser Expression
parseMaterialReflective = do
    try . string $ "matreflec"
    name         <- lexeme word
    color        <- lexeme vector
    diffuse      <- lexeme float
    specular     <- lexeme float
    specularExp  <- lexeme float
    reflectivity <- lexeme float
    return $ EMaterial name $ Reflective color (Phong color diffuse specular specularExp) reflectivity

parseMaterialTransparent :: Parser Expression
parseMaterialTransparent = do
    try . string $ "mattrans"
    name         <- lexeme word
    color        <- lexeme vector
    diffuse      <- lexeme float
    specular     <- lexeme float
    specularExp  <- lexeme float
    reflectivity <- lexeme float
    refraction   <- lexeme float
    transmission <- lexeme float
    return $ EMaterial name $ Transparent color (Phong color diffuse specular specularExp) reflectivity refraction transmission

parsePrimitive :: Parser Expression
parsePrimitive = parseSphere <|> parsePlane

parseSphere :: Parser Expression
parseSphere = do
    try . string $ "sphere"
    pos    <- lexeme vector
    radius <- lexeme float
    mat    <- lexeme word
    return $ EPrimitive $ P.Sphere pos radius mat

parsePlane :: Parser Expression
parsePlane = do
    try . string $ "plane"
    pos     <- lexeme vector
    normal  <- lexeme vector
    mat     <- lexeme word
    return $ EPrimitive $ P.Plane pos normal mat

parseLight :: Parser Expression
parseLight = parsePointLight

parsePointLight :: Parser Expression
parsePointLight = do
    try . string $ "light"
    pos       <- lexeme vector
    color     <- lexeme vector
    intensity <- lexeme float
    return $ ELight $ L.Point pos color intensity

updateScene :: [Expression] -> Scene -> Scene

updateScene [] (Scene c d b (Left p) l m) = Scene c d b (Right $ createTree p) l m
updateScene (EScene dimensions : xs) (Scene c _ b p l m) = updateScene xs (Scene c dimensions b p l m)
updateScene (ECamera camera : xs) (Scene _ d b p l m) = updateScene xs (Scene camera d b p l m)
updateScene (EMaterial name mat : xs) (Scene c d b p l m) = updateScene xs (Scene c d b p l (Map.insert name mat m))
updateScene (EPrimitive primitive : xs) (Scene c d b (Left p) l m) = updateScene xs (Scene c d b (Left $ updatePrecalc p primitive) l m)
updateScene (ELight light : xs) (Scene c d b p l m) = updateScene xs (Scene c d b p (light : l) m)
updateScene (EBackground color : xs) (Scene c d _ p l m) = updateScene xs (Scene c d color p l m)
