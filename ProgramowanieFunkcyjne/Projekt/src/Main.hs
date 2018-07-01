module Main where

import System.Environment(getArgs)
import qualified RayTracer.App as App

main :: IO ()
main = do
    args <- getArgs
    case head' args of
        Just filename -> App.start filename
        Nothing       -> putStrLn "You need to specify scene file!"
    where head' l
            | l == []   = Nothing
            | otherwise = Just $ head l
