{-|
Module: MiodoweInput
Description: Module for parsing data for Miodowe
Copyright: Marcin Witkowski

INCOMPLETE (without parser for list)
|-}
module MiodoweInput (miodoweInput) where

import Data.Char
import Control.Monad
import MiodoweSolver (Miodowe(..))

miodoweInput :: String -> IO Miodowe
miodoweInput file = do
    contents <- readFile file
    let xs = lines contents
    return $ case parseLines xs of
        Just m -> m
        Nothing -> error "Invalid input file"

maybeSplit :: [a] -> Maybe (a, [a])
maybeSplit [] = Nothing
maybeSplit (x:xs) = Just (x, xs)

parseNum :: String -> Maybe Int
parseNum s = do
    guard $ all isDigit s
    return $ foldl (\a x -> a * 10 + digitToInt x) 0 s

parseList :: String -> Maybe [(Int, Int)]
parseList s = undefined -- TODO

parseLines :: [String] -> Maybe Miodowe
parseLines l = do
    guard $ length l == 4
    (strSize, tail) <- maybeSplit l
    size <- parseNum strSize
    (strIslandsNum, tail) <- maybeSplit tail
    islandsNum <- parseNum strIslandsNum
    (strIslandSize, tail) <- maybeSplit tail
    islandSize <- parseNum strIslandSize
    (strCells, tail) <- maybeSplit tail
    cells <- parseList strCells
    return $ Miodowe {
        size = size,
        islands_num = islandsNum,
        island_size = islandSize,
        cells = cells
    }


