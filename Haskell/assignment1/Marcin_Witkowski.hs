Marcin Witkowski
Kurs Haskell
Lista 1
04.03.2020

\begin{code}
import Data.Char (isDigit, digitToInt)
import Control.Monad.State.Lazy
import Control.Arrow
import Control.Monad (forM)

-- Suppress linter errors
main :: IO ()
main = undefined
\end{code}

Zadanie 1

\begin{code}
intercalate :: [a] -> [[a]] -> [a]
intercalate _ [] = []
intercalate sep (x:xs) = x ++ concatMap (sep ++) xs

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:ms) = transpose ms
transpose m = map head m : transpose (map tail m)

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ Main.concat xs

and :: [Bool] -> Bool
and = foldl (&&) True

all :: (a -> Bool) -> [a] -> Bool
all f = Prelude.and . map f

maximum :: [Integer] -> Integer
maximum [x] = x
maximum (x:xs) = foldl max x xs
\end{code}

Zadanie 2

\begin{code}
newtype Vector a = Vector { fromVector :: [a] }

sameSize :: (Vector a -> Vector a -> b) -> Vector a -> Vector a -> b
sameSize f a b
    | length as == length bs = f a b
    | otherwise = error "vectors don't have equal lenghts"
  where
      as = fromVector a
      bs = fromVector b

scaleV :: Num a => a -> Vector a -> Vector a
scaleV a = Vector . map (a *) . fromVector

norm :: Floating a => Vector a -> a
norm = sqrt . sum . map (**2) . fromVector

scalarProd :: Num a => Vector a -> Vector a -> a
scalarProd = sameSize $ curry $ sum . uncurry (flip (zipWith (*) . fromVector) . fromVector)

sumV :: Num a => Vector a -> Vector a -> Vector a
sumV = sameSize $ curry $ Vector . uncurry (flip (zipWith (+) . fromVector) . fromVector)
\end{code}

Zadanie 3

\begin{code}
newtype Matrix a = Matrix { fromMatrix :: [[a]] }

sumM :: Num a => Matrix a -> Matrix a -> Matrix a
sumM m n
  | lm /= ln = error "matrices have incompatible sizes"
  | otherwise = Matrix $ map fromVector $ zipWith sumV (map Vector $ fromMatrix m) (map Vector $ fromMatrix n)
  where
      lm = (length &&& length . head) . fromMatrix $ m
      ln = (length &&& length . head) . fromMatrix $ n

prodM :: Num a => Matrix a -> Matrix a -> Matrix a
prodM m n
  | lm /= ln = error "matrices have incompatible sizes"
  | otherwise = Matrix $ map (\v -> map (sum . zipWith (*) v) (transpose . fromMatrix $ n)) (fromMatrix m)
  where
      lm = length . fromMatrix $ m
      ln = length . head . fromMatrix $ n

det :: Num a => Matrix a -> a
det m
  | length ma == 1 = head . head . fromMatrix $ m
  | otherwise = sum $ zipWith (curry $ uncurry (*) . (det . fst &&& snd)) minors (zipWith (*) x $ cycle [1, -1])
    where ma = fromMatrix m
          (x:xs) = ma
          minors = [Matrix $ map (map snd . filter ((/=k) . fst) . zip [0..]) xs | k <- [0..length ma - 1]]
\end{code}

Zadanie 4

\begin{code}
isbn13_check :: String -> Bool
isbn13_check isbn =
        length filtered == 13 &&
        (sum . zipWith (*) filtered $ cycle [1, 3]) `mod` 10 == 0
    where
        filtered = map digitToInt $ filter isDigit isbn
\end{code}

Zadanie 5

\begin{code}
newtype Natural = Natural { fromNatural :: [Word] }

base :: Word
base = (1+) . fromInteger . floor . sqrt . fromInteger . toInteger $ (maxBound :: Word)
\end{code}

Zadanie 6

\begin{code}
reverse_pad :: Natural -> Natural -> ([Word], [Word])
reverse_pad a b = (pa, pb)
    where aux :: [Word] -> [Word] -> ([Word], [Word])
          aux [] [] = ([0], [0])
          aux (x:xs) [] = let (a, b) = aux xs [] in (x:a, 0:b)
          aux [] (y:ys) = let (a, b) = aux [] ys in (0:a, y:b)
          aux (x:xs) (y:ys) = let (a, b) = aux xs ys in (x:a, y:b)
          ra = reverse . fromNatural $ a
          rb = reverse . fromNatural $ b
          (pa, pb) = aux ra rb

paddings :: [[Word]]
paddings = [replicate x 0 | x <- [0..]]

instance Num Natural where
    a + b = Natural . dropWhile (==0) . reverse $ res
            where (pa, pb) = reverse_pad a b
                  (res, acc) = flip runState 0 $ forM (zip pa pb) $ \(x, y) -> do
                                  acc <- get
                                  let sum = x + y + acc
                                  put $ sum `div` base
                                  return $ sum `mod` base
    a - b
        | a < b = error "You can't go under zero"
        | otherwise = Natural . dropWhile (==0) . reverse $ res
                      where (pa, pb) = reverse_pad a b
                            (res, acc) = flip runState 0 $ forM (zip pa pb) $ \(x, y) -> do
                                            acc <- get
                                            let borrow = x < y + acc
                                            let new_x = if borrow then x + base - (y + acc) else x - (y + acc)
                                            put $ if borrow then 1 else 0
                                            return new_x

    a * b = foldl (+) (Natural []) to_sum
            where (pa, pb) = reverse_pad a b
                  to_sum = flip map (zip paddings pb) $ \(p, v) -> Natural . reverse $ (++) p $ map (v*) pa

    abs = id

    signum n = case n of
                Natural [] -> 0
                _ -> 1

    fromInteger = Natural . dropWhile (==0)  . reverse . aux
        where aux :: Integer -> [Word]
              aux v
                | v < 0 = error "Naturals can't be negative"
                | v == 0 = []
                | otherwise = fromInteger (v `mod` toInteger base) : aux (fromInteger $ v `div` toInteger base)
\end{code}

Zadanie 7

\begin{code}
instance Eq Natural where
    a == b = Prelude.all (uncurry (==)) $ zip (fromNatural a) (fromNatural b)

instance Ord Natural where
    a `compare` b
        | la < lb = LT
        | la == lb  = as `compare` bs
        | otherwise = GT
                where as = fromNatural a
                      bs = fromNatural b
                      la = length as
                      lb = length bs
\end{code}

Próba do zadania 8/9

\begin{code}
simpleDiv :: Natural -> Natural -> (Natural, Natural)
simpleDiv (Natural []) _ = (Natural [], Natural [])
simpleDiv a b
    | a >= b = let (div, mod) = simpleDiv (a - b) b in (div + Natural [1], mod)
    | otherwise = (Natural [], a)

longDiv :: [Word] -> [Word] -> [Word] -> ([Word], [Word])
longDiv [] b x = (fromNatural div, fromNatural mod)
    where (div, mod) = simpleDiv (Natural x) (Natural b)
longDiv a@(aa:as) b x
  | Natural x < Natural b = longDiv as b (x ++ [aa])
  | otherwise = (fromNatural div ++ nndiv, nmod)
        where (div, mod) = simpleDiv (Natural x) (Natural b)
              (ndiv, nmod) = longDiv a b (fromNatural mod)
              nndiv = if null ndiv then [0] else ndiv

instance Show Natural where
    show = show . fromNatural
\end{code}
