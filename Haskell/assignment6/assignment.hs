{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Arrow ((&&&))
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Maybe
import Data.List (unfoldr)

main :: IO ()
main = undefined

-- Zadanie 1

natPairs :: [(Integer, Integer)]
natPairs = [(j, i - j) | i <- [0..], j <- [0..i] ]


(><) :: [a] -> [b] -> [(a, b)]
a >< b = concat . takeWhile (not . null) $ flip map [1..] $ \i -> do
    let as = take i a
    let bs = reverse . take i $ b
    guard $ length as == i && length bs == i
    zip as bs
-- a >< b = concatMap ((zip . flip take a) <*> (reverse . flip take b)) [1..]
-- a >< b = concatMap (\i -> zip (take i a) (reverse . take i $ b)) [1..]

natPairs' :: [(Integer, Integer)]
natPairs' = [0..] >< [0..]

-- Zadanie 2

class Set s where
    emptyS :: s a
    searchS :: Ord a => a -> s a -> Maybe a
    insertS :: Ord a => a -> s a -> s a
    delMaxS :: Ord a => s a -> Maybe (a, s a)
    deleteS :: Ord a => a -> s a -> s a

class Dictionary d where
    emptyD :: d k v
    searchD :: Ord k => k -> d k v -> Maybe v
    insertD :: Ord k => k -> v -> d k v -> d k v
    deleteD :: Ord k => k -> d k v -> d k v

data KeyValue key value = KeyValue { key :: key, value :: value }
newtype SetToDict s k v = SetToDict (s (KeyValue k v))

instance Eq k => Eq (KeyValue k v) where
    (==) = flip (flip (==) . key) . key

instance Ord k => Ord (KeyValue k v) where
    compare = flip (flip compare . key) . key

instance Set s => Dictionary (SetToDict s) where
    emptyD = SetToDict emptyS
    searchD k (SetToDict s) = value <$> searchS (KeyValue k undefined) s
    insertD k v (SetToDict s) = SetToDict $ insertS (KeyValue k v) s
    deleteD k (SetToDict s) = SetToDict $ deleteS (KeyValue k undefined) s

-- Zadanie 3

data PrimRec = Zero | Succ | Proj Int Int
             | Comb PrimRec [PrimRec] | Rec PrimRec PrimRec

arityCheck :: PrimRec -> Maybe Int
arityCheck Zero = Just 1
arityCheck Succ = Just 1
arityCheck (Proj i n)
    | i <= n && i >= 1 = Just n
    | otherwise = Nothing

arityCheck (Comb f gs) = do
    af <- arityCheck f
    ags <- mapM arityCheck gs
    guard $ length ags == af
    let ag = head ags
    guard $ all (==ag) ags
    return ag

arityCheck (Rec g h) = do
    ag <- arityCheck g
    ah <- arityCheck h
    guard $ (ag + 2) == ah
    return $ ag + 1

-- Zadanie 4

evalPrimRec :: PrimRec -> [Integer] -> Integer
evalPrimRec p input
    | fromMaybe True $ (/=) <$> Just (length input) <*> arityCheck p = error "wrong input arity"
    | otherwise =
        case aux p input of
            Right v -> v
            Left e -> error e
  where aux :: PrimRec -> ([Integer] -> Either String Integer)
        aux Zero = const $ Right 0
        aux Succ = \v -> do
            unless (length v == 1) (throwError "length of succ input have invalid arity")
            return $ head v + 1
        aux (Proj i n)
            | i > n || i < 1 = error "invalid projection"
            | otherwise = \v -> do
                unless (length v == n) (throwError "input and projection arity differ")
                return $ v !! (i - 1)
        aux c@(Comb f gs) =
            case arityCheck c of
                Nothing -> error "invalid combination"
                Just arity -> \v -> do
                    unless (length v == arity) (throwError "input and combination arity differ")
                    egs <- mapM (flip aux v) gs
                    aux f egs
        aux r@(Rec g h) =
            case arityCheck r of
                Nothing -> error "invalid recursion"
                Just arity -> \v -> do
                    unless (length v == arity) (throwError "input and recursion arity differ")
                    let n = head v
                    let args = tail v
                    if n == 0 then
                        aux g args
                    else do
                        let nn = n - 1
                        rec <- aux r (nn:args)
                        aux h (nn:rec:args)

addition = Rec (Proj 1 1)
               (Comb Succ [Proj 2 3])

-- Zadanie 5

data Nat = S Nat | Z deriving Show

iter :: (a -> a) -> a -> Nat -> a
iter _ g Z = g
iter f g (S n) = f (iter f g n)

rec :: (Nat -> a -> a) -> a -> Nat -> a
rec f g n = snd $ iter (S . fst &&& uncurry f) (Z, g) n

add :: Nat -> Nat -> Nat
add = rec (const S)

n m = foldl (const . S) Z [1..m]
num Z = 0
num (S n) = 1 + num n

-- Zadanie 6

tail' :: [a] -> [a]
tail' l = foldr go (const []) l False
    where go x f False = f True
          go x f True = x : f True
-- tail' = reverse' . snd . foldr go brrr . reverse'
--     where brrr = (False, error "tail': list is empty")
--           go e (False, _) = (True, [])
--           go e (_, a) = (True, e : a)

reverse' :: [a] -> [a]
reverse' = foldr (flip (++) . pure) []

zip' :: [a] -> [a] -> [(a, a)]
zip' = foldr go brrr
    where brrr = const []
          go x f [] = []
          go x f (y:ys) = (x, y) : f ys

-- Zadanie 7

class ListView t where
    viewList :: t a -> List t a
    toList :: t a -> [a]
    cons :: a -> t a -> t a
    nil :: t a

data List t a = Cons a (t a) | Nil

instance ListView t => Foldable (List t) where
    foldr f a Nil = a
    foldr f a (Cons v l) = v `f` foldr f a (viewList l)

data RAList a = RAZero (RAList (a,a))
              | RAOne a (RAList (a,a))
              | RANil deriving Show

instance ListView RAList where
    nil = RANil

    cons e RANil = RAOne e RANil
    cons e (RAZero l) = RAOne e l
    cons e (RAOne v RANil) = RAZero (RAOne (e, v) RANil)
    cons e (RAOne v (RAZero l)) = RAZero . RAOne (e, v) $ l
    cons e (RAOne v (RAOne (u, w) l)) = RAZero . RAZero . cons ((e, v), (u, w)) $ l

    toList RANil = []
    toList (RAZero l) = concatMap (uncurry ((. return) . (:))) (toList l)
    toList (RAOne v l) = v : concatMap (uncurry ((. return) . (:))) (toList l)

    viewList (toList -> []) = Nil
    viewList (toList -> (x:xs)) = Cons x (foldr cons nil xs)

-- Zadanie 8

class Ord a => Prioq t a where
    empty :: t a
    isEmpty :: t a -> Bool
    single :: a -> t a
    insert :: a -> t a -> t a
    merge :: t a -> t a -> t a
    extractMin :: t a -> (a, t a)
    findMin :: t a -> a
    deleteMin :: t a -> t a
    fromList :: [a] -> t a
    toList' :: t a -> [a]
    insert = merge . single
    single = flip insert Main.empty
    extractMin t = (findMin t, deleteMin t)
    findMin = fst . extractMin
    fromList = foldr insert Main.empty
    toList' = unfoldr (\t -> (guard (isEmpty t) >>) $ return (extractMin t))

data BHeap a = BZero (BHeap (a, a))
             | BOne a (BHeap (a, a))
             | BNil deriving Show

instance Ord a => Prioq BHeap a where
    empty = BNil

    isEmpty BNil = True
    isEmpty _ = False

    single = flip BOne BNil

    merge BNil BNil = BNil
    merge l BNil = l
    merge BNil r = r
    merge (BZero l) (BZero r) = BZero $ merge l r
    merge (BOne la l) (BZero r) = insert la . BZero $ merge l r
    merge (BZero l) (BOne ra r) = insert ra . BZero $ merge l r
    merge (BOne la l) (BOne ra r)
      | la < ra = BZero . insert (la, ra) $ merge l r
      | otherwise = BZero . insert (ra, la) $ merge r l

    insert v BNil = single v
    insert v (BZero l) = fix $ BOne v l
        where fix h@(BOne _ BNil) = h
              fix h@(BOne v (BOne (a, b) l))
                | v <= a = h
                | v > a && v <= b = BOne a $ BOne (v, b) l
                | otherwise BOne a $ insert (b, v) l
              fix h@(BOne v (BZero l)) = undefined

maybeFindMin :: Ord a => BHeap a -> Maybe a
maybeFindMin BNil = Nothing
maybeFindMin (BOne v _) = Just v
maybeFindMin (BZero BNil) = Nothing
maybeFindMin (BZero (BOne (v, u) l)) = Just v
maybeFindMin (BZero l) = uncurry min <$> maybeFindMin l
