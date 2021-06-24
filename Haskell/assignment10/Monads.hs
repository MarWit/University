{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Prelude hiding (Monad, return) -- note: not hiding (>>=)
import qualified Control.Monad
import Test.QuickCheck

class (Functor m) => Monad m where
  return :: a -> m a
  join :: m (m a) -> m a

-------------------------
-- Kill-all list monad(?)
-------------------------

isSingle :: [a] -> Bool
isSingle [a] = True
isSingle _   = False

trivial :: [[a]] -> Bool
trivial [xs] = True
trivial xss  = all isSingle xss

newtype KAList a = KAList { unKAList :: [a] }
  deriving (Functor, Eq)

instance (Show a) => Show (KAList a) where
  show (KAList xs) = show xs

instance Monad KAList where
  return x = KAList [x]
  join (KAList xss) = KAList $ if
    | trivial xss' -> concat xss'
    | otherwise    -> []
   where
    xss' = map unKAList xss

instance (Arbitrary a) => Arbitrary (KAList a) where
    arbitrary = KAList <$> arbitrary

prop_kalist1 :: KAList Int -> Bool
prop_kalist1 m = (join . fmap return $ m) == m
prop_kalist2 :: KAList Int -> Bool
prop_kalist2 m = (join . return $ m) == m
prop_kalist3 :: KAList (KAList (KAList Int)) -> Bool
prop_kalist3 m = (join . fmap join $ m) == (join . join $ m)

-- Postaw ptaszka:
-- [🐦] Monada
-- [  ] Nie monada
--
-- Jeśli nie monada, to wpisz kontrprzykład:


--------------------------------
-- Kill singletons list monad(?)
--------------------------------

newtype KSList a = KSList { unKSList :: [a] }
  deriving (Functor, Eq)

instance (Show a) => Show (KSList a) where
  show (KSList xs) = show xs

instance Monad KSList where
  return x = KSList [x]
  join (KSList xss) = KSList $ if
    | trivial xss' -> concat xss'
    | otherwise    -> concat $ filter (not . isSingle) xss'
   where
    xss' = map unKSList xss

instance (Arbitrary a) => Arbitrary (KSList a) where
    arbitrary = KSList <$> arbitrary
    -- arbitrary = KSList <$> suchThat arbitrary ((==1) . length)

prop_kslist1 :: KSList Int -> Bool
prop_kslist1 m = (join . fmap return $ m) == m
prop_kslist2 :: KSList Int -> Bool
prop_kslist2 m = (join . return $ m) == m
prop_kslist3 :: KSList (KSList (KSList Int)) -> Bool
prop_kslist3 m = (join . fmap join $ m) == (join . join $ m)

-- Postaw ptaszka:
-- [  ] Monada
-- [🐦] Nie monada
--
-- Jeśli nie monada, to wpisz kontrprzykład:
-- KSList . fmap (KSList . (fmap KSList)) $ [[[-3],[3,0]],[[-3,2]],[[-1,3,-1]]]

----------------------------
-- Kill primes list monad(?)
----------------------------

isPrime :: Int -> Bool
isPrime n = all (\k -> n `mod` k /= 0) $ takeWhile (\k -> k*k <= n) primes

primes :: [Int]
primes = 2 : [n | n <- [3,5..], isPrime n]

newtype KPList a = KPList { unKPList :: [a] }
  deriving (Functor, Eq)

instance (Show a) => Show (KPList a) where
  show (KPList xs) = show xs

instance Monad KPList where
  return x = KPList [x]
  join (KPList xss) = KPList $ if
    | trivial xss'                -> concat xss'
    | any null xss'               -> []
    | isPrime (length xss')       -> []
    | any (isPrime . length) xss' -> []
    | otherwise                   -> concat xss'
   where
    xss' = map unKPList xss

instance (Arbitrary a) => Arbitrary (KPList a) where
    arbitrary = KPList <$> arbitrary
    -- arbitrary = KPList <$> suchThat arbitrary (isPrime . length)

prop_kplist1 :: KPList Int -> Bool
prop_kplist1 m = (join . fmap return $ m) == m
prop_kplist2 :: KPList Int -> Bool
prop_kplist2 m = (join . return $ m) == m
prop_kplist3 :: KPList (KPList (KPList Int)) -> Bool
prop_kplist3 m = (join . fmap join $ m) == (join . join $ m)

-- Postaw ptaszka:
-- [🐦] Monada
-- [  ] Nie monada
--
-- Jeśli nie monada, to wpisz kontrprzykład:


-----------------------
-- Palindromad monad(?)
-----------------------

palindromize :: [a] -> [a]
palindromize xs = xs ++ reverse (init xs)

newtype Palindromad a = Palindromad { unPalindromad :: [a] }
  deriving (Functor, Eq)

instance (Show a) => Show (Palindromad a) where
  show (Palindromad xs) = show xs

instance Monad Palindromad where
  return x = Palindromad [x]
  join (Palindromad xss) = Palindromad $ if
    | null xss      -> []
    | any null xss' -> []
    | otherwise     -> (init xss' >>= palindromize) ++ last xss'
   where
    xss' = map unPalindromad xss

instance (Arbitrary a) => Arbitrary (Palindromad a) where
    arbitrary = Palindromad . palindromize <$> (listOf1 arbitrary)
    -- arbitrary = Palindromad <$> (listOf1 arbitrary)

prop_palindromad1 :: Palindromad Int -> Bool
prop_palindromad1 m = (join . fmap return $ m) == m
prop_palindromad2 :: Palindromad Int -> Bool
prop_palindromad2 m = (join . return $ m) == m
prop_palindromad3 :: Palindromad (Palindromad (Palindromad Int)) -> Bool
prop_palindromad3 m = (join . fmap join $ m) == (join . join $ m)

-- Postaw ptaszka:
-- [🐦] Monada
-- [  ] Nie monada
--
-- Jeśli nie monada, to wpisz kontrprzykład: 


-----------------------------
-- Short Palindromad monad(?)
-----------------------------

newtype SPalindromad a = SPalindromad { unSPalindromad :: [a] }
  deriving (Functor, Eq)

instance (Show a) => Show (SPalindromad a) where
  show (SPalindromad xs) = show xs

instance Monad SPalindromad where
  return x = SPalindromad [x]
  join (SPalindromad xss) = SPalindromad $ take 100 $ if
    | trivial xss'  -> concat xss'
    | null xss      -> []
    | any null xss' -> []
    | otherwise     -> (init xss' >>= palindromize) ++ last xss'
   where
    xss' = map unSPalindromad xss

instance (Arbitrary a) => Arbitrary (SPalindromad a) where
    arbitrary = SPalindromad . palindromize <$> (listOf1 arbitrary)
    -- arbitrary = SPalindromad <$> (listOf1 arbitrary)

prop_spalindromad1 :: SPalindromad Int -> Bool
prop_spalindromad1 m = (join . fmap return $ m) == m
prop_spalindromad2 :: SPalindromad Int -> Bool
prop_spalindromad2 m = (join . return $ m) == m
prop_spalindromad3 :: SPalindromad (SPalindromad (SPalindromad Int)) -> Bool
prop_spalindromad3 m = (join . fmap join $ m) == (join . join $ m)

-- Postaw ptaszka:
-- [  ] Monada
-- [🐦] Nie monada
--
-- Jeśli nie monada, to wpisz kontrprzykład:
-- SPalindromad [43,35,-53,6,-12,-9,10,-36,-47,1,21,6,-1,53,-45,21,-18,-3,50,21,43,13,45,-15,12,-50,18,-55,-23,-47,30,-7,-3,-41,0,-42,-40,-9,-1,48,41,-25,-37,55,7,39,2,-41,1,-54,43,35,43,-54,1,-41,2,39,7,55,-37,-25,41,48,-1,-9,-40,-42,0,-41,-3,-7,30,-47,-23,-55,18,-50,12,-15,45,13,43,21,50,-3,-18,21,-45,53,-1,6,21,1,-47,-36,10,-9,-12,6,-53,35,43]

Control.Monad.return []
main = $quickCheckAll
