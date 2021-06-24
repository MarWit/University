-- Kurs języka Haskell
-- Lista 11
-- Marcin Witkowski
-- 2020-06-19

{-# LANGUAGE
   ExistentialQuantification,
   RankNTypes,
   PolyKinds,
   TypeOperators,
   MultiParamTypeClasses,
   FunctionalDependencies,
   DeriveFunctor,
   FlexibleInstances,
   QuantifiedConstraints
#-}

module Lib where

import Control.Monad (ap, MonadPlus, mzero, mplus, (>=>))
import Control.Applicative (Alternative, empty, (<|>))
import Data.Semigroup

-- Zadanie 1

data Yoneda f a = Yoneda (forall x. (a -> x) -> f x)

toYoneda :: (Functor f) => f a -> Yoneda f a
toYoneda f = Yoneda $ flip fmap f

fromYoneda :: Yoneda f a -> f a
fromYoneda (Yoneda y) = y id

instance Functor (Yoneda f) where
    fmap f (Yoneda y) = Yoneda $ \g -> y (g . f)

-- Zadanie 2

newtype DList a = DList { fromDList :: [a] -> [a] }

repDList :: DList a -> Yoneda DList a
repDList (DList d) = Yoneda $ \y -> DList $ (++) (map y (d []))

repList :: [a] -> Yoneda DList a
repList l = repDList $ DList (l++)

instance Semigroup (DList a) where
    (DList f) <> (DList g) = DList $ f . g

instance Semigroup (Yoneda DList a) where
    (Yoneda a) <> (Yoneda b) = Yoneda $ \y -> (a <> b) y

-- Nie wydaje się być to problem, jako że to dodawanie dzieje się od prawej strony,
-- zatem nie powinniśmy tracić żadnych fajnych własności list różnicowych.

-- Zadanie 3

instance {-# OVERLAPPABLE #-} (forall x. Semigroup (f x)) => Semigroup (Yoneda f a) where
    (Yoneda a) <> (Yoneda b) = Yoneda $ \y -> (a <> b) y

-- Zadanie 4

newtype Cod f a = Cod { runCod :: forall x. (a -> f x) -> f x }

instance Functor (Cod f) where
    fmap f (Cod m) = Cod $ \k -> m (\x -> k (f x))

instance Applicative (Cod f) where
  pure  = return
  (<*>) = ap

instance Monad (Cod f) where
  return x = Cod $ \k -> k x
  m >>= k  = Cod $ \c -> runCod m (\a -> runCod (k a) c)

fromCod :: (Monad m) => Cod m a -> m a
fromCod = flip runCod return

toCod :: (Monad m) => m a -> Cod m a
toCod m = Cod (m >>=)

instance (Alternative v) => Alternative (Cod v) where
    empty = Cod $ const empty
    (Cod m) <|> (Cod n) = Cod $ \k -> m k <|> n k

instance MonadPlus m => MonadPlus (Cod m) where
    mzero = Cod $ const mzero
    (Cod m) `mplus` (Cod n) = Cod $ \k -> m k `mplus` n k

-- Testowałem z listami pod monadą kogęstościową, konkatenując listy o wielomianowej długości
-- względem numeru testu i wyszło mi, że konkatenacja działa z grubsza w czasie liniowym.

-- Zadanie 5

class Category (t :: k -> k -> *) where
    ident :: a `t` a
    comp :: b `t` c -> a `t` b -> a `t` c

newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance (Monad m) => Category (Kleisli m) where
    ident = Kleisli $ return
    (Kleisli m) `comp` (Kleisli n) = Kleisli $ n >=> m
    -- (Kleisli m) `comp` (Kleisli n) = Kleisli $ \x -> n x >>= m
