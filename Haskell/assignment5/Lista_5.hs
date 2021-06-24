{-# LANGUAGE ViewPatterns #-}

{-|
Module: Lista_5
Description: Main module
Copyright: Marcin Witkowski
|-}
module Lista_5 where

import Control.Monad
import Data.Functor
import Control.Applicative
import Control.Arrow ((&&&))

-- Zadanie 1

class NFData a where
    rnf :: a -> ()
    rnf a = a `seq` ()

instance NFData Int
instance NFData Word
instance NFData Integer
instance NFData Float
instance NFData Double

instance NFData a => NFData [a] where
    rnf [] = ()
    rnf (x:xs) = rnf x `seq` rnf xs

instance (NFData a, NFData b) => NFData (a, b) where
    rnf (x, y) = rnf x `seq` rnf y

-- TODO: Wincyj tupli?

deepseq a b = rnf a `seq` b
f $!! x = x `deepseq` f x
infixr 0 $!!

-- Zadanie 2

subseqM :: MonadPlus m => [a] -> m [a]
subseqM = filterM (const (return True `mplus` return False))

ipermM :: MonadPlus m => [a] -> m [a]
ipermM [] = return []
ipermM (x:xs) = do ys <- ipermM xs
                   insert x ys
    where insert :: MonadPlus m => a -> [a] -> m [a]
          insert x [] = return [x] `mplus` mzero
          insert x yl@(y:ys) = return (x:yl) `mplus` do
                                zs <- insert x ys
                                return (y:zs)

spermM :: MonadPlus m => [a] -> m [a]
spermM [] = return []
spermM xs = do (y, ys) <- select xs
               zs <- spermM ys
               return $ y : zs
    where select :: MonadPlus m => [a] -> m (a, [a])
          select [y] = return (y, [])
          select (y:ys) = return (y, ys) `mplus` do
                            (z, zs) <- select ys
                            return (z, y:zs)


-- Zadanie 6

data List t a = Cons a (t a) | Nil
newtype SimpleList a = SimpleList { fromSimpleList :: List SimpleList a }

class ListView t where
    viewList :: t a -> List t a
    toList :: t a -> [a]
    toList (viewList -> Cons v l) = v : toList l
    toList _ = []
    cons :: a -> t a -> t a
    nil :: t a

data CList a = CList a :++: CList a | CSingle a | CNil

instance ListView CList where
    cons v t = CSingle v :++: t
    nil = CNil
    viewList CNil = Nil
    viewList (CSingle v) = Cons v CNil
    viewList (CSingle v :++: r) = Cons v r
    viewList (l :++: r) =
        case viewList l of
          Cons v ln -> Cons v (ln :++: r)
          Nil -> viewList r

instance Functor CList where
    fmap f CNil = CNil
    fmap f (CSingle v) = CSingle . f $ v
    fmap f (l :++: r) = fmap f l :++: fmap f r

instance Applicative CList where
    pure v = CSingle v
    (<*>) = ap
    -- CNil <*> _ = CNil
    -- CSingle f <*> l = f <$> l
    -- (fl :++: fr) <*> l = (fl <*> l) :++: (fr <*> l)

instance Monad CList where
    CNil >>= f = CNil
    CSingle v >>= f = f v
    (l :++: r) >>= f = (l >>= f) :++: (r >>= f)

instance MonadPlus CList
instance Alternative CList where
    empty = nil
    (<|>) = (:++:)

instance Foldable CList where
    foldr f a l = foldr f a (toList l)

instance Traversable CList where
    traverse f CNil = pure CNil
    traverse f (CSingle v) = CSingle <$> f v
    traverse f (l :++: r) = (<|>) <$> traverse f l <*> traverse f r

-- Zadanie 7

newtype DList a = DList { fromDList :: [a] -> [a] }

dappend :: DList a -> DList a -> DList a
dappend l r = DList $ fromDList l . fromDList r

instance ListView DList where
    nil = DList id
    cons v t = DList $ fromDList t . (v:)
    toList l = fromDList l []
    viewList (fromDList -> f) =
        case f [] of
          [] -> Nil
          (x:_) -> Cons x (DList $ tail . f)

instance Functor DList where
    fmap f = foldr (cons . f) nil

instance Applicative DList where
    pure = flip cons nil
    (<*>) = ap

instance Monad DList where
    l >>= f = foldr (dappend . f) nil l

instance MonadPlus DList
instance Alternative DList where
    empty = nil
    (<|>) = dappend

instance Foldable DList where
    foldr f a = foldr f a . toList

instance Traversable DList where
    traverse f = foldr aux (pure nil)
        where aux x = (<*>) (fmap cons (f x))

