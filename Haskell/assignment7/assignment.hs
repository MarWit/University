{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, Rank2Types, KindSignatures #-}

import Data.List
import Control.Monad
import Data.Monoid
import Control.Arrow ((&&&))

main :: IO ()
main = undefined

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
    toList :: t a -> [a]
    insert = merge . single
    single = flip Main.insert empty
    extractMin = findMin &&& deleteMin
    findMin = fst . extractMin
    deleteMin = snd . extractMin
    fromList = foldr Main.insert empty
    toList = unfoldr (\t -> if isEmpty t then Nothing else Just (extractMin t))

-- Zadanie 1

data PEmpty a = PEmpty
data PFork t a = PFork (t a) a (t a)
data Pennant t a = Pennant a (t a)
data PList t a = PNil
               | PZero (PList (PFork t) a)
               | POne (Pennant t a) (PList (PFork t) a)
newtype PHeap a = PHeap (PList PEmpty a)

instance Ord a => Prioq PHeap a where
    empty = PHeap PNil

    isEmpty (PHeap PNil) = True
    isEmpty _ = False

    single a = PHeap $ POne (Pennant a PEmpty) PNil

    merge (PHeap a) (PHeap b) = PHeap $ traverse a b where
        traverse :: Ord a => PList t a -> PList t a -> PList t a
        traverse PNil t = t
        traverse t PNil = t
        traverse (PZero ta) (PZero tb) = PZero $ traverse ta tb
        traverse (PZero ta) (POne tree tb) = POne tree $ traverse ta tb
        traverse (POne tree ta) (PZero tb) = POne tree $ traverse ta tb
        traverse (POne t1 ta) (POne t2 tb) = PZero $ traverse (tree_merge t1 t2) $ traverse ta tb
        tree_merge :: Ord a => Pennant t a -> Pennant t a -> PList (PFork t) a
        tree_merge (Pennant a ta) (Pennant b tb)
            | a <= b = POne (Pennant a (PFork ta b tb)) PNil
            | a > b = POne (Pennant b (PFork ta a tb)) PNil

    findMin (PHeap h) = aux Nothing h
        where aux :: Ord a => Maybe a -> PList t a -> a
              aux Nothing PNil = error "Heap is empty!"
              aux (Just e) PNil = e
              aux Nothing (POne (Pennant v _) tail) = aux (Just v) tail
              aux (Just e) (POne (Pennant v _) tail) = aux (Just $ min e v) tail
              aux acc (PZero tail) = aux acc tail

    deleteMin p@(PHeap h) = foldr Main.insert (PHeap nh) values
        where min = findMin p
              (nh, values) = delete (const []) min h
              delete :: Ord a => (t a -> [a]) -> a -> PList t a -> (PList t a, [a])
              delete _ _ PNil = (PNil, [])
              delete f search (PZero tail) = (PZero . fst &&& snd) $ delete (unfork f) search tail
              delete f search (POne p@(Pennant v tree) tail)
                | search == v = case tail of
                    PNil -> (PNil, f tree)
                    _    -> (PZero tail, f tree)
                | otherwise = (POne p . fst &&& snd) $ delete (unfork f) search tail
              unfork :: (t a -> [a]) -> PFork t a -> [a]
              unfork flatten (PFork l v r) = flatten l ++ v : flatten r

-- Zadanie 2

data BEmpty a = BEmpty
data BCons l a = BCons (BFork l a) (l a)
data BFork l a = BFork a (l a)
data BList l a = BNil
               | BZero (BList (BCons l) a)
               | BOne (BFork l a) (BList (BCons l) a)
newtype BHeap a = BHeap (BList BEmpty a)

instance Ord a => Prioq BHeap a where
    empty = BHeap BNil

    isEmpty (BHeap BNil) = True
    isEmpty _ = False

    single a = BHeap $ BOne (BFork a BEmpty) BNil

    merge (BHeap a) (BHeap b) = BHeap $ traverse a b where
        traverse :: Ord a => BList t a -> BList t a -> BList t a
        traverse BNil t = t
        traverse t BNil = t
        traverse (BZero ta) (BZero tb) = BZero $ traverse ta tb
        traverse (BZero ta) (BOne tree tb) = BOne tree $ traverse ta tb
        traverse (BOne tree ta) (BZero tb) = BOne tree $ traverse ta tb
        traverse (BOne t1 ta) (BOne t2 tb) = BZero $ traverse (tree_merge t1 t2) $ traverse ta tb
        tree_merge :: Ord a => BFork l a -> BFork l a -> BList (BCons l) a
        tree_merge (BFork a ta) (BFork b tb)
            | a <= b = BOne (BFork a (BCons (BFork b tb) ta)) BNil
            | otherwise = BOne (BFork b (BCons (BFork a ta) tb)) BNil

    findMin (BHeap h) = aux Nothing h
        where aux :: Ord a => Maybe a -> BList l a -> a
              aux Nothing BNil = error "Heap is empty!"
              aux (Just e) BNil = e
              aux Nothing (BOne (BFork v _) tail) = aux (Just v) tail
              aux (Just e) (BOne (BFork v _) tail) = aux (Just $ min e v) tail
              aux acc (BZero tail) = aux acc tail

    deleteMin b@(BHeap h) = foldr Main.insert (BHeap nh) values
        where min = findMin b
              (nh, values) = delete (const []) min h
              delete :: Ord a => (l a -> [a]) -> a -> BList l a -> (BList l a, [a])
              delete _ _ BNil = (BNil, [])
              delete f search (BZero tail) = (BZero . fst &&& snd) $ delete (unfork f) search tail
              delete f search (BOne p@(BFork v tree) tail)
                | search == v = case tail of
                    BNil -> (BNil, f tree)
                    _    -> (BZero tail, f tree)
                | otherwise = (BOne p . fst &&& snd) $ delete (unfork f) search tail
              unfork :: (l a -> [a]) -> BCons l a -> [a]
              unfork flatten (BCons (BFork v l) r) = v : flatten l ++ flatten r

-- Zadanie 3

data Expr a where
    C :: a -> Expr a
    P :: (Expr a, Expr b) -> Expr (a,b)
    Not :: Expr Bool -> Expr Bool
    (:+), (:-), (:*) :: Expr Integer -> Expr Integer -> Expr Integer
    (:/) :: Expr Integer -> Expr Integer -> Expr (Integer,Integer)
    (:<), (:>), (:<=), (:>=), (:!=), (:==)
        :: Expr Integer -> Expr Integer -> Expr Bool
    (:&&), (:||) :: Expr Bool -> Expr Bool -> Expr Bool
    (:?) :: Expr Bool -> Expr a -> Expr a -> Expr a
    Fst :: Expr (a,b) -> Expr a
    Snd :: Expr (a,b) -> Expr b

infixl 6 :*, :/
infixl 5 :+, :-
infixl 4 :<, :>, :<=, :>=, :!=, :==
infixl 3 :&&
infixl 2 :||
infixl 1 :?

eval :: Expr a -> a
eval (C a) = a
eval (P (a, b)) = (eval a, eval b)
eval (Not v) = not . eval $ v
eval (a :+ b) = eval a + eval b
eval (a :- b) = eval a - eval b
eval (a :* b) = eval a * eval b
eval (a :/ b) = (x, y)
    where (x, y) = eval a `divMod` eval b
eval (a :< b) = eval a < eval b
eval (a :> b) = eval a > eval b
eval (a :<= b) = eval a <= eval b
eval (a :>= b) = eval a >= eval b
eval (a :!= b) = eval a /= eval b
eval (a :== b) = eval a == eval b
eval (a :&& b) = eval a && eval b
eval (a :|| b) = eval a || eval b
eval ((:?) b x y) = if eval b then eval x else eval y
eval (Fst v) = fst . eval $ v
eval (Snd v) = snd . eval $ v

-- Zadanie 4

data Zero :: *
data Succ :: * -> *
data Red :: *
data Black :: *

data Tree :: * -> * -> * -> * where
    Empty :: Tree Black Zero a
    Black :: Tree c1 h a -> a -> Tree c2 h a -> Tree Black (Succ h) a
    Red   :: Tree Black h a -> a -> Tree Black h a -> Tree Red h a

data RedBlackTree :: * -> * where
    RedBlackTree :: Tree Black h a -> RedBlackTree a

rbfind :: Ord a => a -> RedBlackTree a -> Bool
rbfind e (RedBlackTree tree) = aux e tree where
    aux :: Ord a => a -> Tree c h a -> Bool
    aux _ Empty = False
    aux e (Black l v r)
        | e == v = True
        | e > v = aux e r
        | e < v = aux e l
    aux e (Red l v r)
        | e == v = True
        | e > v = aux e r
        | e < v = aux e l

rbinsert :: Ord a => a -> RedBlackTree a -> RedBlackTree a
rbinsert e (RedBlackTree tree) = undefined

rbdelete :: Ord a => a -> RedBlackTree a -> RedBlackTree a
rbdelete = undefined

rbflatten :: RedBlackTree a -> [a]
rbflatten (RedBlackTree tree) = aux tree where
    aux :: Tree c h a -> [a]
    aux Empty = []
    aux (Black l v r) = aux l ++ v : aux r
    aux (Red l v r) = aux l ++ v : aux r

-- Zadanie 6

newtype Church = Church (forall a. (a -> a) -> (a -> a))

zero :: Church
zero = Church $ \f x -> x

one :: Church
one = Church $ \f x -> f x

isZero :: Church -> Bool
isZero (Church n) = n (const False) True

instance Show Church where
    show = show . fromEnum

instance Eq Church where
    n == m
        | isZero n = isZero m
        | otherwise = pred n == pred m

instance Ord Church where
    n <= m = isZero $ n - m

instance Enum Church where
    succ (Church n) = Church $ \f -> f . n f
    pred (Church n) = snd $ n (succ . fst &&& fst) (zero, zero)
    toEnum = (iterate succ zero !!)
    fromEnum (Church n) = n succ 0

instance Num Church where
    abs = id
    signum _ = 1
    fromInteger = aux
        where aux 0 = zero
              aux n = succ . aux $ (n-1)
    a + b
      | isZero a = b
      | otherwise = pred a + succ b

    a * b = aux a b
        where aux n acc
                | isZero n = acc
                | otherwise = aux (pred n) (acc + a)

    a - b
        | isZero a || isZero b = a
        | otherwise = pred a - pred b

-- Zadanie 7

newtype CList x = CList (forall a. (x -> a -> a) -> a -> a)

cempty :: CList x
cempty = CList $ \_ t -> t

ccons :: x -> CList x -> CList x
ccons v (CList f) = CList $ \h t -> h v (f h t)

cappend :: CList x -> CList x -> CList x
cappend (CList f) (CList g) = CList $ \h t -> f h (g h t)

cfromList :: [x] -> CList x
cfromList = foldr ccons cempty

ctoList :: CList x -> [x]
ctoList (CList f) = f (:) []

-- Zadanie 8

newtype MList x = MList (forall m. Monoid m => (x -> m) -> m)

mlempty :: MList x
mlempty = MList $ const mempty

mlcons :: x -> MList x -> MList x
mlcons v (MList f) = MList $ \m -> m v <> f m

mlappend :: MList x -> MList x -> MList x
mlappend (MList f) (MList g) = MList $ \m -> f m <> g m

mlfromList :: [x] -> MList x
mlfromList = foldr mlcons mlempty

mltoList :: MList x -> [x]
mltoList (MList f) = f (:[])
