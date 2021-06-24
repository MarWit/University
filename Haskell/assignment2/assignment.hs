{-# LANGUAGE ParallelListComp #-}

import Data.List
import Control.Arrow

main :: IO ()
main = undefined

-- Zadanie 1

zipC :: [a] -> [b] -> [(a, b)]
zipC a b = [(x,y) | x <- a | y <- b]

subseqC :: [a] -> [[a]]
subseqC [x] = [[x], []]
subseqC (x:xs) = concat $ [[ys, x:ys] | ys <- subseqC xs]

ipermC :: [a] -> [[a]]
ipermC [] = [[]]
ipermC (x:xs) = concat [insert ys | ys <- ipermC xs]
    where insert [] = [[x]]
          insert ys'@(y:ys) = (x:ys') : [y : zs | zs <- insert ys]

spermC :: [a] -> [[a]]
spermC [] = [[]]
spermC l = [x : ys |
   (i, x) <- zip [0 ..] l,
   ys <- spermC [z | (j, z) <- zip [0 .. ] l, i /= j]]

qsortC :: Ord a => [a] -> [a]
qsortC [] = []
qsortC (x:xs) = qsortC [e | e <- xs, e <= x] ++ [x] ++ qsortC [e | e <- xs, e > x]

-- Zadanie 3

data Combinator = S | K | Combinator :$ Combinator
infixl :$

instance Show Combinator where
    show S = "S"
    show K = "K"
    show (a :$ b@(_ :$ _)) = show a ++ "(" ++ show b ++ ")"
    show (a :$ b) = show a ++ show b

-- Zadanie 4

eval :: Combinator -> Combinator
eval (K :$ a) = K :$ eval a
eval (S :$ a) = S :$ eval a
eval (S :$ a :$ b) = S :$ eval a :$ eval b
eval (K :$ a :$ _) = eval a
eval (S :$ a :$ b :$ c) = eval $ a :$ c :$ (b :$ c)
eval (a :$ b) = eval $ eval a :$ b
eval a = a

-- Zadanie 5

data BST a = NodeBST (BST a) a (BST a) | EmptyBST deriving Show

searchBST :: Ord a => a -> BST a -> Maybe a
searchBST _ EmptyBST = Nothing
searchBST what (NodeBST l v r)
    | what == v = Just v
    | what < v = searchBST what l
    | what > v = searchBST what r

insertBST :: Ord a => a -> BST a -> BST a
insertBST e EmptyBST = NodeBST EmptyBST e EmptyBST
insertBST e t@(NodeBST l v r)
  | e == v = t
  | e < v = NodeBST (insertBST e l) v r
  | e > v = NodeBST l v (insertBST e r)

-- Zadanie 6

findMinBST :: Ord a => BST a -> a
findMinBST EmptyBST = error "Tree is empty"
findMinBST (NodeBST EmptyBST v _) = v
findMinBST (NodeBST l _ _) = findMinBST l

deleteMaxBST :: Ord a => BST a -> (BST a, a)
deleteMaxBST EmptyBST = error "Tree is empty"
deleteMaxBST (NodeBST r v EmptyBST) = (r, v)
deleteMaxBST (NodeBST r v l) = (NodeBST r v nl, max)
    where (nl, max) = deleteMaxBST l

deleteBST :: Ord a => a -> BST a -> BST a
deleteBST _ EmptyBST = EmptyBST
deleteBST what (NodeBST l v r)
    | what > v = NodeBST l v (deleteBST what r)
    | what < v = NodeBST (deleteBST what l) v r
deleteBST _ (NodeBST l _ EmptyBST) = l
deleteBST _ (NodeBST EmptyBST _ r) = r
deleteBST _ (NodeBST l _ r) = NodeBST l min nr
    where min = findMinBST r
          nr = deleteBST min r

-- Zadanie 7

data Tree23 a = Node2 (Tree23 a) a (Tree23 a) |
                Node3 (Tree23 a) a (Tree23 a) a (Tree23 a) |
                Empty23

search23 :: Ord a => a -> Tree23 a -> Maybe a
search23 _ Empty23 = Nothing
search23 what (Node2 l v r)
    | what == v = Just v
    | what < v = search23 what l
    | what > v = search23 what r
search23 what (Node3 l a m b r)
    | what == a = Just a
    | what == b = Just b
    | what < a = search23 what l
    | what > a && what < b = search23 what m
    | what > b = search23 what r

-- Zadanie 8

data InsResult a = BalancedIns (Tree23 a) | Grown (Tree23 a) a (Tree23 a)

insert23 e t =
    case ins e t of
        BalancedIns t -> t
        Grown l v r -> Node2 l v r
    where ins :: Ord a => a -> Tree23 a -> InsResult a
          ins e t@(Node2 Empty23 v Empty23)
            | e == v = BalancedIns t
            | e < v = BalancedIns $ Node3 Empty23 e Empty23 v Empty23
            | e > v = BalancedIns $ Node3 Empty23 v Empty23 e Empty23
          ins e t@(Node3 Empty23 a Empty23 b Empty23)
            | e == a || e == b = BalancedIns t
            | e < a = Grown (s e) a (s b)
            | e > a &&
              e < b = Grown (s a) e (s b)
            | e > b = Grown (s a) b (s e)
          ins e t@(Node2 l v r)
            | e == v = BalancedIns t
            | e < v = BalancedIns $ case ins e l of
                        BalancedIns l' -> Node2 l' v r
                        Grown ll lv lr -> Node3 ll lv lr v r
            | e > v = BalancedIns $ case ins e r of
                        BalancedIns r' -> Node2 l v r'
                        Grown rl rv rr -> Node3 l v rl rv rr
          ins e t@(Node3 l a m b r)
            | e == a || e == b = BalancedIns t
            | e < a =  case ins e l of
                        BalancedIns l' -> BalancedIns $ Node3 l' a m b r
                        Grown ll lv lr -> Grown (Node2 ll lv lr) a (Node2 m b r)
            | e > a &&
              e < b = case ins e m of
                        BalancedIns m' -> BalancedIns $ Node3 l a m' b r
                        Grown ml mv mr -> Grown (Node2 l a ml) mv (Node2 mr b r)
            | e > b = case ins e l of
                        BalancedIns r' -> BalancedIns $ Node3 l a m b r'
                        Grown rl rv rr -> Grown (Node2 l a m) b (Node2 rl rv rr)
          s :: Ord a => a -> Tree23 a
          s e = Node2 Empty23 e Empty23

-- Zadanie 10

data Tree234 a = N2 (Tree234 a) a (Tree234 a)
               | N3 (Tree234 a) a (Tree234 a) a (Tree234 a)
               | N4 (Tree234 a) a (Tree234 a) a (Tree234 a) a (Tree234 a)
               | Empty234

data RBTree a = Black (RBTree a) a (RBTree a)
              | Red (RBTree a) a (RBTree a)
              | RBEmpty

from234 :: Tree234 a -> RBTree a
from234 Empty234 = RBEmpty
from234 (N2 t1 a t2) = Black (from234 t1) a (from234 t2)
from234 (N3 t1 a t2 b t3) = Black (Red (from234 t1) a (from234 t2)) b (from234 t3)
from234 (N4 t1 a t2 b t3 c t4) = Black r1 b r2
    where r1 = Red (from234 t1) a (from234 t2)
          r2 = Red (from234 t3) c (from234 t4)

to234 :: RBTree a -> Tree234 a
to234 RBEmpty = Empty234
to234 (Black (Red l1 v1 r1) v (Red l2 v2 r2)) = N4 (to234 l1) v1 (to234 r1) v (to234 l2) v2 (to234 r2)
to234 (Black (Red l v' r) v rt) = N3 (to234 l) v' (to234 r) v (to234 rt)
to234 (Black lt v (Red l v' r) ) = N3 (to234 lt) v (to234 l) v' (to234 r)
to234 (Black l v r) = N2 (to234 l) v (to234 r)
