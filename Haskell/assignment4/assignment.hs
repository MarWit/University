{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ViewPatterns #-}

import Data.List
import Control.Arrow ((&&&))
import Control.Monad (join)

main :: IO ()
main = undefined

data BTree a = BNode (BTree a) a (BTree a) | BLeaf

-- Zadanie 1

flatten' = aux []
    where aux acc (BNode l v BLeaf) = aux (v : acc) l
          aux acc (BNode l v r) = aux (v : aux acc r) l
          aux acc BLeaf = acc

-- insert' a BLeaf = BNode BLeaf a BLeaf
-- insert' a t@(BNode l v r)
--     | a < v = BNode (insert' a l) v r
--     | a > v = BNode l v (insert' a r)
--     | otherwise = t

-- test_tree = foldl (flip insert') BLeaf [2,1,7,5,4,0,6,12,5]

qsort' :: Ord a => [a] -> [a]
qsort' = aux []
    where aux acc [] = acc
          aux acc (x:xs) = aux (x : aux acc [y | y <- xs, y >= x]) [y | y <- xs, y < x]

-- Zadanie 2

queens :: Int -> [[Int]]
queens n = aux n
    where aux 0 = [[]]
          aux k = [x : xs | xs <- aux (k-1), x <- [1..n], check x xs]
          check x xs = not . (||) (x `elem` xs) $ any (uncurry (==) . (abs . (x-) . fst &&& snd)) $ zip xs [1..]

-- Zadanie 3
-- TODO

-- Zadanie 4
-- TODO

-- Zadanie 5

class Queue q where
    emptyQ :: q a
    isEmptyQ :: q a -> Bool
    put :: a -> q a -> q a
    get :: q a -> (a, q a)
    get q = (top q, pop q)
    top :: q a -> a
    top = fst . get
    pop :: q a -> q a
    pop = snd . get

data SimpleQueue a = SimpleQueue { front :: [a], rear :: [a] }

instance Queue SimpleQueue where
    emptyQ = SimpleQueue [] []
    isEmptyQ (SimpleQueue f b) = null f && null b

    put e (SimpleQueue [] []) = SimpleQueue [e] []
    put e (SimpleQueue f b) = SimpleQueue f (e:b)

    get (SimpleQueue [] []) = error "queue is empty"
    get (SimpleQueue [e] b) = (e, SimpleQueue (reverse b) [])
    get (SimpleQueue (e:f) b) = (e, SimpleQueue f b)
    get _ = error "queue is empty"

-- Zadanie 6

primes :: [Integer]
primes = [ p | p <- [2..], p == 2 || and [ p `mod` q /= 0 | q <- takeWhile ((<=p) . join (*)) primes, q /= p ]]

-- Zadanie 7

fib :: [Integer]
fib = 0 : 1 : [a + b | a <- fib | b <- tail fib]

-- Zadanie 8

(<+>) :: Ord a => [a] -> [a] -> [a]
xl@(x:xs) <+> yl@(y:ys)
    | x < y = x : xs <+> yl
    | x > y = y : xl <+> ys
    | otherwise = xl <+> ys

h235 = 1 : map (*2) h235 <+> map (*3) h235 <+> map (*5) h235

-- Zadanie 9

tree = aux 1 where
    aux n = BNode (aux $ 2 * n) n (aux $ 2 * n + 1)

-- Zadanie 10

data RoseTree a = RNode a [RoseTree a]

btree = BNode btree 1 btree
rtree = RNode 1 $ repeat rtree

-- Zadanie 11

showFragList :: Show a => Int -> [a] -> String
showFragList n = ('[':) . (++"\x2026]") . concatMap ((++",") . show) . take n

data BTView t a = Node (t a) a (t a) | Leaf

class BT t where
    toTree :: t a -> BTView t a

instance BT BTree where
    toTree BLeaf = Leaf
    toTree (BNode l v r) = Node l v r

showFragTree :: (BT t, Show a) => Int -> t a -> String
showFragTree n = aux "" True False n where
    aux acc root tail depth (toTree -> Leaf) = ""
    aux acc root tail depth (toTree -> Node l v r) =
        or (depth==0) "" (aux (acc ++ or (tail && not root) line " ") False False (depth - 1) r) ++
        acc ++ or root middle (or tail bottom top) ++ or (depth==0) elipsis (show v) ++ "\n" ++
        or (depth==0) "" (aux (acc ++ or (tail || root) " " line) False True (depth - 1) l)
        where or True a _ = a
              or False _ b = b
              top = "\x250c"
              bottom = "\x2514"
              middle = "\x2500"
              line = "\x2502"
              elipsis = "\x2026"

showFragRose :: Show a => Int -> RoseTree a -> String
showFragRose n = aux 0
    where aux depth (RNode v l)
                | depth == n = replicate depth line ++ [bottom] ++ show v ++ " " ++ showFragList n (map (\(RNode v _) -> v) l) ++ "\n"
                | otherwise = replicate depth line ++ [bottom] ++ show v ++ "\n" ++ concatMap (aux (depth + 1)) (take n l)
          bottom = '\x2514'
          middle = '\x2500'
          line = '\x2502'

-- Zadanie 12

data Cyclist a = Elem (Cyclist a) a (Cyclist a)

label :: Cyclist a -> a
label (Elem _ v _) = v

forward :: Cyclist a -> Cyclist a
forward (Elem _ _ r) = r

backward :: Cyclist a -> Cyclist a
backward (Elem l _ _) = l

fromList :: [a] -> Cyclist a
fromList [x] = let c = Elem c x c in c
fromList (x:xs) = first
    where first = Elem last x next
          (last, next) = build xs first
          build (x:xs) prev = (last, Elem prev x next)
              where now = Elem prev x next
                    (last, next) = build xs now
          build [] prev = (prev, first)

-- Zadanie 13

enumList :: Cyclist Integer
enumList = first where
    first = Elem (build False first) 0 (build True first)
    build inc e@(Elem _ v _)
        | inc = let now = Elem e (v+1) (build inc now) in now
        | otherwise = let now = Elem (build inc now) (v-1) e in now

-- Zadanie 14
-- TODO
