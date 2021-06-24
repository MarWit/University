{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}

import Data.List (transpose)

main :: IO ()
main = undefined

data BTree t a = Node (t a) a (t a) | Leaf

class BT t where
    toTree :: t a -> BTree t a

data UTree a = UNode (UTree a) a (UTree a) | ULeaf

-- Zadanie 1

treeSize :: BT t => t a -> Int
treeSize (toTree -> Leaf) = 0
treeSize (toTree -> Node l _ r) = 1 + max (treeSize l) (treeSize r)

treeLabels :: BT t => t a -> [a]
treeLabels = flip aux []
    where aux (toTree -> Leaf) acc = acc
          aux (toTree -> Node l v r) acc = aux l (v : aux r acc)

treeFold :: BT t => (b -> a -> b -> b) -> b -> t a -> b
treeFold _ e (toTree -> Leaf) = e
treeFold f e (toTree -> Node l v r) = f (treeFold f e l) v (treeFold f e r)

instance BT UTree where
    toTree ULeaf = Leaf
    toTree (UNode l v r) = Node l v r

newtype Unbalanced a = Unbalanced { fromUnbalanced :: BTree Unbalanced a }
instance BT Unbalanced where
    toTree = fromUnbalanced

-- Zadanie 2

searchBT :: (Ord a, BT t) => a -> t a -> Maybe a
searchBT _ (toTree -> Leaf) = Nothing
searchBT e (toTree -> Node l v r)
    | e == v = Just v
    | e < v = searchBT e l
    | e > v = searchBT e r

toUTree :: BT t => t a -> UTree a
toUTree (toTree -> Leaf) = ULeaf
toUTree (toTree -> Node l v r) = UNode (toUTree l) v (toUTree r)

toUnbalanced :: BT t => t a -> Unbalanced a
toUnbalanced (toTree -> Leaf) = Unbalanced Leaf
toUnbalanced (toTree -> Node l v r) = Unbalanced $ Node (toUnbalanced l) v (toUnbalanced r)

-- Zadanie 3

instance (BT t, Show a) => Show (t a) where
    show (toTree -> Leaf) = ""
    show (toTree -> Node l v r) =
        case (tl, tr) of
            (Leaf, Leaf) -> "- " ++ show v ++ " -"
            (Leaf, _) -> "- " ++ show v ++ " (" ++ show r ++ ")"
            (_, Leaf) -> "(" ++ show l ++ ") " ++ show v ++ " -"
            (_, _) -> "(" ++ show l ++ ") " ++ show v ++ " (" ++ show r ++ ")"
        where tl = toTree l
              tr = toTree r

-- Zadanie 4

treeBoxDrawing :: (BT t, Show a) => t a -> String
treeBoxDrawing = aux "" True False where
    aux acc root tail (toTree -> Leaf) = ""
    aux acc root tail (toTree -> Node l v r) =
        aux (acc ++ or (tail && not root) line " ") False False r ++
        acc ++ or root middle (or tail bottom top) ++ show v ++ "\n" ++
        aux (acc ++ or (tail || root) " " line) False True l
        where or True a _ = a
              or False _ b = b
              top = "\x250c"
              bottom = "\x2514"
              middle = "\x2500"
              line = "\x2502"


-- Zadanie 6

class BT t => BST t where
    node :: t a -> a -> t a -> t a
    leaf :: t a

instance BST UTree where
    node = UNode
    leaf = ULeaf

instance BST Unbalanced where
    node l v r = Unbalanced $ Node l v r
    leaf = Unbalanced Leaf

class Set s where
    empty :: s a
    search :: Ord a => a -> s a -> Maybe a
    insert :: Ord a => a -> s a -> s a
    delMax :: Ord a => s a -> Maybe (a, s a)
    delete :: Ord a => a -> s a -> s a

instance BST s => Set s where
    empty = leaf
    search = searchBT

    insert e (toTree -> Leaf) = node leaf e leaf
    insert e t@(toTree -> Node l v r)
        | e == v = t
        | e < v = node (insert e l) v r
        | e > v = node l v (insert e r)

    delMax (toTree -> Leaf) = Nothing
    delMax (toTree -> Node l v r)
        | Leaf <- toTree r = Just (v, l)
        | otherwise = Just (max, node l v nr)
            where Just (max, nr) = delMax r

    delete _ t@(toTree -> Leaf) = t
    delete e (toTree -> Node l v r)
        | e > v = node l v (delete e r)
        | e < v = node (delete e l) v r
        | Leaf <- toTree r = l
        | Leaf <- toTree l = r
        | otherwise = node l min nr
            where findMin (toTree -> Node l v r)
                    | Leaf <- toTree r = v
                    | otherwise = findMin l
                  min = findMin r
                  nr = delete min r

data WBTree a = WBNode (WBTree a) a Int (WBTree a) | WBLeaf

wbsize :: WBTree a -> Int
wbsize (WBNode _ _ n _) = n
wbsize WBLeaf = 0

treeRotateL (toTree -> Node l v (toTree -> Node rl rv rr)) =
    node (node l v rl) rv rr
treeRotateLL (toTree -> Node l v (toTree -> Node (toTree -> Node rll rlv rlr) rv rr)) =
    node (node l v rll) rlv (node rlr rv rr)
treeRotateR (toTree -> Node (toTree -> Node ll lv lr) v r) =
    node ll lv (node lr v r)
treeRotateRR (toTree -> Node (toTree -> Node ll lv (toTree -> Node lrl lrv lrr)) v r) =
    node (node ll lv lrl) lrv (node lrr v r)

-- treeRotateL (toTree -> Node x a (toTree -> Node y b z)) =
--     node (node x a y) b z
-- treeRotateLL (toTree -> Node x a (toTree -> Node (toTree -> Node y1 b y2) c z)) =
--     node (node x a y1) b (node y2 c z)
-- treeRotateR (toTree -> Node (toTree -> Node x a y) b z) =
--     node x a (node y b z)
-- treeRotateRR (toTree -> Node (toTree -> Node x a (toTree -> Node y1 b y2)) c z) =
--     node (node x a y1) b (node y2 c z)

instance BT WBTree where
    toTree WBLeaf = Leaf
    toTree (WBNode l v _ r) = Node l v r

instance BST WBTree where
    leaf = WBLeaf
    node l v r
        | ln + rn < 2 = n l v r
        | rn > 5 * ln =
            if rln < rrn then treeRotateL $ n l v r
            else treeRotateLL $ n l v r
        | ln > 5 * rn =
            if lrn < lln then treeRotateR $ n l v r
            else treeRotateRR $ n l v r
        | otherwise = n l v r
        where ln = wbsize l
              rn = wbsize r
              WBNode ll _ _ lr = l
              lrn = wbsize lr
              lln = wbsize ll
              WBNode rl _ _ rr = r
              rrn = wbsize rr
              rln = wbsize rl
              n l v r = WBNode l v (1 + wbsize l + wbsize r) r

-- Zadanie 8

data HBTree a = HBNode (HBTree a) a Int (HBTree a) | HBLeaf

hbheight :: HBTree a -> Int
hbheight HBLeaf = 0
hbheight (HBNode _ _ h _) = h

instance BT HBTree where
    toTree HBLeaf = Leaf
    toTree (HBNode l v _ r) = Node l v r

instance BST HBTree where
    leaf = HBLeaf
    node l v r
        | lh + rh < 2 = n l v r
        | rh > lh + 1 =
            if rlh < rrh then treeRotateL $ n l v r
            else treeRotateLL $ n l v r
        | lh > rh + 1 =
            if lrh < llh then treeRotateR $ n l v r
            else treeRotateRR $ n l v r
        | otherwise = n l v r
        where lh = hbheight l
              rh = hbheight r
              HBNode ll _ _ lr = l
              lrh = hbheight lr
              llh = hbheight ll
              HBNode rl _ _ rr = r
              rrh = hbheight rr
              rlh = hbheight rl
              n l v r = HBNode l v (1 + max (hbheight l) (hbheight r)) r

-- Zadanie 9

data RBColor = Red | Black
data RBTree a = RBNode RBColor (RBTree a) a (RBTree a) | RBLeaf

instance BT RBTree where
    toTree (RBNode _ l v r) = Node l v r
    toTree RBLeaf = Leaf

rbbalance :: Ord a => RBColor -> RBTree a -> a -> RBTree a -> RBTree a
rbbalance Black (RBNode Red (RBNode Red a x b) y c) z d =
    RBNode Red (RBNode Black a x b) y (RBNode Black c z d)
rbbalance Black (RBNode Red a x (RBNode Red b y c)) z d =
    RBNode Red (RBNode Black a x b) y (RBNode Black c z d)
rbbalance Black a x (RBNode Red (RBNode Red b y c) z d) =
    RBNode Red (RBNode Black a x b) y (RBNode Black c z d)
rbbalance Black a x (RBNode Red b y (RBNode Red c z d)) =
    RBNode Red (RBNode Black a x b) y (RBNode Black c z d)
rbbalance c a x b = RBNode c a x b

instance Set RBTree where
    -- empty :: s a
    empty = RBLeaf
    -- search :: Ord a => a -> s a -> Maybe a
    search = searchBT
    -- insert :: Ord a => a -> s a -> s a
    insert e t = makeBlack (ins t)
        where ins RBLeaf = RBNode Red RBLeaf e RBLeaf
              ins t@(RBNode c l v r)
                    | e < v = rbbalance c (ins l) v r
                    | e > v = rbbalance c l v (ins r)
                    | otherwise = t
              makeBlack (RBNode _ l v r) = RBNode Black l v r
    -- delete :: Ord a => a -> s a -> s a
    delete e t = undefined -- TODO
        where makeBlack (RBNode _ l v r) = RBNode Black l v r

    delMax RBLeaf = Nothing
    delMax t = Just (max, delete max t)
        where findMax (RBNode _ _ v RBLeaf) = v
              findMax (RBNode _ _ _ r) = findMax r
              max = findMax t
