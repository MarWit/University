-- Lista 10
-- Kurs języka Haskell
-- Marcin Witkowski
-- 2020-05-31

{-# LANGUAGE
   ScopedTypeVariables,
   ExistentialQuantification,
   DeriveFunctor
#-}

import Data.Maybe
import Data.Foldable (foldr')
import Data.Char
import Data.Function
import Data.List.Split
import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.List
import Data.STRef
import Generic.Random
import Test.QuickCheck

-- main :: IO ()
-- main = undefined

-- Zadanie 2

data UF s = UF {
    ids :: STUArray s Int Int,
    ranks :: STUArray s Int Int
}

ufCreate :: Int -> ST s (UF s)
ufCreate n = do
    ids <- newListArray (0, n-1) [0..n-1]
    ranks <- newArray (0, n-1) 0
    return $ UF ids ranks

ufFind :: UF s -> Int -> ST s Int
ufFind uf id = do
    (min, max) <- getBounds (ids uf)
    unless (id >= min && id <= max) $ error "UF: Invalid index"
    parent <- readArray (ids uf) id
    root <- if parent /= id then ufFind uf parent
            else return parent
    writeArray (ids uf) id root
    return root

ufUnion :: UF s -> Int -> Int -> ST s ()
ufUnion uf id1 id2 = do
    (min, max) <- getBounds (ids uf)
    unless (id1 >= min && id1 <= max) $ error "UF: Invalid first index"
    unless (id2 >= min && id2 <= max) $ error "UF: Invalid second index"
    root1 <- ufFind uf id1
    root2 <- ufFind uf id2
    unless (root1 /= root2) $ return ()
    rank1 <- readArray (ranks uf) root1
    rank2 <- readArray (ranks uf) root2
    when (rank1 == rank2) $ writeArray (ranks uf) root1 (rank1 + 1)
    if rank1 >= rank2
       then writeArray (ids uf) root2 root1
       else writeArray (ids uf) root1 root2

type Edge = (Int, Float, Int)
data Graph = Graph Int [Edge] deriving (Show, Eq)

graphCreate :: [Edge] -> Graph
graphCreate edges = Graph size newEdges
    where size = maximum . map (\(v, _ , w) -> max v w) $ edges
          newEdges = map (\e@(v, x, w) -> if v >= w then (w, x, v) else e) edges

kruskalSpanning :: Graph -> Graph
kruskalSpanning (Graph size edges) = graphCreate $ runST $ do
        uf <- ufCreate (size + 1)
        edgesMut <- newSTRef []
        forM_ sortedEdges $ \e@(v, x, w) -> do
            rv <- ufFind uf v
            rw <- ufFind uf w
            when (rv /= rw) $ ufUnion uf v w >> modifySTRef edgesMut (e:)
        readSTRef edgesMut
    where sortedEdges = sortBy (\(_, x, _) (_, y, _) -> x `compare` y) edges

data UFFun = UFFun [Int] [Int]

uffunCreate :: Int -> UFFun
uffunCreate n = UFFun [0..n-1] (replicate n 0)

actOnElement :: [a] -> Int -> (a -> a) -> [a]
actOnElement l n f = h ++ (f . head $ t) : tail t
    where (h, t) = splitAt n l

uffunFind :: UFFun -> Int -> (Int, UFFun)
uffunFind uf@(UFFun ids ranks) id
    | id < 0 || id >= len = error "UFFun: Invalid index"
    | otherwise = if parent == id then (id, uf)
                  else (root, UFFun (actOnElement newIds id (const root)) newRanks)
    where len = length ids
          parent = ids !! id
          (root, newUF) = uffunFind uf parent
          (UFFun newIds newRanks) = newUF

uffunUnion :: UFFun -> Int -> Int -> UFFun
uffunUnion uf@(UFFun ids _) id1 id2
    | id1 < 0 || id1 >= len = error "UFFun: Invalid first index"
    | id2 < 0 || id2 >= len = error "UFFun: Invalid second index"
    | otherwise = if root1 == root2 then uf2
                  else UFFun newIDs newRanks
    where len = length ids
          (root1, uf1) = uffunFind uf id1
          (root2, uf2) = uffunFind uf1 id2
          (UFFun ids' ranks') = uf2
          rank1 = ranks' !! id1
          rank2 = ranks' !! id2
          newRanks = if rank1 == rank2
                        then actOnElement ranks' id1 (+1)
                        else ranks'
          newIDs = if rank1 >= rank2
                      then actOnElement ids root2 (const root1)
                      else actOnElement ids root1 (const root2)

kruskalSpanningFun :: Graph -> Graph
kruskalSpanningFun (Graph size edges) = graphCreate newEdges
    where uf = uffunCreate (size + 1)
          sortedEdges = sortBy (\(_, x, _) (_, y, _) -> x `compare` y) edges
          (newEdges, _) = foldl (\(edges, uf) e@(v, x, w) ->
              let (rv, uf1) = uffunFind uf v in
              let (rw, uf2) = uffunFind uf1 w in
              if rv /= rw
                 then (e:edges, uffunUnion uf2 v w)
                 else (edges, uf2)) ([], uf) sortedEdges


randomEdges :: Gen [Edge]
randomEdges = nubBy (\(v, _, w) (v', _, w') -> v == v' && w == w') <$>
              infiniteListOf (
                suchThat
                    arbitrary
                    (\(v, x, w) -> v < w && v >= 0 && w >= 0 && x > 0))

randomEdgesN :: Gen [Edge]
randomEdgesN = sized $ \n -> do
    l <- choose (1, n * n + 1)
    take l <$> randomEdges

instance Arbitrary Graph where
    arbitrary = graphCreate <$> randomEdgesN

-- prop_kruskal :: Graph -> Bool
-- prop_kruskal g = kruskalSpanning g == kruskalSpanningFun g

-- main = quickCheck prop_kruskal
-- main = do
--     g <- generate arbitrary
--     putStr $ show . kruskalSpanning $ g

-- Zadanie 3

data Queue a = Queue { front :: [a], rear :: [a] } deriving Show

empty = Queue [] []
isEmpty (Queue f b) = null f && null b

pushBack e (Queue [] []) = Queue [e] []
pushBack e (Queue f b) = Queue f (e:b)

popFront (Queue [] []) = error "queue is empty"
popFront (Queue [e] b) = Queue (reverse b) []
popFront (Queue (e:f) b) = Queue f b
popFront _ = error "queue is empty"

peek (Queue [] []) = error "queue is empty"
peek (Queue [e] _) = e
peek (Queue (e:_) _) = e
peek _ = error "queue is empty"

fromList :: [a] -> Queue a
fromList = foldl (flip pushBack) empty

toList = (++) <$> front <*> reverse . rear

instance (Arbitrary a) => Arbitrary (Queue a) where
    arbitrary = fromList <$> arbitrary

empty' :: [a]
empty' = []

isEmpty' :: [a] -> Bool
isEmpty' = null
pushBack' e = (++ [e])
popFront' = tail
peek' (x:_) = x

invariant (Queue f r) = not ((not . null $ r) && null f)

(==?) q l = invariant q && toList q == l

prop_isEmpty q = isEmpty q == (null . toList $ q)
prop_empty = empty ==? ([] :: [Int])
prop_pushBack x (q :: Queue Int) = pushBack x q ==? (pushBack' x . toList $ q)
prop_popFront q = isEmpty q || popFront q ==? (popFront' . toList $ q)
prop_peek q = isEmpty q || peek q == (peek' . toList $ q)
prop_fromList l = fromList l ==? l

-- Zadanie 4
-- W pliku Monads.hs

-- Zadanie 5

data CoYoneda f a = forall b. CoYoneda (b -> a) (f b)

toCoYoneda :: f a -> CoYoneda f a
toCoYoneda x = CoYoneda id x

fromCoYoneda :: (Functor f) => CoYoneda f a -> f a
fromCoYoneda (CoYoneda f x) = fmap f x

instance Functor (CoYoneda f) where
    fmap f (CoYoneda g x) = CoYoneda (f . g) x

-- (toCoYoneda · fromCoYoneda) Fa = toCoYoneda (λ f. fmap f Fa)
--                                = (λ f. fmap f Fa) id
--                                = fmap id Fa = Fa
-- (fromCoYoneda · toCoYoneda) f = fromCoYoneda (CoYoneda id f)
--                               = fmap id f = f

-- Zadanie 6

data Expr a = Add  (Expr a) (Expr a)
            | Mult (Expr a) (Expr a)
            | Var a deriving (Show, Functor)

data Action = Rename String String
            | CamelCase
            | SnakeCase
            | Print
            | EOS deriving Show

maybeSubstitute :: String -> Maybe Action
maybeSubstitute line = do
    guard . not . null $ line
    guard . (==2) . length . filter (=='/') $ line
    guard . (=='/') . head $ line
    let [_, from, to] = splitOn "/" line
    return $ Rename from to

readAction :: IO (Maybe Action)
readAction = do
    line <- getLine
    return $ case line of
        "print" -> Just Print
        "camel" -> Just CamelCase
        "snake" -> Just SnakeCase
        _ -> maybeSubstitute line

refactor :: Expr String -> IO ()
refactor e = flip fix (toCoYoneda e) $ \loop prog -> do
    action <- fromMaybe EOS <$> readAction
    case action of
        Print -> let compiled = fromCoYoneda prog in
                     print compiled >> loop (toCoYoneda compiled)
        CamelCase -> loop (fmap toCamelCase prog)
        SnakeCase -> loop (fmap toSnakeCase prog)
        Rename from to -> loop (fmap (\name -> if name == from then to else name) prog)
        _ -> loop prog
    where toCamelCase e =
            case splitOn "_" e of
                [] -> []
                [x] -> map toLower x
                (x:xs) -> map toLower x ++ concatMap capitalize xs
          capitalize [] = []
          capitalize (x:xs) = toUpper x : map toLower xs
          splitWhen' = split . keepDelimsL . whenElt
          toSnakeCase = intercalate "_" . map (map toLower) . splitWhen' isUpper

-- Zadanie 7

data Free f a = Node (f (Free f a))
              | Leaf a deriving Functor

sumFree :: (Functor f, Foldable f) => Free f Int -> Int
sumFree (Leaf n)  = n
sumFree (Node s) = foldr' (+) 0 $ fmap sumFree s

newtype DList a = DList { unDList :: [a] -> [a] }

instance Foldable DList where
    foldr f x xs = foldr f x $ unDList xs []

cydList xs = toCoYoneda $ DList (xs ++)
-- works
test = sumFree $ Node $ cydList
    [ Leaf 4
    , Node $ cydList [Leaf 2, Leaf 6, Leaf 1]
    , Node $ cydList [Leaf 10]
    , Leaf 1
    ]

instance (Foldable f) => Foldable (CoYoneda f) where
    foldMap f (CoYoneda g x) = foldMap (f . g) x

-- return []
-- main = $quickCheckAll
main = refactor $ Add (Var "hello") (Mult (Var "good_morning") (Var "goodnight"))
