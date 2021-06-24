import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.Char
import Data.List
import Data.STRef

main :: IO ()
main = undefined

-- Zadanie 2
-- Testowałem na losowych danych długości 200k, gdzie klucze to 32-bitowe inty próbkowane w 
-- rozkładzie jednostajnym. Wyszło mi, że buckerSort dla wielkości kubełków ~100 działał średnio
-- o 15% szybciej niż normalny sortBy

bucketSize :: Int
bucketSize = 100

bucketSort :: [(Int, a)] -> [(Int, a)]
bucketSort l = concatMap (sortBy ((. fst) . compare . fst)) . elems $ runSTArray $ do
              a <- newArray (0, bucketSize - 1) []
              forM_ l $ \(i, v) -> do
                  let k = ceiling $ b * fromIntegral i / n
                  t <- readArray a k
                  writeArray a k ((i, v):t)
              return a
    where n = fromIntegral . maximum . map fst $ l
          b = fromIntegral (bucketSize - 1)

-- Zadanie 3

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
data Graph = Graph Int [Edge] deriving Show
-- Założenia: graf ma wierzchołki o indentyfikatorach 0-n (pierwszy element konstruktora)
--            dla krawędzi (v, x, u) zachodzi v <= u
--            istnieje tylko jeden element dla krawędzi v <-> u postaci (v, x, u)

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

testGraph = graphCreate $ map (\(v, x, w) -> (ord v - 97, x, ord w - 97)) [
    ('a', 1.0, 'e'),
    ('c', 2.0, 'd'),
    ('a', 3.0, 'b'),
    ('b', 4.0, 'e'),
    ('b', 5.0, 'c'),
    ('c', 6.0, 'e'),
    ('d', 7.0, 'e')]

-- Zadanie 4
-- Testowałem na losowych, spójnych grafach wielkości rzędu 50k nodów,
-- i wersja funkcyjna algorytmu Kruskala jest sporo wolniejsza od jej
-- <<mutowalnej>> wersji.

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
