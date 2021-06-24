-- Lista 8
-- Marcin Witkowski

{-# LANGUAGE GADTs, DataKinds, TypeOperators, RankNTypes, FlexibleContexts, FlexibleInstances,
             StandaloneDeriving, TypeFamilyDependencies, UndecidableInstances, IncoherentInstances,
             ConstraintKinds, QuantifiedConstraints #-}

import GHC.TypeLits (TypeError,  ErrorMessage(..))
import Data.Maybe

main :: IO ()
main = undefined

data Nat = Zero | Succ Nat

type family x + y where
    Zero + k = k
    Succ n + k = Succ (n + k)

-- Zadanie 1

data BTree :: Nat -> * -> *  where
    BLeaf :: BTree Zero a
    BNode :: BTree h a -> a -> BTree h a -> BTree (Succ h) a

-- Zadanie 2

type One = Succ Zero
type Two = Succ One
type Three = Succ Two
type Four = Succ Three
type Five = Succ Four
type Six = Succ Five
type Seven = Succ Six
type Eight = Succ Seven

data SNat :: Nat -> * where
    SZero :: SNat Zero
    SSucc :: SNat n -> SNat (Succ n)

deriving instance Show (SNat h)

type family SameD (l :: Nat) (r :: Nat) where
    SameD n n = True
    SameD n m = TypeError (Text "Matrices have wrong sizes: " :<>: ShowType n :<>: Text " vs. " :<>: ShowType m)

class NatSize (n :: Nat) where
    natSize :: SNat n

instance NatSize Zero where
    natSize = SZero

instance NatSize m => NatSize (Succ m) where
    natSize = SSucc natSize

instance NatSize (m :: Nat) where
    natSize = natSize

data Vector :: Nat -> * -> * where
    VecCons :: a -> Vector h a -> Vector (Succ h) a
    VecNil :: Vector Zero a

data Matrix :: Nat -> Nat -> * -> * where
    MatCons :: Vector h a -> Matrix h w a -> Matrix h (Succ w) a
    MatNil :: Matrix h Zero a

deriving instance (Show a) => Show (Vector h a)
deriving instance (Show a) => Show (Matrix h w a)

instance Functor (Matrix h w) where
    fmap f (MatCons h t) = MatCons (fmap f h) (fmap f t)
    fmap _ MatNil = MatNil

instance Functor (Vector h) where
    fmap f (VecCons a t) = VecCons (f a) (fmap f t)
    fmap _ VecNil = VecNil

matWidth :: Matrix h w a -> SNat w
matWidth (MatCons _ t) = SSucc $ matWidth t
matWidth MatNil = SZero

matHeight :: Matrix h w a -> SNat h
matHeight (MatCons v _) = vecHeight v
matHeight MatNil = undefined

vecHeight :: Vector h a -> SNat h
vecHeight (VecCons _ t) = SSucc $ vecHeight t
vecHeight VecNil = SZero

instance Applicative (Matrix h w) where
    pure a = pureMat (pure a) natSize where
        pureMat :: Vector h a -> SNat w -> Matrix h w a
        pureMat _ SZero = MatNil
        pureMat v (SSucc n) = MatCons v (pureMat v n)

    MatNil <*> _ = MatNil
    (MatCons f fs) <*> (MatCons h t) = MatCons (f <*> h) (fs <*> t)

instance Applicative (Vector h) where
    pure = flip pureVec natSize
        where pureVec :: a -> SNat n -> Vector n a
              pureVec _ SZero = VecNil
              pureVec a (SSucc n) = VecCons a (pureVec a n)

    VecNil <*> _ = VecNil
    (VecCons f fs) <*> (VecCons h t) = VecCons (f h) (fs <*> t)

mat22 = MatCons v1 (MatCons v2 MatNil) where
    v1 = VecCons 1 (VecCons 1 VecNil)
    v2 = VecCons 1 (VecCons 0 VecNil)

mat21 = MatCons v MatNil where
    v = VecCons 5 $ VecCons 3 VecNil

vmap :: (Vector h a -> a) -> Matrix h w a -> Vector w a
vmap f (MatCons v vs) = VecCons (f v) (vmap f vs)
vmap f MatNil = VecNil

matAdd :: Num a => Matrix j i a -> Matrix j i a -> Matrix j i a
matAdd a b = (+) <$> a <*> b

vecHead :: Vector h a -> a
vecHead (VecCons v _) = v

vecDot :: Num a => Vector h a -> Vector h a -> a
vecDot v w = vecSum $ (*) <$> v <*> w

unstack :: Matrix (Succ h) w a -> (Vector w a, Matrix h w a)
unstack (MatCons (VecCons a as) vs) = (na, ma) where
    (m, ms) = unstack vs
    na = VecCons a m
    ma = MatCons as ms
unstack MatNil = (VecNil, MatNil)

matHStack :: Vector i a -> Matrix j i a -> Matrix (Succ j) i a
matHStack (VecCons a t) (MatCons v vs) = MatCons (VecCons a v) (matHStack t vs)
matHStack VecNil _ = MatNil

vecSum :: Num a => Vector h a -> a
vecSum (VecCons a t) = a + vecSum t
vecSum VecNil = 0

matSum :: Num a => Matrix j i a -> a
matSum MatNil = 0
matSum (MatCons v vs) = vecSum v + matSum vs

matMul :: (SameD i1 j2 ~ True, j2 ~ i1, Num a) => Matrix (Succ j1) i1 a -> Matrix j2 i2 a -> Matrix (Succ j1) i2 a
matMul a@(MatCons (VecCons _ VecNil) _) b = transpose row
    where (v, _) = unstack a
          row = vmap (vecDot v) b
          transpose :: Vector h a -> Matrix One h a
          transpose (VecCons a t) = MatCons (VecCons a VecNil) (transpose t)
          transpose VecNil = MatNil
matMul a@(MatCons (VecCons _ (VecCons _ _)) _) b = matHStack row rest
    where (v, vs) = unstack a
          row = vmap (vecDot v) b
          rest = matMul vs b

-- Zadanie 3

data XPos = XA | XB | XC | XD | XE | XF | XG | XH
data YPos = Y1 | Y2 | Y3 | Y4 | Y5 | Y6 | Y7 | Y8
data Who = Black | White

data SXPos (x :: XPos) where
    SXA :: SXPos XA
    SXB :: SXPos XB
    SXC :: SXPos XC
    SXD :: SXPos XD
    SXE :: SXPos XE
    SXF :: SXPos XF
    SXG :: SXPos XG
    SXH :: SXPos XH

data SYPos (x :: YPos) where
    SY1 :: SYPos Y1
    SY2 :: SYPos Y2
    SY3 :: SYPos Y3
    SY4 :: SYPos Y4
    SY5 :: SYPos Y5
    SY6 :: SYPos Y6
    SY7 :: SYPos Y7
    SY8 :: SYPos Y8

type family SwapColor (who :: Who) where
    SwapColor Black = White
    SwapColor White = Black

type family NewPosition (is :: Who) (who :: Who) (x :: XPos) (y :: YPos) (nx :: XPos) (ny :: YPos) where
    NewPosition a a x _ x ny = '(x, ny)
    NewPosition a a _ y nx y = '(nx, y)
    NewPosition a a _ _ _ _ = TypeError (Text "Rook can only move in one axis")
    NewPosition _ _ x y _ _ = '(x, y)

type family GameEnded (wx :: XPos) (wy :: YPos) (bx :: XPos) (by :: YPos) where
    GameEnded x y x y = TypeError (Text "Game ended, you can't move anymore")
    GameEnded _ _ _ _ = False

type family XAsNat (p :: XPos) where
    XAsNat XA = One
    XAsNat XB = Two
    XAsNat XC = Three
    XAsNat XD = Four
    XAsNat XE = Five
    XAsNat XF = Six
    XAsNat XG = Seven
    XAsNat XH = Eight

type family YAsNat (p :: YPos) where
    YAsNat Y1 = One
    YAsNat Y2 = Two
    YAsNat Y3 = Three
    YAsNat Y4 = Four
    YAsNat Y5 = Five
    YAsNat Y6 = Six
    YAsNat Y7 = Seven
    YAsNat Y8 = Eight

type family (a :: Nat) < (b :: Nat) where
    a < a = False
    (Succ a) < a = False
    a < (Succ a) = True
    a < Zero = False
    a < (Succ b) = a < b

type family And (a :: Bool) (b :: Bool) where
    And True True = True
    And _ _ = False

type family Or (a :: Bool) (b :: Bool) where
    Or True _ = True
    Or _ True = True
    Or _ _ = False

type family PhaseError (b :: Bool) where
    PhaseError True = TypeError (Text "One Rook phased though second one")
    PhaseError False = False

type family PositionDiffer (who :: Who) (wpos :: (XPos, YPos)) (bpos :: (XPos, YPos)) (npos :: (XPos, YPos)) where
    PositionDiffer White a _ a = TypeError (Text "You can't stay in place")
    PositionDiffer Black _ a a = TypeError (Text "You can't stay in place")
    PositionDiffer _ _ _ _ = True

type InBetween (a :: Nat) (b :: Nat) (c :: Nat) = And (a < b) (b < c)
type XInBetween (a :: XPos) (b :: XPos) (c :: XPos) = PhaseError
    (Or (InBetween (XAsNat a) (XAsNat b) (XAsNat c))
        (InBetween (XAsNat c) (XAsNat b) (XAsNat a)))
type YInBetween (a :: YPos) (b :: YPos) (c :: YPos) = PhaseError
    (Or (InBetween (YAsNat a) (YAsNat b) (YAsNat c))
        (InBetween (YAsNat c) (YAsNat b) (YAsNat a)))

data Chess :: XPos -> YPos -> XPos -> YPos -> Who -> * where
    Start :: Chess XA Y1 XH Y8 White
    (:|>) :: (GameEnded wx wy bx by ~ False,
             PositionDiffer who '(wx, wy) '(bx, by) '(nx, ny) ~ True,
             SwapColor who ~ newWho,
             NewPosition White who wx wy nx ny ~ '(nwx, nwy),
             NewPosition Black who bx by nx ny ~ '(nbx, nby),
             XInBetween wx bx nwx ~ False,
             XInBetween bx wx nbx ~ False,
             YInBetween wy by nwy ~ False,
             YInBetween by wy nby ~ False) =>
        Chess wx wy bx by who -> (SXPos nx, SYPos ny) -> Chess nwx nwy nbx nby newWho

game = Start :|> (SXA, SY8) :|> (SXC, SY8) :|> (SXC, SY8)

-- Zadanie 4

data SBool :: Bool -> * where
    STrue :: SBool True
    SFalse :: SBool False

class BoolValue (b :: Bool) where
    boolValue :: SBool b

instance BoolValue True where
    boolValue = STrue

instance BoolValue False where
    boolValue = SFalse

fromSBool :: SBool b -> Bool
fromSBool STrue = True
fromSBool SFalse = False

type family CorrectHeight (h1 :: Nat) (h2 :: Nat) where
    CorrectHeight (Succ h) h = False
    CorrectHeight h h = True
    CorrectHeight _ _ = TypeError (Text "HTree is not balanced")

type family InnerSkip (b :: Bool) (b1 :: Bool) (b2 :: Bool) where
    InnerSkip False True True = TypeError (Text "HTree have inner skip")
    InnerSkip True True _ = TypeError (Text "HTree have inner skip")
    InnerSkip False True False = True
    InnerSkip False False True = True
    InnerSkip True _ _ = False
    InnerSkip False _ _ = False
    InnerSkip e _ _ = e

data HTree :: Nat -> Bool -> * -> * where
    HLeaf :: a -> HTree Zero False a
    HNode1 :: HTree h False a -> a -> HTree (Succ h) True a
    HNode2 :: (CorrectHeight h1 h2 ~ b, InnerSkip b b1 b2 ~ s) =>
        HTree h1 b1 a -> a -> HTree h2 b2 a -> HTree (Succ h1) s a

-- x = HNode2 (HLeaf 1) 2 (HNode2 (HLeaf 1) 2 (HLeaf 3))

deriving instance (Show a) => Show (HTree h b a)

-- insert :: a -> HTree h b1 a -> HTree h2 b2 a
-- insert a (HLeaf v)
--   | a > v = HNode1 (HLeaf a) v
--   | otherwise = HNode1 (HLeaf v) a
-- insert a (HNode1 (HLeaf v') v)
--     | v < a = HNode2 (HLeaf v') v (HLeaf a)
--     | otherwise = HNode2 (HLeaf v') a (HLeaf v)

findMin :: HTree h b a -> a
findMin (HLeaf v) = v
findMin (HNode1 _ v) = v
findMin (HNode2 _ v _) = v

-- Zadanie 5

type Vertices n a = Vector n a

type family CorrectEdges (max :: Nat) (list :: [(Nat, Nat)]) where
    CorrectEdges _ '[] = True
    CorrectEdges max ('(a, b):xs) = And (And (a < Succ max) (b < Succ max)) (CorrectEdges max xs)

data Edges :: Nat -> [(Nat, Nat)] -> * where
    EdgeCons :: SNat a -> SNat b -> Edges n l -> Edges (Succ n) ('(a, b):l)
    EdgeNil :: Edges Zero '[]

data Graph_ :: Nat -> Nat -> * -> [(Nat, Nat)] -> * where
    Graph_ :: CorrectEdges n e ~ True => Vertices n v -> Edges m e -> Graph_ n m v e

data Graph v = forall m n l. Graph (Graph_ m n v l)

data Col = R | G | B
data SCol :: Col -> * where
    SR :: SCol R
    SG :: SCol G
    SB :: SCol B

type family ColorDiffer (a :: Col) (b :: Col) where
    ColorDiffer a a = TypeError (Text "Vertices have the same colour")
    ColorDiffer _ _ = True

type family Correct3ColEdges (max :: Nat) (list :: [(Nat, Col, Nat, Col)]) where
    Correct3ColEdges _ '[] = True
    Correct3ColEdges max ('(a, ca, b, cb):xs) = And (And (And (a < Succ max) (b < Succ max)) (ColorDiffer ca cb)) (Correct3ColEdges max xs)

data Edges3Col :: Nat -> [(Nat, Col, Nat, Col)] -> * where
    Edge3ColCons :: (SNat a, SCol ca) -> (SNat b, SCol cb) -> Edges3Col n l -> Edges3Col (Succ n) ('(a, ca, b, cb):l)
    Edge3ColNil :: Edges3Col Zero '[]

data Graph3Col_ :: Nat -> Nat -> * -> [(Nat, Col, Nat, Col)] -> * where
    Graph3Col_ :: Correct3ColEdges n l ~ True => Vertices n v -> Edges3Col m l -> Graph3Col_ n m v l

data Graph3Col v = forall m n l. Graph3Col (Graph3Col_ m n v l)

getID :: Vertices n a -> SNat n
getID _ = natSize

graph1 = graph where
    l1 = VecCons 1 VecNil
    id1 = getID l1
    l2 = VecCons 2 l1
    id2 = getID l2
    l3 = VecCons 2 l2
    id3 = getID l3
    e = Edge3ColCons (id1, SR) (id2, SG) $
        Edge3ColCons (id2, SG) (id3, SB)
        Edge3ColNil
    graph = Graph3Col $ Graph3Col_ l3 e

graph2 = graph where
    l1 = VecCons 1 VecNil
    id1 = getID l1
    l2 = VecCons 2 l1
    id2 = getID l2
    l3 = VecCons 2 l2
    id3 = getID l3
    l4 = VecCons 2 l3
    id4 = getID l4
    e = Edge3ColCons (id1, SR) (id2, SG) $
        Edge3ColCons (id2, SG) (id3, SB) $
        Edge3ColCons (id3, SB) (id4, SG) $
        Edge3ColCons (id1, SR) (id3, SG)
        Edge3ColNil
    graph = Graph3Col_ l4 e

-- -- Error: Vertices have same colour
-- graph3 = graph where
--     l1 = VecCons 1 VecNil
--     id1 = getID l1
--     l2 = VecCons 2 l1
--     id2 = getID l2
--     l3 = VecCons 2 l2
--     id3 = getID l3
--     l4 = VecCons 2 l3
--     id4 = getID l4
--     e = Edge3ColCons (id1, SR) (id2, SG) $
--         Edge3ColCons (id2, SG) (id3, SB) $
--         Edge3ColCons (id3, SB) (id4, SG) $
--         Edge3ColCons (id1, SR) (id3, SG) $
--         Edge3ColCons (id2, SG) (id4, SG)
--         Edge3ColNil
--     graph = Graph3Col $ Graph3Col_ l4 e
