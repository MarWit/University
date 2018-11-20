module RayTracer.Vector where

import Data.Default
import Control.DeepSeq

data VecT a = Vector !a !a !a deriving (Show, Eq)

type Scalar = Double
type Vector = VecT Scalar

instance Functor VecT where
    fmap f (Vector x y z) = Vector (f x) (f y) (f z)

instance Applicative VecT where
    pure x = Vector x x x
    (<*>) (Vector f g h) (Vector x y z) = Vector (f x) (g y) (h z)

instance Num a => Default (VecT a) where
    def = Vector 0 0 0

instance Num a => NFData (VecT a) where
    rnf = (`seq` ())

-- Add two vectors together
(<+>) :: Num a => VecT a -> VecT a -> VecT a
(<+>) = (<*>) . fmap (+)

-- Subtracs two vectors
(<->) :: Num a => VecT a -> VecT a -> VecT a
(<->) = (<*>) . fmap (-)

-- Multiplication by scalar
(<**>) :: Num a => a -> VecT a -> VecT a
(<**>) a = fmap (a *)

-- Sum of vector components
vsum :: Num a => VecT a -> a
vsum (Vector x y z) = x + y + z

vmult :: Num a => VecT a -> VecT a -> VecT a
vmult (Vector x y z) (Vector u v w) = Vector (x * u) (y * v) (z * w)

vneg :: Num a => VecT a -> VecT a
vneg = fmap negate

-- Dot product
(<.>) :: Num a => VecT a -> VecT a -> a
(<.>) a b = vsum $ ((<*>) . fmap (*)) a b

-- Cross product
(><) :: Num a => VecT a -> VecT a -> VecT a
(><) (Vector x y z) (Vector u v w) =
    Vector (y * w - z * v)
           (z * u - x * w)
           (x * v - y * u)


-- Squared norm of vector
normSq :: Num a => VecT a -> a
normSq = vsum . fmap (\x -> x * x)

-- Norm of vector
norm :: Vector -> Scalar
norm = sqrt . normSq

-- Vector reflected over normal
reflect :: Num a => VecT a -> VecT a -> VecT a
reflect vector normal = (2 * vector <.> normal) <**> normal <-> vector

-- Normalized vector
normalized :: Vector -> Vector
normalized v = fmap ((flip (/)) len) v
    where len = norm v

fst :: Num a => VecT a -> a
fst (Vector x _ _) = x

snd :: Num a => VecT a -> a
snd (Vector _ y _) = y

trd :: Num a => VecT a -> a
trd (Vector _ _ z) = z
