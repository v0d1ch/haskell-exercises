{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Exercises where

import Data.Kind (Constraint, Type)
import Debug.Trace
-- | Before we get started, let's talk about the @TypeOperators@ extension. All
-- this does is allow us to write types whose names are operators, and write
-- regular names as infix names with the backticks, as we would at the value
-- level.





{- ONE -}

data Nat = Z | S Nat deriving Show

-- | a. Use the @TypeOperators@ extension to rewrite the 'Add' family with the
-- name '+':

type family (x :: Nat) + (y :: Nat) :: Nat where
  'Z  + y = y
  ('S x) + y = 'S (x + y)

-- | b. Write a type family '**' that multiplies two naturals using '(+)'. Which
-- extension are you being told to enable? Why?
-- UndecidableInstances

type family (x :: Nat) ** (y :: Nat) :: Nat where
  'Z ** y  = y
  ('S x) ** y = y + (x ** y)

data SNat (value :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

-- | c. Write a function to add two 'SNat' values.
add :: SNat a -> SNat b -> SNat (a + b)
add SZ x = x
add (SS x) y = SS (add x y)


add' :: Nat -> Nat -> Nat
add' Z x = x
add' (S x) y = S (add' x y)


{- TWO -}

data Vector (count :: Nat) (a :: Type) where
  VNil  :: Vector 'Z a
  VCons :: Show a => a -> Vector n a -> Vector ('S n) a

deriving instance Show (Vector c a)
-- | a. Write a function that appends two vectors together. What would the size
-- of the result be?

append :: Vector m a -> Vector n a -> Vector (m + n) a
append VNil x = x
append (VCons a xs) ys = VCons a (append xs ys)

-- | b. Write a 'flatMap' function that takes a @Vector n a@, and a function
-- @a -> Vector m b@, and produces a list that is the concatenation of these
-- results. This could end up being a deceptively big job.

flatMap :: Vector n a -> (a -> Vector m b) -> Vector (n ** m) b
-- flatMap VNil _ = VNil
flatMap (VCons a as) f = append (f a) (flatMap as f)



{- THREE -}

-- | a. More boolean fun! Write the type-level @&&@ function for booleans.

type family (a :: Bool) && (b :: Bool) where
  'True && 'True = 'True
  _ && _ = 'False

-- | b. Write the type-level @||@ function for booleans.

type family (a :: Bool) || (b :: Bool) where
  'True || _ = 'True
  _ || 'True  = 'True
  _ || _  = 'False

-- | c. Write an 'All' function that returns @'True@ if all the values in a
-- type-level list of boleans are @'True@.

type family All (a :: [Bool]) :: Bool where
  All '[] = 'True
  All (x ': xs) = x && All xs




{- FOUR -}

-- | a. Nat fun! Write a type-level 'compare' function using the promoted
-- 'Ordering' type.

type family Compare (x :: Nat) (y :: Nat) :: Ordering where
  Compare  'Z     'Z    = 'EQ
  Compare  'Z    ('S n) = 'LT
  Compare ('S n)  'Z    = 'GT
  Compare ('S x) ('S y) =  Compare x y

-- | b. Write a 'Max' family to get the maximum of two natural numbers.
type family Max (a :: Nat) (b :: Nat) :: Nat where
  Max x y = Max' (Compare x y) x y

type family Max' (x :: Ordering) (a :: Nat) (b :: Nat) :: Nat where
  Max' 'LT _ y = y
  Max' _ x y = x
-- | c. Write a family to get the maximum natural in a list.





{- FIVE -}

data Tree = Empty | Node Tree Nat Tree

-- | Write a type family to insert a promoted 'Nat' into a promoted 'Tree'.
type family Insert (x :: Nat) (y :: Tree) :: Tree where
  Insert _ 'Empty = 'Empty
  Insert x ('Node l i r) = 'Node l (x + i) r





{- SIX -}

-- | Write a type family to /delete/ a promoted 'Nat' from a promoted 'Tree'.





{- SEVEN -}

-- | With @TypeOperators@, we can use regular Haskell list syntax on the
-- type-level, which I think is /much/ tidier than anything we could define.

data HList (xs :: [Type]) where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

-- | Write a function that appends two 'HList's.





{- EIGHT -}

-- | Type families can also be used to build up constraints. There are, at this
-- point, a couple things that are worth mentioning about constraints:
--
-- - As we saw before, '()' is the empty constraint, which simply has "no
--   effect", and is trivially solved.
--
-- - Unlike tuples, constraints are "auto-flattened": ((a, b), (c, (d, ())) is
--   exactly equivalent to (a, b, c, d). Thanks to this property, we can build
--   up constraints using type families!

type family CAppend (x :: Constraint) (y :: Constraint) :: Constraint where
  CAppend x y = (x, y)

-- | a. Write a family that takes a constraint constructor, and a type-level
-- list of types, and builds a constraint on all the types.

type family Every (c :: Type -> Constraint) (x :: [Type]) :: Constraint where
  -- ...

-- | b. Write a 'Show' instance for 'HList' that requires a 'Show' instance for
-- every type in the list.

-- | c. Write an 'Eq' instance for 'HList'. Then, write an 'Ord' instance.
-- Was this expected behaviour? Why did we need the constraints?





{- NINE -}

-- | a. Write a type family to calculate all natural numbers up to a given
-- input natural.

-- | b. Write a type-level prime number sieve.

-- | c. Why is this such hard work?
