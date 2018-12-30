{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fshow-hole-constraints #-}
module Exercises where

import Data.Monoid ((<>))
import Data.Maybe
{- ONE -}

-- | Let's introduce a new class, 'Countable', and some instances to match.
class Show a => Countable a where count :: a -> Int
instance Countable Int  where count = id
instance Show a => Countable [a]  where count = length
instance Countable Bool where count x = if x then 1 else 0

-- | a. Build a GADT, 'CountableList', that can hold a list of 'Countable'
-- things.

data CountableList where
  CNil :: CountableList
  CCons :: Countable a => a -> CountableList -> CountableList

deriving instance Show CountableList

-- | b. Write a function that takes the sum of all members of a 'CountableList'
-- once they have been 'count'ed.

countList :: CountableList -> Int
countList CNil = 0
countList (CCons a list) = count a + countList list


-- | c. Write a function that removes all elements whose count is 0.

dropZero :: CountableList -> CountableList
dropZero CNil = CNil
dropZero (CCons x xs) =
  if count x == 0
     then dropZero xs
     else CCons x (dropZero xs)


-- | d. Can we write a function that removes all the things in the list of type
-- 'Int'? If not, why not?
-- We can't do it. We don't know nothing about the type except that is has Countable constraint
-- so we can only count it
filterInts :: CountableList -> CountableList
filterInts = undefined





{- TWO -}

-- | a. Write a list that can take /any/ type, without any constraints.

data AnyList where
  ANNil :: AnyList
  ACCons :: Show a => a -> AnyList -> AnyList

deriving instance Show AnyList

-- | b. How many of the following functions can we implement for an 'AnyList'?

reverseAnyList :: AnyList -> AnyList
reverseAnyList xs =
  go xs id
  where
    go ANNil f = f ANNil
    go (ACCons a as) f = go as (ACCons a . f)

-- this assumes all of the types need to be the same @a@ which is not the case since a in AnyList
-- is existential
filterAnyList :: (a -> Bool) -> AnyList -> AnyList
filterAnyList = undefined

countAnyList :: AnyList -> Int
countAnyList ANNil = 0
countAnyList (ACCons _ xs) = 1 + countAnyList xs

isEmptyAnyList :: AnyList -> Bool
isEmptyAnyList ANNil = False
isEmptyAnyList _ = True

-- instance Show AnyList where
--   show ANNil = " ANNil "
--   show (ACCons x xs) = " ACCons " ++ show x ++ show xs





{- THREE -}

-- | Consider the following GADT:

data TransformableTo output where
  TransformWith
    :: (input -> output)
    ->  input
    -> TransformableTo output

-- | ... and the following values of this GADT:

transformable1 :: TransformableTo String
transformable1 = TransformWith show 2.5

transformable2 :: TransformableTo String
transformable2 = TransformWith (uncurry (++)) ("Hello,", " world!")

-- | a. Which type variable is existential inside 'TransformableTo'? What is
-- the only thing we can do to it?
-- input, we can know the type of output
-- | b. Could we write an 'Eq' instance for 'TransformableTo'? What would we be
-- able to check?
-- we can write the instance that is capable of checking the result
-- | c. Could we write a 'Functor' instance for 'TransformableTo'? If so, write
-- it. If not, why not?
instance Functor TransformableTo where
  fmap f (TransformWith fnc i) = TransformWith (f . fnc) i




{- FOUR -}

-- | Here's another GADT:

data EqPair where
  EqPair :: Eq a => a -> a -> EqPair

-- | a. There's one (maybe two) useful function to write for 'EqPair'; what is
-- it?
-- id
-- | b. How could we change the type so that @a@ is not existential? (Don't
-- overthink it!)
-- use fied type for one of the parameters
-- | c. If we made the change that was suggested in (b), would we still need a
-- GADT? Or could we now represent our type as an ADT?
-- no





{- FIVE -}

-- | Perhaps a slightly less intuitive feature of GADTs is that we can set our
-- type parameters (in this case @a@) to different types depending on the
-- constructor.

data MysteryBox a where
  EmptyBox  ::                                MysteryBox ()
  IntBox    :: Int    -> MysteryBox ()     -> MysteryBox Int
  StringBox :: String -> MysteryBox Int    -> MysteryBox String
  BoolBox   :: Bool   -> MysteryBox String -> MysteryBox Bool

-- | When we pattern-match, the type-checker is clever enough to
-- restrict the branches we have to check to the ones that could produce
-- something of the given type.

getInt :: MysteryBox Int -> Int
getInt (IntBox int _) = int

-- | a. Implement the following function by returning a value directly from a
-- pattern-match:

getInt' :: MysteryBox String -> Int
getInt' (StringBox _ (IntBox i _)) = i

-- | b. Write the following function. Again, don't overthink it!

countLayers :: MysteryBox a -> Int
countLayers EmptyBox = 0
countLayers (IntBox _ xs) = 1 + countLayers xs
countLayers (StringBox _ xs) = 1 + countLayers xs
countLayers (BoolBox _ xs) = 1 + countLayers xs

-- | c. Try to implement a function that removes one layer of "Box". For
-- example, this should turn a BoolBox into a StringBox, and so on. What gets
-- in our way? What would its type be?




{- SIX -}

-- | We can even use our type parameters to keep track of the types inside an
-- 'HList'!  For example, this heterogeneous list contains no existentials:

data HList a where
  HNil  :: HList ()
  HCons :: head -> HList tail -> HList (head, tail)

exampleHList :: HList (String, (Int, (Bool, ())))
exampleHList = HCons "Tom" (HCons 25 (HCons True HNil))

-- | a. Write a 'head' function for this 'HList' type. This head function
-- should be /safe/: you can use the type signature to tell GHC that you won't
-- need to pattern-match on HNil, and therefore the return type shouldn't be
-- wrapped in a 'Maybe'!

typeSafeHead :: HList (String, (Int, (Bool, ()))) -> String
typeSafeHead (HCons a _) = a

-- | b. Currently, the tuples are nested. Can you pattern-match on something of
-- type @HList (Int, String, Bool, ())@? Which constructor would work?

-- can't pattern match on this one
patternMatchMe :: HList (Int, String, Bool, ()) -> Int
patternMatchMe  = undefined

-- | c. Can you write a function that appends one 'HList' to the end of
-- another? What problems do you run into?
append :: HList a -> HList b -> HList c
append = undefined
-- problem is that the HList tail keeps `expanding`

{- SEVEN -}

-- | Here are two data types that may help:

data Empty
data Branch left centre right

-- | a. Using these, and the outline for 'HList' above, build a heterogeneous
-- /tree/. None of the variables should be existential.

data HTree a where
  HEmpty :: HTree Empty
  HBranch :: l -> c -> HTree r -> HTree (Branch l c r)

-- | b. Implement a function that deletes the left subtree. The type should be
-- strong enough that GHC will do most of the work for you. Once you have it,
-- try breaking the implementation - does it type-check? If not, why not?
deleteLeft
  :: HTree (Branch left centre right)
  -> HTree (Branch (HTree Empty) centre right)
deleteLeft (HBranch _ centre right)
  = HBranch HEmpty centre right

-- | c. Implement 'Eq' for 'HTree's. Note that you might have to write more
-- than one to cover all possible HTrees. You might also need an extension or
-- two, so look out for something... flexible... in the error messages!
-- Recursion is your friend here - you shouldn't need to add a constraint to
-- the GADT!

instance Eq (HTree Empty) where
  HEmpty == HEmpty = True

instance (Eq left , Eq (HTree left), Eq centre, Eq (HTree right))
    => Eq (HTree (Branch left centre right)) where
  HBranch l x r == HBranch l' x' r' = l == l' && x == x' && r == r'


{- EIGHT -}

-- | a. Implement the following GADT such that values of this type are lists of
-- values alternating between the two types. For example:
--
-- @
--   f :: AlternatingList Bool Int
--   f = ACons True (ACons 1 (ACons False (ACons 2 ANil)))
-- @

data AlternatingList a b where
  ANil :: (Show a, Show b) => AlternatingList a b
  ACons :: (Show a, Show b) => a -> AlternatingList b a -> AlternatingList a b

deriving instance (Show a, Show b) => Show (AlternatingList a b)
-- | b. Implement the following functions.

getFirsts :: AlternatingList a b -> [a]
getFirsts ANil = []
getFirsts (ACons a xs) = a : getSeconds xs

getSeconds :: AlternatingList a b -> [b]
getSeconds  ANil        = []
getSeconds (ACons _ xs) = getFirsts xs

-- | c. One more for luck: write this one using the above two functions, and
-- then write it such that it only does a single pass over the list.

foldValues :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues ANil  = (mempty, mempty)
foldValues list = (mconcat (getFirsts list), mconcat (getSeconds list))

{- NINE -}

-- | Here's the "classic" example of a GADT, in which we build a simple
-- expression language. Note that we use the type parameter to make sure that
-- our expression is well-formed.

data Expr a where
  Equals    :: Expr Int  -> Expr Int            -> Expr Bool
  Add       :: Expr Int  -> Expr Int            -> Expr Int
  If        :: Expr Bool -> Expr a   -> Expr a  -> Expr a
  IntValue  :: Int                              -> Expr Int
  BoolValue :: Bool                             -> Expr Bool

-- | a. Implement the following function and marvel at the typechecker:

eval :: (Show a, Eq a) => Expr a -> a
eval (IntValue i) = i
eval (BoolValue b) = b
eval (Equals a b) = eval a == eval b
eval (Add a b) = eval a + eval b
eval (If a b c) = if eval a then eval b else eval c

deriving instance Show a => Show (Expr a)
-- | b. Here's an "untyped" expression language. Implement a parser from this
-- into our well-typed language. Note that (until we cover higher-rank
-- polymorphism) we have to fix the return type. Why do you think this is?

data DirtyExpr
  = DirtyEquals    DirtyExpr DirtyExpr
  | DirtyAdd       DirtyExpr DirtyExpr
  | DirtyIf        DirtyExpr DirtyExpr DirtyExpr
  | DirtyIntValue  Int
  | DirtyBoolValue Bool
  deriving Eq

parse :: DirtyExpr -> Maybe (Expr Int)
parse (DirtyIntValue i)  = Just $ IntValue i
parse (DirtyBoolValue b)  = if b then Just $ IntValue 1 else Nothing
parse (DirtyEquals a b) = if a == b then parse a else parse b
parse (DirtyAdd _ _) = Nothing
parse (DirtyIf _ _ _) = Nothing

-- | c. Can we add functions to our 'Expr' language? If not, why not? What
-- other constructs would we need to add? Could we still avoid 'Maybe' in the
-- 'eval' function?
-- we can but we need to add a type parameter/s





{- TEN -}

-- | Back in the glory days when I wrote JavaScript, I could make a composition
-- list like @pipe([f, g, h, i, j])@, and it would pass a value from the left
-- side of the list to the right. In Haskell, I can't do that, because the
-- functions all have to have the same type :(

-- | a. Fix that for me - write a list that allows me to hold any functions as
-- long as the input of one lines up with the output of the next.
-- hmm....
data TypeAlignedList a b where
  Z :: TypeAlignedList a a
  S :: (a -> b) -> TypeAlignedList b c -> TypeAlignedList a c

-- | b. Which types are existential?

-- | c. Write a function to append type-aligned lists. This is almost certainly
-- not as difficult as you'd initially think.
appendTAL :: TypeAlignedList b c -> TypeAlignedList a b -> TypeAlignedList a c
appendTAL xs Z = xs
appendTAL xs (S y ys) = S y (appendTAL xs ys)

-- composeTALs :: TypeAlignedList b c -> TypeAlignedList a b -> TypeAlignedList a c
-- composeTALs Z whatever = whatever

data A = A
  { _x :: Maybe Int
  , _y :: Maybe Int
  } deriving (Show,Eq)

test :: IO ()
test = do
  -- let x = Just 1
      -- y = Just 3
  let a = Nothing -- $ A { _x = x, _y = y }
  case (_x =<< a, _y =<< a) of
    (Just _, Just _) -> print "all good"
    _  -> print "something went wrong"








