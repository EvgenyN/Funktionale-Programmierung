-- | Functional Programming, course at LMU, summer term 2012
--   Andreas Abel and Steffen Jost
--
-- Exercise sheet 1, 2012-04-25
--
-- Instructions:
--
-- Replace all occurrences of 'undefined' and 'Undefined' by sensible code.
-- Do not change the type of functions you are asked to implement!
-- Quickcheck test cases may help you finding bugs.
--
-- Spell out proofs where asked for.
--
-- Submit your solutions via UniWorX.

----------------------------------------------------------------------
-- Header
--
-- You can add imports and LANGUAGE pragmas here.
----------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Monads where

import Control.Monad.Error

import qualified Data.Set as Set

import Test.QuickCheck
import Test.QuickCheck.All

data Undefined

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- a) Define functoriality from a monad

infixl 5 <$>

-- | Map function for monads.
(<$>) :: Monad m => (a -> b) -> m a -> m b
f <$> m = do x <- m
             return $ f x

-- b) For your implementation, prove the functor laws from the monad laws

{-
Identity:
  id <$> m = m

let m1 = id <$> m = do x <- m
                       return $ id x

Nach "Links-Eins" ist m1 = id m = m

Composition:
  f <$> (g <$> m) = f . g <$> m

1) let m1 = g <$> m = do x <- m
                         return $ g x

Nach "Links-Eins" ist m1 = g m

2) let m2 = f <$> m1 = do x <- m1               
                          return f x            

Nach "Links-Eins" ist m2 = f m1 = f . g m

3) let m3 = f . g <$> m = do x <- m
                             return f . g x  

Nach "Links-Eins" ist m3 = f . g m   

Also, m2 = m3            

-}

-- Tests for Exercise 1 ----------------------------------------------

prop_mmap_identity m =
  id <$> m == (m :: [Int])

-- TEST prop_mmap_identity [1,3,5]

prop_mmap_composition f g m =
  f <$> (g <$> m) == f . g <$> (m :: [Int])

-- TEST prop_mmap_composition (\x -> 2*x) (\x -> x*x) [1,3,5]

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-- Define the Monad and MonadPlus instances for the following
-- Option type constructor.

data Option a = None | Some a
  deriving (Show, Eq, Ord)

bindO :: Option a -> (a -> Option b) -> Option b
bindO None k = None
bindO (Some a) k = k a

instance Monad Option where
  return a = Some a
  (>>=) = bindO

instance MonadPlus Option where
  mzero = None
  mplus None a    = a 
  mplus a    b    = a

-- Tests for Exercise 2 ----------------------------------------------

instance Arbitrary a => Arbitrary (Option a) where
  arbitrary = frequency [(1, return None), (4, fmap Some arbitrary)]
  shrink _  = []

instance Show (a -> b) where
  show f = "<function>"

-- Monad laws
prop_monad_left a k =
  (return a >>= k)         == (k a :: Option Int)
prop_monad_right m =
  (m >>= return)           == (m   :: Option Int)
prop_monad_assoc m k l =
  (m >>= \ x -> k x >>= l) == (m >>= k >>= l :: Option Int)

-- MonadZero laws
prop_mzero_left  m = mzero `mplus` m == (m :: Option Int)
prop_mzero_right m = m `mplus` mzero == (m :: Option Int)
prop_mzero_assoc m1 m2 m3 =
  m1 `mplus` (m2 `mplus` m3) == (m1 `mplus` m2) `mplus` (m3 :: Option Int)

-- MonadZero bind laws
prop_zero_bind_l k   = (mzero >>= k) == (mzero :: Option Int)
prop_zero_bind_r m   = (m >> mzero)  == (mzero :: Option Int)

-- Left distribution law does not hold for Option
-- prop_plus_dist_l m n k =
--   ((m `mplus` n) >>= k) == ((m >>= k) `mplus` (n >>= k) :: Option Int)
prop_plus_dist_r m k l =
  (m >>= \ x -> k x `mplus` l x) == ((m >>= k) `mplus` (m >>= l) :: Option Int)

testOption = (prop_monad_left 2 (\x -> Some(x*x))) &&
             (prop_monad_right (Some 3)) &&
             (prop_monad_assoc (Some 3) (\x -> Some(x*x)) (\x -> Some(2*x))) &&
             (prop_mzero_left (Some 3)) &&
             (prop_mzero_right (Some 3)) &&
             (prop_mzero_assoc (Some 1) (Some 2) (Some 3)) &&
             (prop_zero_bind_l (\x -> Some(x*x))) &&
             (prop_zero_bind_r (Some 2)) &&
             (prop_plus_dist_r (Some 3) (\x -> Some(x*x)) (\x -> Some(2*x)))





----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- | Class of service monads that generate unique Integer numbers.
class Monad m => MonadUnique m where
  unique :: m Integer

newtype Unique a = Unique Undefined

instance Monad Unique where
  return = undefined
  (>>=)  = undefined

instance MonadUnique Unique where
  unique = undefined

runUnique :: Unique a -> a
runUnique = undefined

-- Tests for Exercise 3 ----------------------------------------------

-- | @uniques n@ generates a list of @n@ different @Integer@s.
uniques :: Int -> [Integer]
uniques n = runUnique $ forM [1..n] $ const unique

-- | @allDifferent l@ checks that list @l@ does not contain duplicates.
--   O(n log n), but not implemented efficiently.
allDifferent :: (Eq a, Ord a) => [a] -> Bool
allDifferent l = l == Set.toList (Set.fromList l)

-- | Test that numbers generated by @unique@ are in fact unique.
prop_unique = forAll (choose (1,100)) $ \ n ->
  allDifferent (uniques n)

----------------------------------------------------------------------
-- Exercise 4  Output monad
----------------------------------------------------------------------

-- Implement a monad that lets you pay for computation

-- | Abstract monad for accumulating costs.

class Monad m => MonadCost m where
  pay :: Integer -> m ()

newtype Cost a = C(Integer -> (a, Integer))

instance Monad Cost where
  return a = C(\n -> (a, n))
  m >>= k  = C $ \n -> let (a, n') = runCost m n in runCost (k a) n'

instance MonadCost Cost where
  pay m = C(\n -> ((), n + m))

runCost :: Cost a -> Integer -> (a, Integer)
runCost (C f) n = f n

-- | @revAppSum xs acc@ reverses @xs@ onto @acc@ and 'pay's amount @x@
--   for each element @x@ of the list @xs@ that is touched.
revAppSum :: MonadCost m => [Integer] -> [Integer] -> m [Integer]
revAppSum [] acc = return acc
revAppSum (x:xs) acc = pay x >> revAppSum xs (x:acc)

-- | @reverseSum xs@ returns @(reverse xs, sum xs)@ which is computed
--   by a call to 'revAppSum'.
reverseSum :: [Integer] -> ([Integer], Integer)
reverseSum xs = runCost (revAppSum xs []) 0

testOutput = reverseSum [1,2,3,4,5]

-- Tests for Exercise 4 ----------------------------------------------

prop_reverseSum xs = reverseSum xs == (reverse xs, sum xs)

----------------------------------------------------------------------
-- Haskell footer
----------------------------------------------------------------------

-- | Runs all tests starting with "prop_" in this file.
runTests = $quickCheckAll
