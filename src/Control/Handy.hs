module Control.Handy
    ( -- re-exported definitions
      (>>>), (<<<)
    , first, second
    , guard
    , on
    , (<$>), (<*>)
    , (<>)
    , mconcat
    , Endo(..)
    , isJust, isNothing
    , fromMaybe
      -- new definitions
    , count
    , countNot
    , modArg
    , modRes
    , (.:)
    , concatMapM
    , xor
    )
where

import qualified Data.Traversable as T

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Function
import Data.Maybe
import Data.Monoid

-- | count how many elements in a list meets the condition
count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

-- | count how many elements in a list does not meet the condition
countNot :: (a -> Bool) -> [a] -> Int
countNot = (modArg . modRes) not count

-- | semantic editor combinators: argument modifier
modArg :: (a -> b) -> (b -> c) -> a -> c
modArg = flip (.)

-- | semantic editor combinators: result modifier
modRes :: (a -> b) -> (c -> a) -> c -> b
modRes = (.)

-- | semantic editor combinators: binary function result modifier
(.:) :: (a -> b) -> (c -> d -> a) -> c -> d -> b
(.:) = modRes . modRes

-- | monadic "concatMap"
concatMapM :: (Monad m,T.Traversable t,Monad t) =>
              (a -> m (t b)) -> (t a -> m (t b))
concatMapM = liftM join .: T.mapM

-- | boolean exclusive or
xor :: Bool -> Bool -> Bool
xor = (/=)
