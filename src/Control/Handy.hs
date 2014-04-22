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
    , intercalate
    , intersperse
      -- new definitions
    , count
    , countNot
    , modArg
    , modRes
    , (.:)
    , concatMapM
    , xor
    , (<&>)
    , toChurch
    , churchSucc
    , churchs
    )
where

import qualified Data.Traversable as T

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Function
import Data.List
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
              (a -> m (t b)) -> t a -> m (t b)
concatMapM = liftM join .: T.mapM

-- | boolean exclusive or
xor :: Bool -> Bool -> Bool
xor = (/=)

-- | @flip fmap@ in disguise
(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as

type Church a = (a -> a) -> a -> a

-- | non-negative numbers to Church numbers
toChurch :: Int -> Church a
toChurch = toChurchAux id
    where
        toChurchAux acc n f
            | n == 0    = acc
            | odd n     = toChurchAux (f. acc) (n-1) f
            | otherwise = toChurchAux acc (n `div` 2) (f.f)

-- | succ for church numbers
churchSucc :: Church a -> Church a
churchSucc n f x = f (n f x)

-- | an infinite list of church numbers
churchs :: [Church a]
churchs = id : map churchSucc churchs
