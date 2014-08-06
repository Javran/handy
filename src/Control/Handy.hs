module Control.Handy
    ( -- re-exported definitions
      (>>>), (<<<)
    , first, second
    , guard
    , on
    , (<$>), (<*>), (*>), (<*)
    , (<>)
    , mconcat
    , Endo(..)
    , Sum(..)
    , Product(..)
    , isJust, isNothing
    , fromMaybe
    , intercalate
    , intersperse
    , isSuffixOf
    , isPrefixOf
    , nub
    , F.foldMap
    , splitOn
    , splitOneOf
    , splitWhen
    , endBy
    , chunksOf
    , Down(..)
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
    , theUniverse
    , predMerge
    , allTrueFor
    , anyTrueFor
    , takeLength
    , ifThenElse
    , scanM
    , notNull
    , lengthExceeds
    , lengthAtLeast
    , lengthIs
    , listLengthCmp
    , equalLength
    , compareLength
    , unDown
    )
where

import qualified Data.Traversable as T

import Control.Applicative
import Control.Arrow
import Control.Monad
import qualified Data.Foldable as F
import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Monoid
import Data.Ord

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

-- | universe for types that possess both 'Enum' and `Bounded`
theUniverse :: (Enum a, Bounded a) => [a]
theUniverse = [minBound, maxBound]

-- | merge predicates together, use the strategy
--   given by its first argument
predMerge :: ([Bool] -> Bool) -> [a -> Bool] -> a -> Bool
predMerge mg ps x = mg (map ($ x) ps)

-- | given a list of predicates,
--   test if all the predicates hold
allTrueFor :: [a -> Bool] -> a -> Bool
allTrueFor = predMerge and

-- | given a list of predicates,
--   test if any of these predicates holds
anyTrueFor :: [a -> Bool] -> a -> Bool
anyTrueFor = predMerge or

-- | `takeLength` is the same as `take . length`
-- but without involving `length` function
takeLength :: [a] -> [b] -> [b]
takeLength = zipWith (curry snd)

-- | if expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse True x _ = x
ifThenElse False _ y = y

-- | 'scanM' is similar to 'foldM', but returns a list of successive
-- reduced monadic values from the left:
scanM :: Monad m => (a -> b -> m a) -> a -> [b] -> m [a]
{-
scanM _ seed [] = return [seed]
scanM go seed (x:xs) = do
    seed' <- go seed x
    results <- scanM go seed' xs
    return $ seed : result
-}
scanM go seed ls = liftM (seed:) $
   case ls of
     [] -> return []
     (x:xs) -> do seed' <- go seed x
                  scanM go seed' xs

-- copied from: http://haddocks.fpcomplete.com/fp/7.4.2/20130829-168/ghc/src/Util.html
atLength :: ([a] -> b)
         -> (Int -> b)
         -> [a]
         -> Int
         -> b
atLength atLenPred atEndPred ls n
  | n < 0     = atEndPred n
  | otherwise = go n ls
  where
    go n' [] = atEndPred n'
    go 0 ls' = atLenPred ls'
    go n' (_:xs) = go (n'-1) xs

-- | the list is not null?
notNull :: [a] -> Bool
notNull = not . null

-- | length of this list is greater than ...
lengthExceeds :: [a] -> Int -> Bool
-- ^ > (lengthExceeds xs n) = (length xs > n)
lengthExceeds = atLength notNull (const False)

-- | length of this list is at least ...
lengthAtLeast :: [a] -> Int -> Bool
lengthAtLeast = atLength notNull (== 0)

-- | length of this list is exactly ...
lengthIs :: [a] -> Int -> Bool
lengthIs = atLength null (==0)

-- | compare the length of this list with a number
listLengthCmp :: [a] -> Int -> Ordering
listLengthCmp = atLength atLen atEnd
 where
  atEnd 0      = EQ
  atEnd x
   | x > 0     = LT -- not yet seen 'n' elts, so list length is < n.
   | otherwise = GT

  atLen []     = EQ
  atLen _      = GT

-- | the length of two lists are equal?
equalLength :: [a] -> [b] -> Bool
equalLength = (== EQ) .: compareLength

-- | compare the length of two lists
compareLength :: [a] -> [b] -> Ordering
compareLength []     []     = EQ
compareLength (_:xs) (_:ys) = compareLength xs ys
compareLength []     _      = LT
compareLength _      []     = GT

unDown :: Down a -> a
unDown (Down v) = v
