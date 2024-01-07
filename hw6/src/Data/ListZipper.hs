-- | This module defines 'ListZipper' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.ListZipper
  (
  -- * Types
    ListZipper (..)

  -- * Functions
  , lWrite
  , toList
  , lLeft
  , lRight
  , lGenerator
  ) where

import Control.Comonad (Comonad (..))

-- | List zipper datatype.
data ListZipper a = LZ [a] a [a]

-- Implementation from lecture 13
-- | List zipper is a functor.
instance Functor ListZipper where
  fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

-- Implementation from lecture 13
-- | Iterate skipping a head.
iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

-- Implementation from lecture 13
-- | List zipper generator. Takes two functions and a focus.
lGenerator :: (a -> a)     -- ^ The left generator
           -> (a -> a)     -- ^ The right generator
           -> a            -- ^ The focus
           -> ListZipper a -- ^ The resulting list zipper
lGenerator f g x = LZ (iterateTail f x) x (iterateTail g x)

-- Implementation from lecture 13
-- | Moves the focus to the left.
lLeft :: ListZipper a -> ListZipper a
lLeft (LZ (l : ls) c rs) = LZ ls l (c : rs)
lLeft lz                 = lz

-- Implementation from lecture 13
-- | Moves the focus to the right.
lRight :: ListZipper a -> ListZipper a
lRight (LZ ls c (r : rs)) = LZ (c : ls) r rs
lRight lz                 = lz

-- Implementation from lecture 13
-- | Writes a value into the focus.
lWrite :: a -> ListZipper a -> ListZipper a
lWrite x (LZ ls _ rs) = LZ ls x rs

-- Implementation from lecture 13
-- | Converts a list zipper to a list of given distance from the focus.
toList :: ListZipper a -> Int -> [a]
toList (LZ ls x rs) n = reverse (take n ls) ++ [x] ++ take n rs

-- | List zipper is a comonad.
instance Comonad ListZipper where
  -- Implementation from lecture 13
  extract (LZ _ x _) = x

  -- Implementation from lecture 13
  duplicate = lGenerator lLeft lRight
