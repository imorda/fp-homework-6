-- | This module defines 'Grid' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.Grid
  (
  -- * Types
    Grid (..)

  -- * Functions
  , gUp
  , gDown
  , gLeft
  , gRight
  , gWrite
  , toList
  ) where

import Control.Comonad (Comonad (..))

import Data.ListZipper (ListZipper (..), lGenerator, lLeft, lRight, lWrite)
import qualified Data.ListZipper as LZ

-- | Grid datatype.
newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

-- Implementation from lecture 13
-- | Moves the focus up.
gUp :: Grid a -> Grid a
gUp (Grid g) = Grid (lLeft g)

-- Implementation from lecture 13
-- | Moves the focus down.
gDown :: Grid a -> Grid a
gDown (Grid g) = Grid (lRight g)

-- Implementation from lecture 13
-- | Moves the focus left.
gLeft :: Grid a -> Grid a
gLeft (Grid g) = Grid (fmap lLeft g)

-- Implementation from lecture 13
-- | Moves the focus right.
gRight :: Grid a -> Grid a
gRight (Grid g) = Grid (fmap lRight g)

-- Implementation from lecture 13
-- | Writes a value into the focus.
gWrite :: a -> Grid a -> Grid a
gWrite x (Grid g) = Grid $ lWrite newLine g
  where
    oldLine = extract g
    newLine = lWrite x oldLine

-- | Converts a grid to a list of given distance from the focus.
toList :: Grid a -> Int -> [[a]]
toList (Grid g) n = fmap (`LZ.toList` n) g `LZ.toList` n

-- Implementation from lecture 13
-- | Makes a list zipper from a grid that moves the focus horizontally along the grid.
gHorizontal :: Grid a -> ListZipper (Grid a)
gHorizontal = lGenerator gLeft gRight

-- Implementation from lecture 13
-- | Makes a list zipper from a grid that moves the focus vertically along the grid.
gVertical :: Grid a -> ListZipper (Grid a)
gVertical = lGenerator gUp gDown

-- | Grid is a functor.
instance Functor Grid where
  fmap f (Grid g) = Grid $ fmap (fmap f) g

-- | Grid is a comonad.
instance Comonad Grid where
  -- Implementation from lecture 13
  extract = extract . extract . unGrid

  -- Implementation from lecture 13
  duplicate = Grid . fmap gHorizontal . gVertical
