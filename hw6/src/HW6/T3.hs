module HW6.T3
  (
  -- * Types
    Config (..)
  , Cell (..)
  , CellState (..)
  , Comonad19Grid

  -- * Functions
  , simulate
  ) where

import Control.Comonad (duplicate, extend, extract)
import Control.Monad (liftM2)
import Data.Foldable (foldl')
import Data.Grid (Grid (..), gDown, gLeft, gRight, gUp, gWrite)
import Data.ListZipper
import System.Random (StdGen, mkStdGen, randomR, split)

-- | Configuration of the simulation.
data Config = Config
  { probability      :: Double -- ^ Probability of infection.
  , incubationPeriod :: Int -- ^ Duration of incubation.
  , illnessDuration  :: Int -- ^ Duration of illness.
  , immunityDuration :: Int -- ^ Duration of immunity after illness.
  } deriving Show

-- | State of a cell.
data CellState
  = Healthy -- ^ Healthy state.
  | Infected Int -- ^ Infected state for some time.
  | Ill Int -- ^ Ill after incubation for some time.
  | Immune Int -- ^ Immune after illness for some time.
  deriving Show

-- | Cell of the grid.
data Cell = Cell
  { cellState :: CellState -- ^ State of the cell.
  , cellRand  :: StdGen -- ^ Random generator for the cell.
  }

-- | Type of the grid.
type Comonad19Grid = Grid Cell

-- | Creates an infinite list of grids using the given configuration.
-- Each element of this list represents one infection simulation step.
--
-- This function may take additional parameters (e.g. initial seed for random).
simulate :: Int -> Config -> [Comonad19Grid]
simulate initialSeed conf = iterate (generation conf) $ initializeInfected initialSeed

  where
    newCell :: StdGen -> Cell
    newCell x = Cell Healthy x

    iterateFold :: (a -> (b, a)) -> a -> [b]
    iterateFold f start = let (next, state) = f start in
      (next: iterateFold f state)

    randomCellGenerator :: StdGen -> (Cell, StdGen)
    randomCellGenerator lastGen = let (a, b) = split lastGen in
      (newCell a, b)

    initializeEmpty :: Int -> Comonad19Grid
    initializeEmpty seed = let (ls, tmpGen) = split $ mkStdGen seed in
      let (x, rs) = split tmpGen in
        Grid $ duplicate $ LZ
          (iterateFold randomCellGenerator ls)
          (newCell x)
          (iterateFold randomCellGenerator rs)

    initializeInfected :: Int -> Comonad19Grid
    initializeInfected seed = let empty = initializeEmpty seed in
      gWrite (Cell (Infected 0) (cellRand $ extract empty)) empty

    incrementOrNextStage :: CellState -> Int -> CellState -> CellState
    incrementOrNextStage curState maxDays nextStage = case curState of
      Infected x -> if x >= maxDays then nextStage else Infected $ x + 1
      Ill x      -> if x >= maxDays then nextStage else Ill $ x + 1
      Immune x   -> if x >= maxDays then nextStage else Immune $ x + 1
      Healthy    -> Healthy

    -- Implementation from lecture 13
    neighbors :: [Grid a -> Grid a]
    neighbors = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
      where
        horizontals = [gLeft, gRight]
        verticals = [gUp, gDown]

    cellInfectProb :: Config -> CellState -> Double
    cellInfectProb config (Infected _) = probability config
    cellInfectProb config (Ill _)      = probability config
    cellInfectProb _ (Immune _)        = 0
    cellInfectProb _ Healthy           = 0

    neighborsInfectProb :: Config -> Comonad19Grid -> Double
    neighborsInfectProb config grid =
      1.0 - foldl' (*) 1.0 (map (\direction ->
        1.0 - cellInfectProb config (cellState $ extract $ direction grid)) neighbors)

    rule :: Config -> Comonad19Grid -> Cell
    rule config grid =case extract grid of
      Cell state@(Infected _) rng ->
        Cell (incrementOrNextStage state (incubationPeriod config) (Ill 0)) rng
      Cell state@(Ill _) rng ->
        Cell (incrementOrNextStage state (illnessDuration config) (Immune 0)) rng
      Cell state@(Immune _) rng ->
        Cell (incrementOrNextStage state (immunityDuration config) Healthy) rng
      Cell Healthy rng -> let (testProb, rng') = randomR (0, 1) rng in
        Cell (if testProb >= neighborsInfectProb config grid then Healthy else Infected 0) rng'

    generation :: Config -> Comonad19Grid -> Comonad19Grid
    generation config = extend (rule config)
