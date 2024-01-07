{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Control.Exception (Exception, throwIO)
import Data.Foldable (for_)
import Data.Grid (toList)
import HW6.T3
import Options.Applicative

-- | Command line arguments.
data Args = Args
  {
    seed       :: Int -- ^ Seed for random number generation in the simulation
  , prob       :: Double -- ^ Value between 0 and 1.0 meaning chance of a person to be infected after a contact
  , incub      :: Int -- ^ Incubation period duration
  , ill        :: Int -- ^ Illness duration
  , immun      :: Int -- ^ Immunity duration
  , gridSize   :: Int -- ^ Grid render distance calculated from the start cell
  , iterations :: Int -- ^ The number of simulation iterations
  }
  deriving Show

-- | Exception thrown when the arguments are invalid.
newtype IllegalArgumentException =
  IllegalArgumentException String
  deriving (Show)

instance Exception IllegalArgumentException

-- | Parser for command line arguments.
argParser :: Parser Args
argParser = Args
      <$> option auto
          ( long "seed"
         <> showDefault
         <> value 0
         <> metavar "INT"
         <> help "Seed for random number generation in the simulation" )
      <*> option auto
          ( long "prob"
         <> help "Value between 0 and 1.0 meaning chance of a person to be infected after a contact"
         <> showDefault
         <> value 0.5
         <> metavar "DOUBLE" )
      <*> option auto
          ( long "incub"
         <> help "Incubation period duration"
         <> showDefault
         <> value 3
         <> metavar "INT" )
      <*> option auto
          ( long "ill"
         <> help "Illness duration"
         <> showDefault
         <> value 7
         <> metavar "INT" )
      <*> option auto
          ( long "immun"
         <> help "Immunity duration"
         <> showDefault
         <> value 14
         <> metavar "INT" )
      <*> option auto
          ( long "grid-size"
         <> help "Grid render distance calculated from the start cell"
         <> showDefault
         <> value 20
         <> metavar "INT" )
      <*> option auto
          ( long "iterations"
         <> help "The number of simulation iterations"
         <> showDefault
         <> value 40
         <> metavar "INT" )

-- | Program description and help.
opts :: ParserInfo Args
opts = info (argParser <**> helper)
  ( fullDesc
  <> progDesc "Simulates the Covid-19 infection on a 2-dimensional grid using the power of comonads."
  <> header "Comonad-19" )

-- | Validates the command line arguments and throws an IO exception if they are invalid.
validateArgs :: Args -> IO ()
validateArgs args
  | prob args < 0 || prob args > 1 = throwIO $ IllegalArgumentException "prob is out of bounds [0, 1]"
  | incub args <= 0 = throwIO $ IllegalArgumentException "incub is non-positive"
  | ill args <= 0 = throwIO $ IllegalArgumentException "ill is non-positive"
  | immun args <= 0 = throwIO $ IllegalArgumentException "immun is non-positive"
  | gridSize args <= 0 = throwIO $ IllegalArgumentException "grid-size is non-positive"
  | iterations args <= 0 = throwIO $ IllegalArgumentException "iterations is non-positive"
  | otherwise = return ()

main :: IO ()
main = execParser opts >>= \args -> do
  validateArgs args

  let simulation = simulate (seed args) Config {
    probability = prob args,
    incubationPeriod = incub args,
    illnessDuration = ill args,
    immunityDuration = immun args}

  for_ (take (iterations args) simulation) \grid -> do
    for_ (toList grid $ gridSize args) (putStrLn . map cellToChar)
    putStr "\n\n\n"

  where
    cellToChar :: Cell -> Char
    cellToChar cell = case cellState cell of
      Healthy    -> '_'
      Infected _ -> 'i'
      Ill _      -> '#'
      Immune _   -> '@'
