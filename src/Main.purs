module Main where

import Prelude

import Data.Newtype (class Newtype, unwrap)
import Data.Array as Array
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.MonadZero (guard)
import Data.Traversable (traverse)
import Data.Foldable (traverse_)
import Data.List.Lazy (List)
import Data.List.Lazy as List
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Int as Int
import Partial.Unsafe (unsafeCrashWith)
import Node.Process as Process

-- The value of a number on a clock puzzle.
newtype Value = Value Int

derive newtype instance eqValue :: Eq Value
derive newtype instance showValue :: Show Value

-- The position of a number on a clock puzzle.
newtype Position = Position Int

derive newtype instance eqPosition :: Eq Position
derive newtype instance showPosition :: Show Position

newtype Puzzle = Puzzle (List Value)

derive newtype instance eqPuzzle :: Eq Puzzle
derive newtype instance showPuzzle :: Show Puzzle

-- A path through a puzzle; built up in reverse order.
newtype Path = Path (List Position)

derive newtype instance eqPath :: Eq Path
derive newtype instance showPath :: Show Path
derive instance newtypePath :: Newtype Path _

emptyPath :: Path
emptyPath = Path mempty

consPath :: Position -> Path -> Path
consPath p (Path ps) = Path (List.cons p ps)

pathLength :: Path -> Int
pathLength (Path ps) = List.length ps

pathToList :: Path -> List Position
pathToList (Path ps) = List.reverse ps

notVisited :: Position -> Path -> Boolean
notVisited pos (Path ps) =
  List.notElem pos ps

slots :: Puzzle -> Int
slots (Puzzle p) = List.length p

lookup :: Position -> Puzzle -> Value
lookup (Position p) (Puzzle vs) =
  case List.index vs p of
    Just i -> i
    _ -> unsafeCrashWith ("position out of range: " <> show p)

advance :: Int -> Int -> Position -> Position
advance n m (Position i) =
  Position ((i + m) `rem` n)

rem :: Int -> Int -> Int
rem v n =
  ((v `mod` n) + n) `mod` n

accessibleFrom :: Position -> Puzzle -> List Position
accessibleFrom pos puzzle =
  case lookup pos puzzle of
    Value v ->
      let
        a = advance (slots puzzle)
      in
        List.fromFoldable [a v pos, a (-v) pos]

-- Generate a list of hamiltonian paths (i.e. solutions) of the puzzle lazily.
solve :: Puzzle -> List Path
solve puzzle = map Position (List.range 0 (n-1)) >>= go emptyPath
  where
  go path pos = do
    let newPath = consPath pos path
    if isComplete path
      then pure newPath
      else do
        next <- accessibleFrom pos puzzle
        guard (notVisited next path)
        go newPath next

  isComplete = (_ == (n - 1)) <<< pathLength
  n = slots puzzle

main = do
  args <- map (List.drop 2 <<< List.fromFoldable) Process.argv
  case traverse Int.fromString args of
    Just args' -> do
      let p = Puzzle (map Value args')
      log "looking for solutions..."
      traverse_ (log <<< show <<< Array.fromFoldable <<< pathToList) (solve p)
    Nothing ->
      log ("invalid input: " <> show args)
