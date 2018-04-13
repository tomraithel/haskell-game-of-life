module Main where

import Control.Monad (forever)
import qualified Data.Map as M

data Coord = Coord Int Int deriving(Ord, Eq, Show)
data Cell = Dead | Alive deriving(Ord, Eq, Show)
data Grid = Grid (M.Map Coord Cell) deriving(Ord, Eq, Show)

nextTick :: Grid -> Grid
nextTick = id

runGame :: Grid -> IO ()
runGame grid = forever $ do
  putStrLn $ show grid
  putStrLn "Press enter to proceed"
  _ <- getLine
  return (nextTick grid) >>= runGame

main :: IO ()
main = runGame $ Grid (M.fromList [
    (Coord 0 0, Dead),
    (Coord 0 1, Dead),
    (Coord 1 0, Dead),
    (Coord 1 1, Dead)
  ]);
