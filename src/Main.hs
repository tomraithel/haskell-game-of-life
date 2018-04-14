module Main where

import Control.Monad (forever)
import qualified Data.List as L

data Coord = Coord Int Int
data Cell = Dead | Alive deriving(Ord, Eq)
data Grid = Grid [[Cell]] deriving(Ord, Eq)

instance Show Cell where
  show Dead = " □ "
  show _ = " ■ "

instance Show Grid where
  show (Grid css) = L.intercalate "\n" $ L.map renderLine css where
    renderLine cs = L.concat $ L.map show cs


gridSize :: Grid -> Int
gridSize (Grid ls) = L.length ls

getCell :: Coord -> Grid -> Cell
getCell (Coord x y) (Grid ls) = ls !! y !! x

setCell :: Coord -> Cell -> Grid -> Grid
setCell (Coord x y) cell (Grid css) = (Grid newRows)
 where
  newRows = L.take y css ++ newRow : drop (y + 1) css
  newRow  = L.take x cs ++ cell : drop (x + 1) cs
  cs      = css !! y

isAlive :: Coord -> Grid -> Bool
isAlive c g = case getCell c g of
  Alive -> True
  Dead  -> False

neighbours :: Coord -> Grid -> Int
neighbours (Coord x y) g = L.length $ L.filter id $ L.map
  f
  [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]
 where
  f (ox, oy) = isAlive (Coord (normalize (x - ox)) (normalize (y - oy))) g
  normalize i = i `mod` (gridSize g)

nextGeneration :: Cell -> Int -> Cell
nextGeneration Dead  3 = Alive
nextGeneration Alive 2 = Alive
nextGeneration Alive 3 = Alive
nextGeneration _     _ = Dead

nextTick :: Grid -> Grid
nextTick g@(Grid ls) = Grid (L.map walkLine (withIndex ls))
 where
  walkLine (y, l) = L.map (walkCell y) (withIndex l)
  walkCell y (x, c) = nextGeneration c (neighbours (Coord x y) g)
  withIndex xs = zip [0 .. (L.length xs - 1)] xs

runGame :: Grid -> IO ()
runGame grid = forever $ do
  putStrLn $ show grid
  putStrLn
    $  "\n> Press enter to proceed, CTRL + C to exit\ESC["
    ++ (show (gridSize grid + 3))
    ++ "A"
  _ <- getLine
  return (nextTick grid) >>= runGame

makeGrid :: Int -> Grid
makeGrid s = Grid (L.map (const (L.map (const Dead) l)) l)
  where l = [0 .. (s - 1)]

main :: IO ()
main =
  runGame
    $ ( setCell (Coord 2 5) Alive
      . setCell (Coord 3 5) Alive
      . setCell (Coord 4 5) Alive
      . setCell (Coord 4 6) Alive
      . setCell (Coord 3 7) Alive
      )
        (makeGrid 20)
