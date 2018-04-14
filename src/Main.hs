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

setCell :: Coord -> Cell -> Grid -> Grid
setCell (Coord x y) cell (Grid css) = (Grid newRows)
 where
  newRows = L.take y css ++ newRow : drop (y + 1) css
  newRow  = L.take x cs ++ cell : drop (x + 1) cs
  cs      = css !! y

nextTick :: Grid -> Grid
nextTick = (setCell (Coord 9 2) Alive) . (setCell (Coord 9 9) Alive)

runGame :: Grid -> IO ()
runGame grid = forever $ do
  putStrLn $ show grid
  putStrLn "\n> Press enter to proceed"
  _ <- getLine
  return (nextTick grid) >>= runGame

makeGrid :: Int -> Grid
makeGrid s = Grid (L.map (const (L.map (const Dead) l)) l)
  where l = [0 .. (s - 1)]

main :: IO ()
main = runGame $ (makeGrid 10)
