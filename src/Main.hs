module Main where

import Control.Monad (forever)
import qualified Data.List as L
import qualified Data.List.Split as S
import qualified Data.Map as M

data Coord = Coord Int Int deriving(Ord, Eq, Show)
data Cell = Dead | Alive deriving(Ord, Eq)
data Grid = Grid Int (M.Map Coord Cell) deriving(Ord, Eq)

instance Show Cell where
  show Dead = " □ "
  show _ = " ■ "

instance Show Grid where
  show (Grid s m) = L.intercalate "\n" $ map renderLine ls where
    ls = S.chunksOf s (M.elems m)
    renderLine l = L.concat $ L.map show l

nextTick :: Grid -> Grid
nextTick = id

runGame :: Grid -> IO ()
runGame grid = forever $ do
  putStrLn $ show grid
  putStrLn "\n> Press enter to proceed"
  _ <- getLine
  return (nextTick grid) >>= runGame

makeGrid :: Int -> Grid
makeGrid s = Grid
  s
  (M.fromList [ (Coord x y, Dead) | x <- [0 .. (s - 1)], y <- [0 .. (s - 1)] ])

main :: IO ()
main = runGame $ (makeGrid 10)