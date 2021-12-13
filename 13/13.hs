module Main where

import Control.Monad
import Data.Functor
import Data.List
import Data.List.Split
import qualified Data.Set as Set

type Grid = Set.Set (Int, Int)
data Fold = FoldX Int | FoldY Int deriving (Show, Eq)

input :: IO (Grid, [Fold])
input = do 
  [l, cmd] <- readFile "./input" <&> splitOn [""] . lines
  let s = Set.fromList $ map readLocation l
  let f = map readFold cmd
  return (s,f)
    where
      readLocation s = let [x,y] = map read (splitOn "," s) in (x,y)
      readFold s = case words s of
        [_, _, 'y':'=':ys] -> FoldY (read ys)
        [_, _, 'x':'=':xs] -> FoldX (read xs)

fold :: Grid -> Fold -> Grid
fold g f = case f of
  FoldX at -> Set.map (foldx at) g
  FoldY at -> Set.map (foldy at) g
  where
    foldy at (x,y) = if y > at then (x, at - (y - at)) else (x,y)
    foldx at (x,y) = if x > at then (at - (x - at), y) else (x,y)

bounds :: Grid -> (Int, Int)
bounds g = foldl' (\(x,y) (x',y')-> (max x x', max y y')) (0,0) $ Set.elems g

printGrid :: Grid -> IO ()
printGrid g = do
  let (y,x) = bounds g
  forM_ [0..x] $ \i -> do
    forM_ [0..y] $ \j -> do
      if (j,i) `Set.member` g
         then putStr "#"
         else putStr " "
    putStrLn ""

main :: IO ()
main = do
  (s, cmd) <- input
  let part1 = fold s $ head cmd
  let part2 = foldl' fold s cmd
  printGrid part2
