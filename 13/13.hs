module Main where

import Control.Arrow
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
fold g f = Set.map (fold' f) g
  where
    fold' (FoldX x) = first $ flip' x
    fold' (FoldY y) = second $ flip' y
    flip' at n = if n > at then  at - (n-at) else n

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
