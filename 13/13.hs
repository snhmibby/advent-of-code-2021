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
      readLocation :: String -> (Int, Int)
      readLocation s = let [x,y] = map read (splitOn "," s) in (x,y)
      readFold :: String -> Fold
      readFold s = case words s of
        [_, _, 'y':'=':ys] -> FoldY (read ys)
        [_, _, 'x':'=':xs] -> FoldX (read xs)
        _                  -> error "bad fold"

fold :: Grid -> Fold -> Grid
fold g (FoldX at) = foldx g at
fold g (FoldY at) = foldy g at

foldy :: Grid -> Int -> Grid
foldy g at = Set.filter ((<=at).snd) $ Set.map mapy g
  where
    mapy (x,y) =
      if y > at
      then (x, at - (y - at))
      else (x,y)

foldx :: Grid -> Int -> Grid
foldx g at = Set.filter ((<=at).fst) $ Set.map mapx g
  where
    mapx (x,y) =
      if x > at
      then (at - (x - at), y)
      else (x,y)

bounds :: Grid -> (Int, Int)
bounds g = foldl' (\(x,y) (x',y')-> (max x x', max y y')) (0,0) $ Set.elems g

printGrid :: Grid -> IO ()
printGrid g = do
  let (y,x) = bounds g
  forM_ [0..x] $ \i -> do
    forM_ [0..y] $ \j -> do
      if (j,i) `Set.member` g
         then putStr "*"
         else putStr " "
    putStrLn ""

main :: IO ()
main = do
  (s, cmd) <- input
  let part1 = fold s $ head cmd
  let part2 = foldl' fold s cmd
  printGrid part2

