module Main where

import Algorithm.Search
import Data.List.Split
import Data.Char
import Data.Ix
import Data.Functor
import Data.Bifunctor
import qualified Data.Map as Map

type Location = (Int, Int)
type Cost = Int
type Grid = Map.Map Location Cost

bounds = ((0,0), (99,99))
bounds2 = ((0,0), (499, 499))

neighbors :: Location -> [Location]
neighbors (r,c) = filter (inRange bounds) $ map (bimap (+r) (+c)) [ (-1,0), (1,0), (0,-1), (0,1) ]
neighbors2 (r,c) = filter (inRange bounds2) $ map (bimap (+r) (+c)) [ (-1,0), (1,0), (0,-1), (0,1) ]

input :: IO Grid
input = readFile "input" <&> Map.fromList . zip (range bounds) . map digitToInt . concat . lines

part1 grid = dijkstra neighbors cost' end start
  where
    cost i = grid Map.! i
    cost' _ i = cost i
    end = (== snd bounds)
    start = (0,0)

part2 grid = dijkstra neighbors2 cost' end start
  where
    start = (0,0)
    end = (== snd bounds2)
    mx = 1 + fst (snd bounds)
    idx (r,c) = (r`mod`mx, c`mod`mx)
    dist (r,c) = r`div`mx + c`div`mx
    cost' _ i = cost i
    cost i = let c = grid Map.! idx i + dist i in
                 if c > 9
                    then c - 9
                    else c

main :: IO ()
main = do
  grid <- input
  print $ part1 grid
  print $ part2 grid
