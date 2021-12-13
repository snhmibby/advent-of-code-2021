module Main where

import qualified Data.IntSet as Set
import Data.List
import Data.Functor
import Data.List.Split

type Numbers = Set.IntSet
type Row = [Int]
type Card = [Row]

input :: IO ([Int],[Card])
input = do
  (nr':cs) <- readFile "input" <&> lines
  let nr = map read $ splitOn "," nr'
  return (nr, readCards cs)

-- 1 empty line, 5 lines with 5 ints, 0 error checking
readCards :: [String] -> [Card]
readCards [] = []
readCards cs = readCard (tail $ take 6 cs) : readCards (drop 6 cs)

readCard :: [String] -> Card
readCard = map readRow
readRow = map read . words

rows,cols :: Card -> [[Int]]
rows = id
cols = transpose

bingo n c = any (all (`Set.member` n)) $ rows c ++ cols c

score :: Card -> Numbers -> Int -> Int
score c n x = x * sum (filter (`Set.notMember` n) $ concat c)

-- loop tries the number-list 1-by-1 until the card has a bingo
-- returns: (round, score)
loop :: Int -> [Int] -> Numbers -> Card -> (Int, Int)
loop _ [] _ _ = error "oops"
loop round (x:xs) n' c
  = let n = x `Set.insert` n' in
    if bingo n c
    then (round, score c n x)
    else loop (round+1) xs n c

main = do
  (nrs, cards) <- input
  let scores = sortBy (\a b -> compare (fst a) (fst b)) $ map (loop 0 nrs Set.empty) cards
  print $ snd $ head scores
  print $ snd $ last scores
