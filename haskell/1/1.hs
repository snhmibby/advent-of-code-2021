module Main where

import Data.Functor

-- slice windows
part2 :: [Integer] -> [Integer]
part2 [x,x1,x2] = [x+x1+x2]
part2 (x:x1:x2:xs) = (x+x1+x2) : part2 (x1:x2:xs)

part1 :: [Integer] -> Int
part1 ls = length $ filter inc $ zip ls (drop 1 ls)
  where inc (a,b) = a < b

input :: IO [Integer]
input = readFile "input.txt" <&> lines <&> map read

main1 = input >>= print . part1
main2 = input >>= print . part1 . part2
main = main2
