module Main where 

import Data.Array.Unboxed
import Data.Set (empty, insert, deleteFindMin, singleton, member)

main = do
  input <- map (map (read . pure)) . lines <$> readFile "15.in"
  let w = length (head input); h = length input
  let cave = listArray ((0, 0), (h - 1, w - 1)) $ concat input :: Array (Int, Int) Int
  let big = array ((0, 0), (h * 5 - 1, w * 5 - 1))
        [((y + h * row, x + w * col), (r - 1 + row + col) `mod` 9 + 1)
        | ((y, x), r) <- assocs cave, row <- [0..4], col <- [0..4]] :: Array (Int, Int) Int
  mapM_ (print . \x -> search (x !) (bounds x) empty (singleton (0, (0, 0)))) [cave, big]

search cave b visited fringe
  | i == snd b       = risk
  | member i visited = search cave b visited next
  | otherwise        = search cave b (insert i visited) add2Fringe 
    where
      ((risk, i), next) = deleteFindMin fringe
      add2Fringe = foldr insert next $ map (\x -> (risk + cave x, x)) $ adj b i

adj ((a, b), (c, d)) (y, x) =
  filter (\(e, f) -> e >= a && e <= c && f >= b && f <= d)
  [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]
