module Main where

import Data.Bifunctor
import Data.Functor
import Data.List
import Data.List.Split
import qualified Data.Map as Map

type Pattern = (Char, Char)
type Count = Map.Map Char Integer
type Freq = Map.Map Pattern Integer
type Rules = Map.Map Pattern String

input :: IO (Freq, Rules)
input = do 
  template:_:rules' <- readFile "./input" <&> lines
  let freq = str2Freq template
  let rules = Map.fromList $ map (tuple . splitOn " -> ") rules'
  return (freq, rules)
    where
      tuple [[a,as],b] = ((a,as),b)

str2Freq :: String -> Freq
str2Freq s = Map.fromListWith (+) $ zip (zip s $ tail s) (repeat 1)

step :: Rules -> Freq -> Freq
step rules = Map.fromListWith (+) . concatMap addFreq . Map.assocs
  where
    addFreq (k@(a,b), n) = case Map.lookup k rules of
      Nothing  -> [(k,n)]
      Just [c] -> [((a,c), n), ((c,b), n)]

step' r = iterate (step r)

count :: Freq -> Count
count = Map.map (\x -> x `div` 2 + x `mod` 2) . Map.fromListWith (+) . concatMap split . Map.assocs
  where
    split ((a,b), n) = [(a,n), (b,n)]

count' :: Freq -> (Integer, Integer)
count' f = (maximum c, minimum c)
  where
    c = Map.elems $ count f

main :: IO ()
main = do
  (s, rules) <- input
  let part2 = step' rules s !! 40
  let (mx, mn) = count' part2
  print $ mx - mn
