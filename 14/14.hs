module Main where

import Data.Bifunctor
import Data.Functor
import Data.List.Split
import Data.List
import qualified Data.Map as Map

type Count = Map.Map Char Integer
type Freq = Map.Map String Integer
type Rules = Map.Map String String
 
input :: IO (Freq, Rules)
input = do 
  template:_:rules' <- readFile "./input" <&> lines
  let freq = str2Freq template
  let rules = Map.fromList $ map (tuple . splitOn " -> ") rules'
  return (freq, rules)
    where
      tuple [a,b] = (a,b)

str2Freq :: String -> Freq
str2Freq s = Map.fromListWith (+) $ zip (zipWith (\a b -> [a,b]) s (tail s)) (repeat 1)

step :: Rules -> Freq -> Freq
step rules = Map.fromListWith (+) . concatMap addFreq . Map.assocs
  where
    addFreq :: (String, Integer) -> [(String, Integer)]
    addFreq (k@[a,b], n) = case Map.lookup k rules of
      Nothing  -> [(k,n)]
      Just [c] -> [([a,c], n), ([c,b], n)]

step' r = iterate (step r)

count :: Freq -> Count
count = Map.fromListWith (+) . map divCount . concatMap split . Map.assocs
  where
    divCount = second $ \x -> (x `div` 2) + (x `mod` 2)
    split ([a,b], n) = [(a,n), (b,n)]

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

