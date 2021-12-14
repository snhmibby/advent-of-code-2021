module Main where

import Data.Functor
import Data.List.Split
import Data.List
import qualified Data.Map as Map

type Rules = Map.Map String String
  
input :: IO (String, Rules)
input = do 
  template:_:rules' <- readFile "./input" <&> lines
  let rules = Map.fromList $ map (tuple . splitOn " -> ") rules'
  return (template, rules)
    where
      tuple [a,b] = (a,b)

step :: String -> Rules -> String
step s rules = loop s
  where
    loop :: String -> String
    loop [] = []
    loop [a] = [a]
    loop (a:b:cs)
      = let nn = [a,b] in
        case Map.lookup nn rules of
          Just n  ->  a : n ++ loop (b:cs)
          Nothing -> a : loop (b:cs)

stepN n s r = iterate (`step` r) s !! n

count :: String -> Map.Map Char Int
count = foldl' (\m c -> Map.insertWith (const $ (+)1) c 1 m) Map.empty

maxOf, minOf :: Map.Map Char Int -> Int
minOf = minimum . Map.elems
maxOf = maximum . Map.elems

main :: IO ()
main = do
  (s, rules) <- input
  let polymer = stepN 20 s rules
  let counts = count polymer
  print $ maxOf counts - minOf counts

