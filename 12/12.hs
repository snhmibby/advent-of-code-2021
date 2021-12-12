module Main where

import Control.Monad
import Control.Monad.Trans.Writer
import Data.Maybe
import Data.Functor
import Data.Char
import Data.Array
import Data.List.Split
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Node = String
type Edge = (Node, Node)
type Graph = Map.Map Node [Node]
type Seen = Set.Set Node
type Path = [Node]

small, big :: Node -> Bool
small = not . big
big = isUpper . (!!0)

readEdge :: String -> Edge
readEdge s = case splitOn "-" s of
      [to, from] -> (to, from)
      _ -> error $ "bad edge in input: " ++ s

readNodes :: [Edge] -> Set.Set Node
readNodes = Set.fromList . foldl (\b (t,f)-> t:f:b) []

readNeighbors :: [Edge] -> Node -> [Node]
readNeighbors e n = map neighborInEdge $ filter hasNode e
  where
    neighborInEdge (v1, v2)
      | v1 == n   = v2
      | otherwise = v1
    hasNode (v1, v2) = v1 == n || v2 == n

input :: IO Graph
input = do
  e <- readFile "./input" <&> map readEdge .lines
  let v = readNodes e
  let g = Map.fromSet (readNeighbors e) v
  return g

neighbours :: Graph -> Node -> [Node]
neighbours = (Map.!)

addNode :: Seen -> Node -> Seen
addNode s n =
  if big n
  then s
  else Set.insert n s

walk :: Graph -> Seen -> Node -> Path -> Writer [Path] ()
walk g s n p
  | n == "end"     = tell [p]
  | Set.member n s = return ()
  | otherwise      = forM_ (neighbours g n) $ \v -> walk g (addNode s n) v (n:p)

paths g = execWriter $ walk g Set.empty "start" []

main :: IO ()
main = do
  g <- input
  print $ length $ paths g
