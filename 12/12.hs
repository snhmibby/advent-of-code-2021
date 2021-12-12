module Main where

import Control.Monad
import Control.Monad.Trans.Writer
import Data.Char
import Data.Functor
import Data.List.Split
import Data.Maybe
import Data.Map.Strict ((!))
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

mkGraph = foldr addEdge Map.empty
  where
    addEdge (v1,v2) = Map.insertWith (++) v1 [v2] . Map.insertWith (++) v2 [v1]

input :: IO Graph
input = readFile "./input" <&> mkGraph . map readEdge . lines

addNode :: Seen -> Node -> Seen
addNode s n =
  if big n
  then s
  else Set.insert n s

walk :: Graph -> Seen -> Node -> Path -> Writer [Path] ()
walk g s n p
  | n == "end"     = tell [p]
  | Set.member n s = return ()
  | otherwise      = forM_ (g!n) $ \v -> walk g (addNode s n) v (n:p)

paths g = execWriter $ walk g Set.empty "start" []

main = input >>= print . length . paths
