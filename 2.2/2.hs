module Main where

import Data.Functor

type Command = (String, Integer)

data Location
  = Location
  { depth :: Integer
  , pos   :: Integer
  , aim   :: Integer
  } deriving (Eq, Show)

part2 :: [Command] -> Location
part2 = foldl move (Location 0 0 0)
  where
    move l (cmd, val) = case cmd of
      "forward" -> l { pos = pos l + val, depth = depth l + val * aim l  }
      "down"    -> l { aim = aim l + val }
      "up"      -> l { aim = aim l - val }

part1 :: [Command] -> Location
part1 = foldl move (Location 0 0 0)
  where
    move l (cmd, val) = case cmd of
      "forward" -> l { pos = pos l + val  }
      "down"    -> l { depth = depth l + val }
      "up"      -> l { depth = depth l - val }

input :: IO [Command]
input = readFile "input.txt" <&> lines <&> map readCommand

readCommand s = (cmd, read val)
  where [cmd, val] = words s

main = input <&> part1 >>= \l -> print (pos l * depth l)
main2 = input <&> part2 >>= \l -> print (pos l * depth l)
