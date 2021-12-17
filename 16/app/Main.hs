module Main where

import Control.Monad
import Control.Monad.State.Lazy
import Data.Bits
import Data.Char
import Data.Functor
import Data.List
import Debug.Trace

main :: IO ()
main = do
  pkts <- readFile "input" <&> parse packets . mkbits . concat . lines
  print $ versionSum pkts -- part 1
  print $ val pkts -- part 2

data Packet
  = Literal { version, typeid, value :: Int }
  | Operator { version, typeid :: Int, operands :: [Packet] }
  deriving (Show)

type Parser = State Bit -- XXX can't fail

type Bit = [Int]

mkbits = concatMap hexDigit 
  where
    hexDigit :: Char -> Bit
    hexDigit c = case toUpper c of
      '0' -> [0,0,0,0]
      '1' -> [0,0,0,1]
      '2' -> [0,0,1,0]
      '3' -> [0,0,1,1]
      '4' -> [0,1,0,0]
      '5' -> [0,1,0,1]
      '6' -> [0,1,1,0]
      '7' -> [0,1,1,1]
      '8' -> [1,0,0,0]
      '9' -> [1,0,0,1]
      'A' -> [1,0,1,0]
      'B' -> [1,0,1,1]
      'C' -> [1,1,0,0]
      'D' -> [1,1,0,1]
      'E' -> [1,1,1,0]
      'F' -> [1,1,1,1]
      _ -> error "Weird bits!"

parse :: Parser a -> Bit -> a
parse = evalState

split :: Int -> Parser Bit
split n = do
  l <- get
  let (ns,rest) = splitAt n l
  put rest
  return ns

number :: Int -> Parser Int
number n = foldl' (\x b -> 2*x + b) 0 <$> split n

fin :: Parser Bool
fin = null <$> get

packets :: Parser [Packet]
packets = do
  f <- fin
  if f
  then return []
  else do
    p <- packet
    ps <- packets
    return (p:ps)

packet :: Parser Packet
packet = do
  version <- number 3
  typeid  <- number 3
  if typeid == 4
    then Literal  version typeid <$> literal 0
    else Operator version typeid <$> operator

literal :: Int -> Parser Int
literal v = do
    continue <- number 1
    val <- number 4 <&> (+ v*16)
    if continue == 0
    then return val
    else literal val

operator :: Parser [Packet]
operator = do
  length_id <- number 1
  if length_id == 0
    then do
      len <- number 15
      split len <&> parse packets
    else do
      pkts <- number 11
      forM [1..pkts] $ const packet

-- part 1
versionSum :: [Packet] -> Int
versionSum = sum . map versionSum'

versionSum' :: Packet -> Int
versionSum' (Literal v _ _) = v
versionSum' (Operator v _ op) = v + versionSum op

-- part 2
val :: [Packet] -> [Int]
val = map val'

val' :: Packet -> Int
val' (Literal _ _ v) = v
val' (Operator _ id os) = let f = op id in f (val os)

op :: Int -> ([Int] -> Int)
op id = case lookup id ops of
  Nothing -> error "bad operator"
  Just f -> f

ops :: [(Int, [Int] -> Int)]
ops = 
  [ (0, sum)
  , (1, product)
  , (2, minimum)
  , (3, maximum)
  , (5, binop (>))
  , (6, binop (<))
  , (7, binop (==))
  ]

binop :: (Int -> Int -> Bool) -> [Int] -> Int
binop f [l,r] = if l `f` r then 1 else 0
binop _ _ = error "bad"
