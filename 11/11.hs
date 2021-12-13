module Main where

import Control.Monad
import Data.Array.IO
import Data.Functor
import Data.List

type Location = (Int, Int)
type Map = IOArray Location Int

--hardcoded problem size
bounds = ((0,0), (9,9))

next2 :: Location -> [Location]
next2 (r,c) = filter (inRange bounds) neighbours
  where
    neighbours = [ (r+r', c+c') | r' <- [-1, 0, 1], c' <- [-1, 0, 1], not (r' == 0 && c' == 0) ]

char2Digit :: Char -> Int
char2Digit c = read [c]

readMap :: [String] -> IO Map
readMap l = newListArray bounds $ map char2Digit $ concat l

input :: IO [String]
input = readFile "./input" <&> lines

-- step 1: increase all by 1
inc1 :: Map -> IO ()
inc1 m = inc m (range bounds)
inc m b = forM_ b $ \i -> do
  x <- readArray m i
  writeArray m i (x+1)

-- step 2: flash if  == 10
flash :: Map -> IO ()
flash m = getAssocs m >>= flash' . mustFlash 
  where
    mustFlash :: [(Location, Int)] -> [Location]
    mustFlash = map fst . filter ((==10) . snd)
    flash' :: [Location] -> IO ()
    flash' ls = forM_ ls $ \i -> do
      let neigh = next2 i
      inc m neigh
      f <- mustFlash . zip neigh <$> mapM (readArray m) neigh
      flash' f

-- step3: reset all flashed octopii to 0, count them
reset :: Map -> IO Int
reset m = do
  r <- map fst . filter ((>9) . snd) <$> getAssocs m
  forM_ r $ \i -> writeArray m i 0
  return $ length r

-- update map & return the number of flashes
step :: Map -> IO Int
step map = do
  inc1 map
  flash map
  reset map

printMap :: Map -> IO ()
printMap m = do
  forM_ [0..9] $ \i -> do
    forM_ [0..9] $ \j -> do
      x <- readArray m (i,j)
      putStr (show x)
    putStrLn ""

main :: IO ()
main = do
  map <- input >>= readMap
  flashes <- forM [1..507] (const $ step map)
  print (sum $ take 100 flashes)
  print $ 1 + length (takeWhile (/=100) flashes)
