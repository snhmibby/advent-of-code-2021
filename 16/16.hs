module Main where

import Data.Bits

type Range = (Int, Int)
type BitString = String

type Packet = [Layer]
data Layer
  = Header { version, typeID :: Integer }
  | Literal { value :: Integer }
  | BadPacket String
  deriving (Show)

type PacketParser a = BitString -> Maybe (BitString, a)

len :: Range -> Int
len (f,t) = t - f

-- take the number of a range in the bitstring
numOf :: Int -> BitString -> Integer
numOf l b = foldl addbit 0 $ take l b
  where
    addbit :: Integer -> Char -> Integer
    addbit x '1' = 2*x + 1
    addbit x '0' = 2*x
    addbit _ x = error "??"
    -- mask = (1 `shiftL` (l+1)) - 1   -- if bitstring was an integer

numOfTest
  = [ ("111", 7)
    , ("000", 0)
    , ("001", 1)
    , ("010", 2)
    , ("011", 3)
    , ("100", 4)
    , ("101", 5)
    , ("10000", 16)
    ]

testNumOf :: Bool
testNumOf = all (== True) $ map test1 numOfTest
  where test1 (input, want) = want == (numOf (length input) input)

parsePacket :: PacketParser Packet
parsePacket bits = do
  (nextbits, h@(Header _ id)) <- parseHeader bits
  (nextbits', layer) <- case id of
    4 -> literal nextbits


    _ -> return (nextbits, BadPacket $ show nextbits)
  return (nextbits', [h, layer])

parseHeader :: PacketParser Layer
parseHeader bits = do
  let version = numOf 3 bits
      typeid  = numOf 3 (drop 3 bits)
  return (drop 6 bits, Header version typeid)

-- a list of 5 bit-portions.
-- bit 1 == 1 -> not the end
literal :: PacketParser Layer
literal [] = error "bad literal (empty bitstring)"
literal b = do
  (rest, (_, value)) <- go b
  return (rest, Literal value)
  where
    go :: PacketParser (Int, Integer)
    go [] = error "empty bitstring for literal"
    go (b:bs) = do
      let value = numOf 4 bs
          rest = drop 4 bs
      if b == '0' -- don't recurse
      then return (rest, (1, value))
      else do
        (rest', (l, valuerest)) <- go rest
        return  (rest', (l+1, (value `shiftL` (4*l)) + valuerest))

showbits :: Integer -> String
showbits x = showbits' x -- lol' each 'bit' is a character :D
  where
    showbits' :: Integer -> String
    showbits' 0 = ""
    showbits' bits = do
      case 1 .&. bits of
        0 -> '0' : showbits' (bits `shiftR` 1)
        1 -> '1' : showbits' (bits `shiftR` 1)
        d -> error $ "bad bits?? " ++ show d

main :: IO ()
main = do
  let pkt = showbits $ read ("0x" ++ "D21050") -- literal ?
  let pkt = showbits $ read ("0x" ++ "D21120") -- literal ?
  let pkt = showbits $ read ("0x" ++ "D21028") -- literal ?
  let pkt = showbits $ read ("0x" ++ "D2FE28") -- literal 2021
  print $ drop 6 pkt
  case parsePacket pkt of
    Nothing -> print $ "bad parse: " ++ show pkt
    Just (rest, packet) -> do
      print rest
      print packet
