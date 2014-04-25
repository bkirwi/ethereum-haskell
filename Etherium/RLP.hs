module Etherium.RLP(Item(String, List), encode, decode, encodeInt, decodeInt) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Functor
import Data.Word(Word8)
import Data.Attoparsec.ByteString as ABS
import Data.Bits

data Item = String ByteString
          | List [Item]
          deriving (Eq, Show)

encode :: Item -> ByteString
encode x = case x of
  String bytes  -> encodeLength 0x80 bytes
  List children -> encodeLength 0xc0 (mconcat $ map encode children)
  where
    encodeLength :: Word8 -> ByteString -> ByteString
    encodeLength offset bytes 
      | len == 1 && BS.head bytes <= 0x7f = bytes
      | len <= 55 = prefix len <> bytes
      | otherwise = 
        let lenBytes = encodeInt len
            lenLen = BS.length lenBytes + 55
        in prefix lenLen <> lenBytes <> bytes
      where
        len = BS.length bytes
        prefix n = BS.singleton $ offset + fromIntegral n

encodeInt :: Int -> ByteString
encodeInt = BS.reverse <$> BS.unfoldr nextByte
  where
    nextByte 0 = Nothing
    nextByte n = Just (fromIntegral n, n `quot` 256)

decodeInt :: ByteString -> Int
decodeInt = BS.foldl addByte 0 
  where
    addByte n b = (n * 256) + fromIntegral b
      
parseItem :: Parser (Int, Item)
parseItem = do 
  first <- anyWord8
  let typeBits = 0xc0 .&. first
      parseString, parseList :: Int => Parser Item
      parseString n = String <$> ABS.take n
      parseList 0 = return $ List []
      parseList n = do
        (took, ret) <- parseItem
        List rest <- parseList (n - took)
        return . List $ ret : rest
      withSize offset parser
        | lenBits <= 55 = do
          res <- parser lenBits
          return (1 + lenBits, res)
        | otherwise = do
          let lenLen = lenBits - 55
          bytes <- ABS.take lenLen
          let len = decodeInt bytes
          ret <- parser len
          return (1 + lenLen + len, ret)
        where lenBits = fromIntegral $ first - offset
  case typeBits of
    0x80 -> withSize 0x80 parseString
    0xc0 -> withSize 0xc0 parseList
    otherwise -> return (1, String $ BS.singleton first)

decode :: ByteString => Either String Item
decode = parseOnly (snd <$> parseItem)
