module Ethereum.RLP.Item(Item(String, List), encode, decode) where

import Control.Error
import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString as A
import Data.Bits

import Ethereum.Prelude

data Item = String ByteString
          | List [Item]
          deriving (Eq)

instance Show Item where
  show (String str) = show str
  show (List list) = show list

encode :: Item -> ByteString
encode x = case x of
  String bytes -> 
    if BS.length bytes == 1 && BS.head bytes <= 0x7f then bytes
    else encodeLength 0x80 bytes
  List children -> encodeLength 0xc0 (mconcat $ map encode children)
  where
    encodeLength :: Word8 -> ByteString -> ByteString
    encodeLength offset bytes 
      | len <= 55 = prefix len <> bytes
      | otherwise = 
        let lenBytes = encodeInt len
            lenLen = BS.length lenBytes + 55
        in prefix lenLen <> lenBytes <> bytes
      where
        len = BS.length bytes
        prefix n = BS.singleton $ offset + fromIntegral n

parseItem :: Parser (Int, Item)
parseItem = do 
  first <- anyWord8
  let typeBits = 0xc0 .&. first
      parseString, parseList :: Int -> Parser Item
      parseString n = String <$> A.take n
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
          bytes <- A.take lenLen
          len <- justZ $ decodeInt bytes
          ret <- parser len
          return (1 + lenLen + len, ret)
        where lenBits = fromIntegral $ first - offset
  case typeBits of
    0x80 -> withSize 0x80 parseString
    0xc0 -> withSize 0xc0 parseList
    _ -> return (1, String $ BS.singleton first)

decode :: ByteString -> Maybe Item
decode bs = hush $ parseOnly (snd <$> parser) bs
  where
    parser = do
      a <- parseItem
      A.endOfInput
      return a
