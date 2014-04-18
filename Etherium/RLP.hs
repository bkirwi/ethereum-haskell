module Etherium.RLP(Item(String, List), encode) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Word(Word8)
import Data.String(IsString, fromString)

data Item = String ByteString
          | List [Item]
          deriving Show

instance IsString Item where
  fromString = String . fromString
          
encode :: Item -> ByteString
encode x = case x of
  String bytes     -> encodeLength 0x80 bytes
  List children -> encodeLength 0xc0 (mconcat $ map encode children)
  where
    encodeLength :: Word8 -> ByteString -> ByteString
    encodeLength offset bytes 
      | len == 1 && BS.head bytes < 0x7f = bytes
      | len < 56 = prefix len <> bytes
      | otherwise = 
        let lenBytes = encodeInt len
            lenLen = BS.length lenBytes + 55
        in prefix lenLen <> lenBytes <> bytes
      where
        len = BS.length bytes
        prefix n = BS.singleton $ offset + fromIntegral n
    encodeInt :: Int -> ByteString
    encodeInt = BS.unfoldr nextByte
      where
        nextByte 0 = Nothing
        nextByte n = Just (fromIntegral n, n `quot` 256)
      
