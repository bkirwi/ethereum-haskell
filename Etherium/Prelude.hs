module Etherium.Prelude
  ( Word4, word4to8, packWord8, fstWord4, sndWord4, word4toInt, unpackWord4s
  , ByteString, Word8, Map
  , module X
  ) where

import Data.Bits
import qualified Data.ByteString as BS
import Data.ByteString(ByteString)
import Data.Char (intToDigit)
import Data.Word (Word8)
import Data.Map (Map)

import Control.Applicative as X
import Data.Functor as X
import Data.Maybe as X
import Data.Monoid as X

newtype Word4 = Word4 { word4to8 :: Word8 }
  deriving (Eq)

instance Show Word4 where
  show = (:[]) . intToDigit . word4toInt

packWord8 :: Word4 -> Word4 -> Word8
packWord8 (Word4 a) (Word4 b) = (a `shift` 4) .|. b

fstWord4, sndWord4 :: Word8 -> Word4
fstWord4 b = Word4 $ b `shiftR` 4
sndWord4 b = Word4 $ b .&. 0x0f

word4toInt :: Word4 -> Int
word4toInt = fromIntegral . word4to8

unpackWord4s :: ByteString -> [Word4]
unpackWord4s bs = BS.unpack bs >>= unpackByte
  where unpackByte b = [fstWord4 b, sndWord4 b]
