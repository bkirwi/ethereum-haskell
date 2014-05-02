{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Etherium.Trie.Path
  ( Word4, packByte, fstWord4, sndWord4, asInt
  , Path, toPath, encodePath, decodePath
  ) where

import Control.Error
import Data.Bits
import qualified Data.ByteString as BS
import Data.ByteString(ByteString)
import Data.Functor
import Data.Monoid
import Data.Word(Word8)

newtype Word4 = Word4 Word8
  deriving (Eq, Show)

type Path = [Word4]

packByte :: Word4 -> Word4 -> Word8
packByte (Word4 a) (Word4 b) = (a `shift` 4) .|. b

fstWord4, sndWord4 :: Word8 -> Word4
fstWord4 b = Word4 $ b `shiftR` 4
sndWord4 b = Word4 $ b .&. 0x0f

asInt :: Word4 -> Int
asInt (Word4 n) = fromIntegral n

toPath :: ByteString -> Path
toPath bs = BS.unpack bs >>= unpackByte
  where unpackByte b = [fstWord4 b, sndWord4 b]

encodePath :: Bool -> Path -> ByteString
encodePath isTerminal path = 
  let pair a (Nothing, xs) = (Just a, xs)
      pair a (Just b, xs)  = (Nothing, a `packByte` b : xs)
      (odd, body) = foldr pair (Nothing, []) path
      terminalFlag = if isTerminal then 0x20 else 0
      oddFlag = case odd of
        Nothing -> 0x0
        Just (Word4 b) -> 0x10 .|. b
      firstByte = terminalFlag .|. oddFlag
  in BS.singleton firstByte <> BS.pack body

decodePath :: ByteString -> Maybe (Bool, Path)
decodePath = fmap decodePair . BS.uncons
  where 
    decodePair (firstByte, body) =
      let isTerminal = (firstByte .&. 0x20) == 0x20
          bodyPath = toPath body
          path = case firstByte .&. 0x10 of
            0x10 -> (sndWord4 firstByte) : bodyPath
            _ -> bodyPath
      in (isTerminal, path)

