{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Etherium.Trie.Path(Path, toPath, encodePath, decodePath) where

import Control.Error
import Data.Bits
import qualified Data.ByteString as BS
import Data.ByteString(ByteString)
import Data.Functor
import Data.Monoid
import Data.Word(Word8)

newtype Path = Path [Word8]
  deriving (Eq, Show)

packByte :: Word8 -> Word8 -> Word8
packByte a b = (a `shift` 4) .|. b

unpackByte :: Word8 -> [Word8]
unpackByte a = [a `shiftR` 4, a .&. 0x0f]

toPath :: ByteString -> Path
toPath bs = Path $ BS.unpack bs >>= unpackByte

encodePath :: Bool -> Path -> ByteString
encodePath isTerminal (Path path) = 
  let pair a (Nothing, xs) = (Just a, xs)
      pair a (Just b, xs)  = (Nothing, a `packByte` b : xs)
      (odd, body) = foldr pair (Nothing, []) path
      terminalFlag = if isTerminal then 0x20 else 0
      oddFlag = case odd of
        Nothing -> 0
        Just b -> 0x10 .|. b
      firstByte = terminalFlag .|. oddFlag
  in BS.singleton firstByte <> BS.pack body

decodePath :: ByteString -> Maybe (Bool, Path)
decodePath = fmap decodePair . BS.uncons
  where 
    decodePair (firstByte, body) =
      let isTerminal = (firstByte .&. 0x20) == 0x20
          Path bodyPath = toPath body
          path = case (firstByte .&. 0x10) of
            0x10 -> (firstByte .&. 0x0f) : bodyPath
            _ -> bodyPath
      in (isTerminal, Path path)

