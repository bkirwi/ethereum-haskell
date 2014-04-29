{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Etherium.Patricia() where

import Crypto.Hash.SHA3 as SHA3
import Control.Error
import Control.Monad.State
import Data.Bits
import qualified Data.ByteString as BS
import Data.ByteString(ByteString)
import Data.Functor
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Monoid
import Data.Word(Word8)

import qualified Etherium.RLP as RLP
import Etherium.RLP.Convert

newtype Path = Path [Word8]

encodePath :: Bool -> Path -> ByteString
encodePath isTerminal (Path path) = 
  let pack a b = (a `shift` 4) .|. b
      pair a (Nothing, xs) = (Just a, xs)
      pair a (Just b, xs)  = (Nothing, a `pack` b : xs)
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
          unpackByte a = [a `shiftR` 4, a .&. 0x0f]
          bodyPath = BS.unpack body >>= unpackByte
          path = case (firstByte .&. 0x10) of
            0x10 -> (firstByte .&. 0x0f) : bodyPath
            _ -> bodyPath
      in (isTerminal, Path path)

newtype Digest = Digest ByteString
  deriving (Ord, Show, Eq, AsRLP)

data Ref = Hash Digest | Literal Node

data Node = Empty
          | Value Path ByteString
          | Subnode Path Node
          | Node [Ref] ByteString

instance AsRLP Ref where
  toRLP (Hash h) = toRLP h
  toRLP (Literal l) = toRLP l
  fromRLP rlp = do
    bytes <- fromString rlp
    if BS.length bytes < 32 
      then case RLP.decode bytes of
        Left err -> Nothing
        Right decoded -> fromRLP decoded
      else return . Hash . Digest $ bytes

instance AsRLP Node where
  toRLP Empty = RLP.String BS.empty
  toRLP _ = undefined
  fromRLP _ = undefined
