{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Etherium.Patricia() where

import Prelude hiding (lookup)
import Crypto.Hash.SHA3 as SHA3
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.ByteString(ByteString)
import Data.Functor
import qualified Data.Map as Map
import Data.Map(Map)

import qualified Etherium.RLP as RLP
import Etherium.RLP.Convert

newtype Path = Path [Word8]

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
