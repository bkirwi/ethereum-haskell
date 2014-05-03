{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Etherium.Trie() where

import qualified Crypto.Hash.SHA3 as SHA3
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
import Etherium.Trie.Path

newtype Digest = Digest ByteString
  deriving (Ord, Show, Eq, AsRLP)

data Ref = Hash Digest | Literal Node

data Node = Empty
          | Value Path ByteString
          | Subnode Path Ref
          | Node [Ref] ByteString

instance AsRLP Ref where
  toRLP (Hash h) = toRLP h
  toRLP (Literal l) = toRLP l
  fromRLP rlp = do
    bytes <- fromRLP rlp
    if BS.length bytes < 32 
      then (hush $ RLP.decode bytes) >>= fromRLP
      else return . Hash . Digest $ bytes

instance AsRLP Node where
  toRLP Empty = toRLP BS.empty
  toRLP (Value path val) = toRLP [encodePath True path, val]
  toRLP (Subnode path ref) = toRLP [toRLP $ encodePath False path, toRLP ref]
  toRLP (Node refs val) = toRLP (map toRLP refs <> [toRLP val])
  fromRLP (RLP.String bs)
    | BS.null bs = Just Empty
    | otherwise = Nothing
  fromRLP (RLP.List [pathItem, targetItem]) = do
    pathBS <- fromRLP pathItem
    (isTerminal, path) <- decodePath pathBS
    if isTerminal 
      then do
        val <- fromRLP targetItem
        return $ Value path val 
      else do
        ref <- fromRLP targetItem 
        return $ Subnode path ref
  fromRLP (RLP.List many)
    | length many == 17 = do
      refs <- mapM fromRLP $ init many 
      val <- fromRLP $ last many
      return $ Node refs val
    | otherwise = Nothing
