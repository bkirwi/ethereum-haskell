{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Etherium.Trie
  ( DB(..), Digest
  , getNode, putNode
  , lookup, lookupPath
  , insert, insertPath
  , emptyRef
  , Ref(..), Node(..)
  ) where

import Prelude hiding (lookup)
import qualified Crypto.Hash.SHA3 as SHA3
import Control.Error
import Control.Monad.State
import Data.Bits
import qualified Data.ByteString as BS
import Data.ByteString(ByteString)
import Data.Functor
import Data.List(stripPrefix)
import Data.Maybe
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Monoid
import Data.Word(Word8)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq)

import qualified Etherium.RLP as RLP
import Etherium.RLP.Convert
import Etherium.Trie.Path

newtype Digest = Digest ByteString
  deriving (Ord, Show, Eq, AsRLP)

data Ref = Hash Digest | Literal Node
  deriving (Show, Eq)

data Node = Empty
          | Shortcut Path (Either Ref ByteString)
          | Full (Seq Ref) ByteString
  deriving (Show, Eq)

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
  toRLP (Shortcut path (Right val)) = toRLP [encodePath True path, val]
  toRLP (Shortcut path (Left ref)) = toRLP [toRLP $ encodePath False path, toRLP ref]
  toRLP (Full refs val) = toRLP (fmap toRLP refs <> Seq.singleton (toRLP val))
  fromRLP (RLP.String bs)
    | BS.null bs = Just Empty
    | otherwise = Nothing
  fromRLP (RLP.List [pathItem, targetItem]) = do
    pathBS <- fromRLP pathItem
    (isTerminal, path) <- decodePath pathBS
    target <-
      if isTerminal then Right <$> fromRLP targetItem 
      else Left <$> fromRLP targetItem
    return $ Shortcut path target 
  fromRLP (RLP.List many)
    | length many == 17 = do
      refs <- mapM fromRLP $ init many 
      val <- fromRLP $ last many
      return $ Full (Seq.fromList refs) val
    | otherwise = Nothing

class (Functor m, Monad m) => DB m where
  getDB :: Digest -> m Node
  putDB :: Digest -> Node -> m ()

putNode :: DB m => Node -> m Ref
putNode node =
  let bytes = RLP.encode $ toRLP node
      digest = Digest $ SHA3.hash 256 bytes
  in if BS.length bytes < 32
    then return $ Literal node
    else do
      putDB digest node 
      return $ Hash digest

getNode :: DB m => Ref -> m Node
getNode (Hash d) = getDB d
getNode (Literal n) = return n
            
lookupPath :: DB m => Ref -> Path -> m ByteString
lookupPath root path = getNode root >>= getVal
  where
    getVal Empty = return BS.empty
    getVal (Shortcut nodePath (Right val)) = 
      return $ case stripPrefix nodePath path of
        Just [] -> val
        _ -> BS.empty
    getVal (Shortcut nodePath (Left ref)) =
      case stripPrefix nodePath path of
        Nothing -> return BS.empty
        Just remaining -> lookupPath ref remaining
    getVal (Full refs val) = case path of
      [] -> return val
      (w:rest) -> lookupPath (refs `Seq.index` asInt w) rest

lookup :: DB m => Ref -> ByteString -> m ByteString
lookup ref bs = lookupPath ref $ toPath bs

emptyRefs = Seq.replicate 16 $ Literal Empty
emptyRef = Literal Empty

insertPath :: DB m => Node -> Path -> ByteString -> m Node
insertPath Empty path bs = return $ Shortcut path $ Right bs
-- FIXME
insertPath (Shortcut nPath nVal) path bs = return $ Shortcut path $ Right bs 
insertPath (Full refs val) [] bs = return $ Full refs bs 
insertPath (Full refs val) (p:ps) bs = do
  let index = asInt p
      ref = (refs `Seq.index` index)
  newRef <- insertRef ref ps bs
  let newRefs = Seq.update index newRef refs
  return $ Full newRefs val
  
insertRef :: DB m => Ref -> Path -> ByteString -> m Ref
insertRef ref path bs = do
  node <- getNode ref
  newNode <- insertPath node path bs
  putNode newNode

insert :: DB m => Ref -> ByteString -> ByteString -> m Ref
insert ref key val = insertRef ref (toPath key) val
