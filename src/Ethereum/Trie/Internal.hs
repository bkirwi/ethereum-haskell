{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ethereum.Trie.Internal
  ( NodeDB, Digest(..)
  , getNode, putNode
  , lookupPath
  , insertRef, insertPath
  , emptyRef, normalize
  , Ref(..), Node(..)
  ) where

import qualified Crypto.Hash.SHA3 as SHA3
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.Char (intToDigit)
import Data.Foldable (toList)
import Data.List(stripPrefix)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq)

import Ethereum.Prelude
import qualified Ethereum.RLP as RLP
import Ethereum.RLP.Convert
import Ethereum.Trie.Path

newtype Digest = Digest ByteString
  deriving (Ord, Eq, RLP.Convert)

instance Show Digest where
  show (Digest bs) = map (intToDigit . word4toInt) $ unpackWord4s bs 

data Ref = Hash Digest | Literal Node
  deriving (Show, Eq)

data Node = Empty
          | Shortcut Path (Either Ref ByteString)
          | Full (Seq Ref) ByteString
  deriving (Show, Eq)

instance RLP.Convert Ref where
  asRLP = RLPConvert to from
    where
      to (Hash h) = toRLP h
      to (Literal l) = toRLP l
      from rlp = do
        bytes <- fromRLP rlp
        if BS.length bytes < 32 
          then RLP.decode bytes >>= fromRLP
          else return . Hash . Digest $ bytes

instance RLP.Convert Node where
  asRLP = RLPConvert to from
    where
      to Empty = toRLP BS.empty
      to (Shortcut path (Right val)) = toRLP [encodePath True path, val]
      to (Shortcut path (Left ref)) = toRLP [toRLP $ encodePath False path, toRLP ref]
      to (Full refs val) = toRLP (fmap toRLP refs <> Seq.singleton (toRLP val))
      from (RLP.String bs)
        | BS.null bs = Just Empty
        | otherwise = Nothing
      from (RLP.List [pathItem, targetItem]) = do
        pathBS <- fromRLP pathItem
        (isTerminal, path) <- decodePath pathBS
        target <-
          if isTerminal then Right <$> fromRLP targetItem 
          else Left <$> fromRLP targetItem
        return $ Shortcut path target 
      from (RLP.List many) = do
        guard $ length many == 17 
        refs <- mapM fromRLP $ init many 
        val <- fromRLP $ last many
        return $ Full (Seq.fromList refs) val

type NodeDB = DB Digest Node

putNode :: Node -> NodeDB Ref
putNode node =
  let bytes = RLP.encode $ toRLP node
      digest = Digest $ SHA3.hash 256 bytes
  in if BS.length bytes < 32
    then return $ Literal node
    else do
      insertDB digest node 
      return $ Hash digest

getNode :: Ref -> NodeDB Node
getNode (Hash d) = lookupDB d
getNode (Literal n) = return n
            
lookupPath :: Ref -> Path -> NodeDB ByteString
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
      (w:rest) -> lookupPath (refs `Seq.index` word4toInt w) rest

emptyRefs = Seq.replicate 16 $ Literal Empty
emptyRef = Literal Empty

toFull :: Node -> NodeDB Node
toFull Empty = return $ Full emptyRefs BS.empty
toFull f@(Full _ _) = return f
toFull (Shortcut [] (Left ref)) = getNode ref >>= toFull
toFull (Shortcut [] (Right bs)) = return $ Full emptyRefs bs
toFull (Shortcut (p:ps) val) = do
  ref <- putNode $ Shortcut ps val
  let newRefs = Seq.update (word4toInt p) ref emptyRefs 
  return $ Full newRefs BS.empty

insertPath :: Node -> Path -> ByteString -> NodeDB Node
insertPath Empty path bs 
  | BS.null bs = return Empty
  | otherwise = return $ Shortcut path $ Right bs
insertPath (Shortcut nPath nVal) path bs = do
  let (prefix, nSuffix, suffix) = splitPrefix nPath path
  next <- case (nSuffix, suffix, nVal) of
    ([], [], Right _) -> return $ Right bs
    ([], _, Left ref) -> do
      node <- getNode ref
      newNode <- insertPath node suffix bs
      return $ Left newNode 
    _ -> do 
      full <- toFull (Shortcut nSuffix nVal) 
      newNode <- insertPath full suffix bs
      return $ Left newNode
  case (prefix, next) of
    ([], Left newNode) -> return newNode
    (_, Left newNode) -> Shortcut prefix . Left <$> putNode newNode 
    (_, Right bs) -> return $ Shortcut prefix $ Right bs 
  where
    splitPrefix [] b = ([], [], b)  
    splitPrefix a [] = ([], a, [])  
    splitPrefix (a:as) (b:bs)
      | a == b =
        let (prefix, aSuffix, bSuffix) = splitPrefix as bs
        in (a:prefix, aSuffix, bSuffix)
      | otherwise = ([], a:as, b:bs)
insertPath (Full refs val) [] bs = return $ Full refs bs 
insertPath (Full refs val) (p:ps) bs = do
  let index = word4toInt p
      ref = refs `Seq.index` index
  newRef <- insertRef ref ps bs
  let newRefs = Seq.update index newRef refs
  return $ Full newRefs val
  
insertRef :: Ref -> Path -> ByteString -> NodeDB Ref
insertRef ref path bs = do
  node <- getNode ref
  newNode <- insertPath node path bs
  newRef <- putNode newNode
  normalize newRef

normalize :: Ref -> NodeDB Ref
normalize ref = getNode ref >>= nrml >>= putNode
  where
    nrml :: Node -> NodeDB Node
    nrml Empty = return Empty
    nrml (Shortcut [] (Left ref)) = getNode ref >>= nrml
    nrml (Shortcut path (Left ref)) = do
      node <- getNode ref >>= nrml
      addPrefix path node
    nrml (Shortcut _ (Right val)) | BS.null val = return Empty
    nrml s@(Shortcut _ _) = return s
    nrml (Full refs val) = do
      nrmlRefs <- mapM normalize $ toList refs
      let nonEmpty = filter (\x -> snd x /= Literal Empty) $ zip [0..] nrmlRefs
      case (BS.null val, nonEmpty) of
        (True, []) -> return Empty
        (True, (w, ref) : []) -> getNode ref >>= nrml >>= addPrefix [sndWord4 w]
        (False, []) -> return $ Shortcut [] $ Right val
        _ -> return $ Full (Seq.fromList nrmlRefs) val
    addPrefix path node = case node of
      Empty -> return Empty
      Shortcut subpath val -> return $ Shortcut (path ++ subpath) val
      _ -> Shortcut path . Left <$> putNode node
