{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Etherium.Trie.MapDB (
  MapDB, runMapDB,
  ) where

import Control.Monad.State
import qualified Data.Map as Map

import Etherium.Prelude
import Etherium.Trie

newtype MapDB a = MapDB { getState :: State (Map Digest Node) a }
  deriving (Functor, Applicative, Monad)

instance DB MapDB where
  getDB key = MapDB $ 
    let getNode = fromMaybe Empty . Map.lookup key
    in get >>= return . getNode
  putDB key value = MapDB $ 
    let putNode = Map.insert key value
    in get >>= return . putNode >>= put

instance Show a => Show (MapDB a) where
  show = show . runMapDB

runMapDB :: MapDB a -> a
runMapDB x = evalState (getState x) Map.empty

