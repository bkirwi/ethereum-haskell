{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Etherium.TrieSpec(spec) where

import Control.Monad.State
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Maybe
import Data.Functor

import Etherium.Trie as P

import Test.Hspec
import Test.QuickCheck
import Etherium.QuickCheck

data MapDB = MapDB { getMap :: Map Digest Node }

instance DB (State MapDB) where
  getDB key = getNode <$> getMap <$> get
    where getNode map = fromMaybe Empty $ Map.lookup key map
  putDB key value = undefined

emptyMap :: MapDB
emptyMap = MapDB { getMap = Map.empty }

spec :: Spec
spec = do

  describe "Node-specific cases" $ do
    it "Empty: should always return an empty string" $ property $ \path ->
      let (result, _) = runState (lookupPath (Literal Empty) path) emptyMap 
      in result `shouldBe` ""
    
    it "Value: should retrieve the value with same path" $ property $ \path val ->
      let (result, _) = runState (lookupPath (Literal $ Value path val) path) emptyMap 
      in result `shouldBe` val
      
    it "Value: should retrieve default when paths differ" $ property $ \path nodePath val->
      path /= nodePath ==>
        let (result, _) = runState (lookupPath (Literal $ Value nodePath val) path) emptyMap 
        in result `shouldBe` ""
    
