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

newtype MapDB = MapDB { getMap :: Map Digest Node }
  deriving (Show, Eq)

instance DB (State MapDB) where
  getDB key = getNode <$> getMap <$> get
    where getNode map = fromMaybe Empty $ Map.lookup key map
  putDB key value = undefined

instance Arbitrary MapDB where
  arbitrary = return $ MapDB { getMap = Map.empty }

runDB ::  MapDB -> (State MapDB a) -> (a, MapDB)
runDB = flip runState

spec :: Spec
spec = do
  describe "Node-specific cases" $ do
    it "Empty: should always return an empty string" $ property $ \db path ->
      let (result, _) = runDB db $ lookupPath (Literal Empty) path
      in result `shouldBe` ""
    
    it "Value: should retrieve the value with same path" $ property $ \db path val ->
      let (result, _) = runDB db $ lookupPath (Literal $ Value path val) path
      in result `shouldBe` val
      
    it "Value: should retrieve default when paths differ" $ property $ \db path nodePath val ->
      path /= nodePath ==>
        let (result, _) = runDB db $ lookupPath (Literal $ Value nodePath val) path
        in result `shouldBe` ""
