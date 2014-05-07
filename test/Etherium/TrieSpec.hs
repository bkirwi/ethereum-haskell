{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Etherium.TrieSpec(spec) where

import Control.Monad.State
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Maybe
import Data.Functor
import Data.List(isPrefixOf)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq)

import Etherium.Trie as P
import Etherium.Trie.Path(asInt)

import Test.Hspec
import Test.QuickCheck
import Etherium.QuickCheck

newtype MapDB = MapDB { getMap :: Map Digest Node }
  deriving (Show, Eq)

emptyMap = MapDB { getMap = Map.empty }

instance DB (State MapDB) where
  getDB key = getNode <$> getMap <$> get
    where getNode map = fromMaybe Empty $ Map.lookup key map
  putDB key value = do
    map <- get
    put $ MapDB { getMap = Map.insert key value $ getMap map }

instance Arbitrary (State MapDB Ref) where
  arbitrary = sized anynode
    where
      anynode size 
        | size <= 1 = empty
        | otherwise = oneof [empty, value, subnode, node] 
        where
          empty, value, subnode, node :: Gen (State MapDB Ref)
          empty = return $ putNode Empty
          value = do
            path <- arbitrary
            value <- arbitrary
            return $ putNode $ Value path value
          subnode = do
            path <- arbitrary
            refM <- anynode (size - 1)
            return $ do
              ref <- refM 
              putNode $ Subnode path ref
          node = do
            refsM <- sequence $ replicate 16 $ anynode (size `div` 16)
            value <- arbitrary
            return $ do
              refs <- sequence $ refsM
              putNode $ Full (Seq.fromList refs) value

instance Show (State MapDB Ref) where
  show x = show $ runState x emptyMap

runDB :: (State MapDB a) -> a
runDB x = fst $ runState x emptyMap

spec :: Spec
spec = do
  describe "Node-specific cases" $ do
    
    describe "Empty" $ do
      it "should always return an empty string" $ property $ \path ->
        let result = runDB $ do
              ref <- putNode Empty
              lookupPath ref path
        in result `shouldBe` ""
    
    describe "Value" $ do
      it "should retrieve the value with same path" $ property $ \path val ->
        let result = runDB $ do
              ref <- putNode $ Value path val
              lookupPath ref path
        in result `shouldBe` val
        
      it "should retrieve default when paths differ" $ property $ \path nodePath val ->
        path /= nodePath ==>
          let result = runDB $ do
                ref <- putNode $ Value nodePath val
                lookupPath ref path
          in result `shouldBe` ""

    describe "Subnode" $ do
      it "should follow the path to the targeted node" $ property $ \root initPath subpath ->
        let expected = runDB $ do
              ref <- root
              lookupPath ref subpath
            result = runDB $ do
              sub <- root
              ref <- putNode $ Subnode initPath sub 
              lookupPath ref (initPath ++ subpath)
        in result `shouldBe` expected

      it "should return nothing when paths conflict" $ property $ \root path nodePath ->
        not (nodePath `isPrefixOf` path) ==>
          let result = runDB $ do
                sub <- root
                ref <- putNode $ Subnode nodePath sub
                lookupPath ref path
          in result `shouldBe` ""

    describe "Full" $ do
      it "should get the value with an empty path" $ property $ \root ->
        runDB $ do
          ref <- root
          node <- getNode ref
          case node of
            Full refs val -> do
              result <- lookupPath ref []
              return $ result `shouldBe` val
            _ -> discard

      it "should follow the path to descendants" $ property $ \root pHead pTail->
        runDB $ do
          ref <- root
          node <- getNode ref
          case node of
            Full refs _ -> do
              let path = pHead : pTail
                  next = refs `Seq.index` asInt pHead
              val <- lookupPath next pTail
              result <- lookupPath ref path
              return $ result `shouldBe` val
            _ -> discard

