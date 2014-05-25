{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Etherium.TrieSpec(spec) where

import Prelude hiding (lookup)
import Control.Monad
import Data.Aeson
import qualified Data.ByteString as BS
import Data.List(isPrefixOf)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq)
import qualified Data.Text.Encoding as T
import Data.Text (Text)
import GHC.Generics (Generic)

import Etherium.Prelude
import Etherium.Trie
import Etherium.Trie.MapDB

import Test.Hspec
import Test.QuickCheck
import Etherium.Testing

instance Arbitrary (MapDB Ref) where
  arbitrary = sized anyNode
    where
      anyNode size
        | size <= 1 = oneof [empty, shortcutVal]
        | otherwise = oneof [empty, shortcutVal, shortcutRef, node]
        where
          empty, shortcutVal, shortcutRef, node :: Gen (MapDB Ref)
          empty = return $ putNode Empty
          shortcutVal = do
            path <- arbitrary
            value <- arbitrary
            return $ do
              ref <- putNode $ Shortcut path $ Right value
              normalize ref
          shortcutRef = do
            path <- arbitrary
            refM <- anyNode (size - 1)
            return $ do
              ref <- refM 
              newRef <- putNode $ Shortcut path $ Left ref
              normalize newRef
          node = do
            refsM <- sequence $ replicate 16 $ anyNode (size `div` 8)
            value <- arbitrary
            return $ do
              refs <- sequence $ refsM
              ref <- putNode $ Full (Seq.fromList refs) value
              normalize ref

data TrieCase = TrieCase
  { inputs :: [(Text, Text)]
  , expectation :: ByteString
  } deriving (Show, Generic)

instance FromJSON TrieCase 

spec :: Spec
spec = do
  describe "Node-specific cases" $ do
    
    describe "Empty" $ do
      it "should always return an empty string" $ property $ \path ->
        let result = runMapDB $ do
              ref <- putNode Empty
              lookupPath ref path
        in result `shouldBe` ""
    
    describe "Shortcut" $ do
      it "should retrieve the value with same path" $ property $ \path val ->
        let result = runMapDB $ do
              ref <- putNode $ Shortcut path $ Right val
              lookupPath ref path
        in result `shouldBe` val
        
      it "should retrieve default when paths differ" $ property $ \path nodePath val ->
        path /= nodePath ==>
          let result = runMapDB $ do
                ref <- putNode $ Shortcut nodePath $ Right val
                lookupPath ref path
          in result `shouldBe` ""

      it "should follow the path to the targeted node" $ property $ \root initPath subpath ->
        let expected = runMapDB $ do
              ref <- root
              lookupPath ref subpath
            result = runMapDB $ do
              sub <- root
              ref <- putNode $ Shortcut initPath $ Left sub 
              lookupPath ref (initPath ++ subpath)
        in result `shouldBe` expected

      it "should return nothing when paths conflict" $ property $ \root path nodePath ->
        not (nodePath `isPrefixOf` path) ==>
          let result = runMapDB $ do
                sub <- root
                ref <- putNode $ Shortcut nodePath $ Left sub
                lookupPath ref path
          in result `shouldBe` ""

    describe "Full" $ do
      it "should get the value with an empty path" $ property $ \root ->
        runMapDB $ do
          ref <- root
          node <- getNode ref
          case node of
            Full refs val -> do
              result <- lookupPath ref []
              return $ result `shouldBe` val
            _ -> discard

      it "should follow the path to descendants" $ property $ \root pHead pTail->
        runMapDB $ do
          ref <- root
          node <- getNode ref
          case node of
            Full refs _ -> do
              let path = pHead : pTail
                  next = refs `Seq.index` word4toInt pHead
              val <- lookupPath next pTail
              result <- lookupPath ref path
              return $ result `shouldBe` val
            _ -> discard

  describe "Externally-visible properties" $ do
    it "should read its writes" $ property $ \root key value ->
      runMapDB $ do
        ref <- root
        ref0 <- insert ref key value
        got <- lookup ref0 key
        return $ got `shouldBe` value

    it "preserves writes to different keys" $ property $ \root key0 value0 key1 value1 ->
      key0 /= key1 ==> runMapDB $ do
        ref <- root
        root0 <- insert ref key0 value0
        root1 <- insert root0 key1 value1
        got <- lookup root1 key0
        return $ got `shouldBe` value0

    it "should keep the same hash when reinserting a key" $ property $ \root key ->
      runMapDB $ do
        ref <- root
        got <- lookup ref key
        newRef <- insert ref key got
        return $ newRef `shouldBe` ref

  describe "Common test cases" $ testCommon "trietest" $ \test ->
    it ("should hash to " ++ (show $ Digest $ expectation test) ++ " when all values are inserted") $ 
      let result = runMapDB $ foldM insertPair (Literal Empty) $ inputs test 
          pack = T.encodeUtf8
          insertPair root (key, value) = insert root (pack key) (pack value)
      in result `shouldBe` (Hash . Digest $ expectation test)

