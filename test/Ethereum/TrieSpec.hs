{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Ethereum.TrieSpec(spec) where

import Prelude hiding (lookup)
import Control.Monad
import Control.Monad.State
import Data.Aeson
import qualified Data.ByteString as BS
import Data.List(isPrefixOf)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Sequence(Seq)
import qualified Data.Text.Encoding as T
import Data.Text (Text)
import GHC.Generics (Generic)

import Ethereum.Prelude
import Ethereum.Trie
import Ethereum.Trie.Internal
import Ethereum.Trie.MapDB

import Test.Hspec
import Test.QuickCheck
import Ethereum.Testing

instance Arbitrary (NodeDB Ref) where
  arbitrary = sized anyNode
    where
      anyNode size
        | size <= 1 = oneof [empty, shortcutVal]
        | otherwise = oneof [empty, shortcutVal, shortcutRef, node]
        where
          empty, shortcutVal, shortcutRef, node :: Gen (NodeDB Ref)
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
            refsM <- replicateM 16 $ anyNode (size `div` 8)
            value <- arbitrary
            return $ do
              refs <- sequence refsM
              ref <- putNode $ Full (Seq.fromList refs) value
              normalize ref

instance Arbitrary (Trie ()) where
  arbitrary = toTrie <$> arbitrary
    where
      toTrie x = do
        ref <- lift x
        put ref

instance Show (Trie ()) where
  show x = "<dunno>"

instance Show (NodeDB Ref) where
  show x = "<dunno>"

data TrieCase = TrieCase
  { inputs :: [(Text, Text)]
  , expectation :: ByteString
  } deriving (Show, Generic)

instance FromJSON TrieCase 

evalMapDB :: Ord k => DB k v a -> Maybe a
evalMapDB db = evalStateT (runMapDB db) Map.empty

runEmpty :: Trie a -> Maybe (a, Ref)
runEmpty = evalMapDB . flip runStateT (Literal Empty)

runEmptyTrie = runEmpty . runTrie

spec :: Spec
spec = do
  describe "Node-specific cases" $ do
    
    describe "Empty" $ do
      it "should always return an empty string" $ property $ \path ->
        let result = evalMapDB $ do
              ref <- putNode Empty
              lookupPath ref path
        in result `shouldBe` Just ""
    
    describe "Shortcut" $ do
      it "should retrieve the value with same path" $ property $ \path val ->
        let result = evalMapDB $ do
              ref <- putNode $ Shortcut path $ Right val
              lookupPath ref path
        in result `shouldBe` Just val
        
      it "should retrieve default when paths differ" $ property $ \path nodePath val ->
        path /= nodePath ==>
          let result = evalMapDB $ do
                ref <- putNode $ Shortcut nodePath $ Right val
                lookupPath ref path
          in result `shouldBe` Just ""

      it "should follow the path to the targeted node" $ property $ \root initPath subpath ->
        let expected = evalMapDB $ do
              ref <- root
              lookupPath ref subpath
            result = evalMapDB $ do
              sub <- root
              ref <- putNode $ Shortcut initPath $ Left sub 
              lookupPath ref (initPath ++ subpath)
        in result `shouldBe` expected

      it "should return nothing when paths conflict" $ property $ \root path nodePath ->
        not (nodePath `isPrefixOf` path) ==>
          let result = evalMapDB $ do
                sub <- root
                ref <- putNode $ Shortcut nodePath $ Left sub
                lookupPath ref path
          in result `shouldBe` Just ""

    describe "Full" $ do
      it "should get the value with an empty path" $ property $ \root ->
        fromJust $ evalMapDB $ do
          ref <- root
          node <- getNode ref
          case node of
            Full refs val -> do
              result <- lookupPath ref []
              return $ result `shouldBe` val
            _ -> discard

      it "should follow the path to descendants" $ property $ \root pHead pTail->
        fromJust $ evalMapDB $ do
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

    let withInitialState init update =
          runEmpty $ do
            () <- init
            runTrie update

    it "should read its writes" $ property $ \init key value ->
      let Just (after, _) = withInitialState init $ do
            insertDB key value
            lookupDB key
      in after `shouldBe` value

    it "preserves writes to different keys" $ property $ \init key0 value0 key1 value1 ->
      key0 /= key1 ==> 
        let Just (got, _) = withInitialState init $ do
              insertDB key0 value0
              insertDB key1 value1
              lookupDB key0
        in got `shouldBe` value0

    it "should keep the same hash when reinserting a key" $ property $ \init key ->
      let Just orig = withInitialState init $ return ()
          Just updated = withInitialState init $ do
            got <- lookupDB key
            insertDB key got
      in updated `shouldBe` orig

  describe "Common test cases" $ testCommon "trietest" $ \test ->
    it ("should hash to " ++ show (Digest $ expectation test) ++ " when all values are inserted") $ 

      let trie = runTrie $ mapM_ insertPair $ inputs test 
          mapDB = runMapDB $ runStateT trie (Literal Empty)
          result = snd <$> evalStateT mapDB Map.empty
          pack = T.encodeUtf8
          insertPair (key, value) = insertDB (pack key) (pack value)
      in result `shouldBe` Just (Hash . Digest $ expectation test)
