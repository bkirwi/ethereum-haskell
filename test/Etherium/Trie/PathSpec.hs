{-# LANGUAGE OverloadedStrings #-}
module Etherium.Trie.PathSpec(spec) where

import qualified Data.ByteString as BS
import Data.ByteString(ByteString)
import Data.Functor
import Data.Monoid

import Etherium.Trie.Path

import Test.Hspec
import Test.QuickCheck
import Etherium.QuickCheck

spec :: Spec
spec = do
  it "should roundtrip paths" $ property $ \x y ->
    (decodePath $ encodePath x y) `shouldBe` Just (x, y) 

  it "should recover the first nibble" $ property $ \x y ->
    (fstWord4 $ x `packByte` y) `shouldBe` x

  it "should recover the second nibble" $ property $ \x y ->
    (sndWord4 $ x `packByte` y) `shouldBe` y
