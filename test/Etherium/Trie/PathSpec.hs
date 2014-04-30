{-# LANGUAGE OverloadedStrings #-}
module Etherium.Trie.PathSpec(spec) where

import qualified Data.ByteString as BS
import Data.ByteString(ByteString)
import Data.Functor
import Data.Monoid

import Etherium.Trie.Path

import Test.Hspec
import Test.QuickCheck

instance Arbitrary Path where
  arbitrary = toPath <$> arbitrary

instance Arbitrary ByteString where 
  arbitrary = do
    BS.pack <$> listOf arbitrary
  shrink bytes =
    BS.pack <$> (shrink . BS.unpack $ bytes) 


spec :: Spec
spec = do
  it "should roundtrip paths correctly" $ property $ \x y ->
    (decodePath $ encodePath x y) `shouldBe` Just (x, y) 
