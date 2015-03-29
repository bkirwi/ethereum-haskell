{-# LANGUAGE OverloadedStrings #-}

module Ethereum.PreludeSpec(spec) where

import Ethereum.Testing
import Test.Hspec
import Test.QuickCheck
import Ethereum.Prelude
import qualified Data.ByteString as BS

spec :: Spec
spec = do
  describe "decodeInt" $ do
    it "should decode what it encodes (ints)" $ property $ \n ->
      (n :: Int) >= 0 ==> (decodeInt . encodeInt $ n) `shouldBe` Just n

    it "fails to decode zero-prefixed integers" $ property $ \n padding ->
      padding > 0  && n >= 0 ==>
        let padded = BS.replicate padding 0 <> encodeInt (n :: Int)
        in decodeInt padded `shouldBe` Nothing

  describe "encodeInt" $ do
    it "should encode 15" $ encodeInt 15 `shouldBe` "\x0f"
    it "should encode 1024" $ encodeInt 1024 `shouldBe` "\x04\x00"
