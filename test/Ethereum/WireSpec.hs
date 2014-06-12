module Ethereum.WireSpec(spec) where

import Data.Attoparsec.ByteString as A

import Ethereum.Wire

import Test.Hspec
import Test.QuickCheck
import Ethereum.Testing

spec = do

  it "should roundtrip message bodies" $ property $ \bytes ->
    A.parseOnly parseMessage (buildMessage bytes) `shouldBe` Right bytes
