{-# LANGUAGE OverloadedStrings #-}
module Etherium.RLPSpec(spec) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Char(ord)
import Data.Word(Word8)
import Data.Monoid
import Data.Functor

import qualified Etherium.RLP as RLP

import Test.Hspec
import Test.QuickCheck
import Data.String(IsString, fromString)
import Etherium.QuickCheck

instance IsString RLP.Item where
  fromString = RLP.String . fromString

instance Arbitrary RLP.Item where
  arbitrary = sized item
    where 
      item n 
        | n <= 2 = strGen
        | otherwise = oneof [strGen, resize (n `div` 2) listGen ]
      strGen = RLP.String <$> arbitrary
      listGen = RLP.List <$> arbitrary

  shrink (RLP.String bytes) =
    RLP.String <$> shrink bytes
  shrink (RLP.List list) =
    let shrunkLists = RLP.List <$> shrink list
    in list ++ shrunkLists

spec :: Spec
spec = do

  it "should decode what it encodes (ints)" $ property $ \n ->
    n >= 0 ==> (RLP.decodeInt . RLP.encodeInt $ n) `shouldBe` n

  describe "handle example integer conversions" $ do
    it "should encode 15" $ RLP.encodeInt 15 `shouldBe` "\x0f"
    it "should encode 1024" $ RLP.encodeInt 1024 `shouldBe` "\x04\x00"

  it "should decode what it encodes" $ property $ \rlp ->
    (RLP.decode . RLP.encode $ rlp) `shouldBe` Right rlp

  it "should encode small bytes as themselves" $ property $ \byte ->
    let oneByte = BS.pack [byte]
    in byte <= 0x7f ==> (RLP.encode $ RLP.String oneByte) `shouldBe` oneByte

  describe "handles example conversions" $ do

    "dog" `encodesTo` "\x83\&dog"
    
    (RLP.List ["cat", "dog"]) `encodesTo` "\xc8\x83\&cat\x83\&dog"

    "" `encodesTo` "\x80"

    RLP.List [] `encodesTo` "\xc0"

    "\x0f" {-15-} `encodesTo` "\x0f"

    "\x04\x00" {-1024-} `encodesTo` "\x82\x04\x00"

    -- [ [], [[]], [ [], [[]] ] ]
    (RLP.List [ (RLP.List [])
              , (RLP.List [RLP.List []]) 
              , (RLP.List [ (RLP.List []) 
                          , (RLP.List [RLP.List []])
                          ])
              ]) 
      `encodesTo`
      "\xc7\xc0\xc1\xc0\xc3\xc0\xc1\xc0"

    it "should encode lorem ipsum properly" $
      let encoded = RLP.encode "Lorem ipsum dolor sit amet, consectetur adipisicing elit"
      in ( "\xb8\x38Lorem " `BS.isPrefixOf` encoded) && ("elit" `BS.isSuffixOf` encoded )
  where 
    input `encodesTo` output = do
      it ("should encode " <> show input <> " as " <> show output) $ 
        RLP.encode input `shouldBe` output
      it ("should decode " <> show output <> " to " <> show input) $ 
        RLP.decode output `shouldBe` Right input
