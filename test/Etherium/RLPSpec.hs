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

instance Arbitrary ByteString where 
  arbitrary = do
    BS.pack <$> listOf arbitrary
  shrink bytes =
    BS.pack <$> (shrink . BS.unpack $ bytes) 

instance Arbitrary RLP.Item where
  arbitrary = sized $ \n -> 
    if n <= 2 then strGen
    else oneof [strGen]
    where 
      strGen = RLP.String <$> arbitrary
      listGen = RLP.List <$> listOf arbitrary 

  shrink (RLP.String bytes) =
    RLP.String <$> shrink bytes
  shrink (RLP.List list) =
    let shrunkLists = RLP.List <$> shrink list
    in list ++ shrunkLists

spec :: Spec
spec = do
  it "should decode what it encodes (ints)" $ property $ \n ->
    n >= 0 ==> (RLP.decodeInt . RLP.encodeInt $ n) `shouldBe` n

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

    RLP.String (RLP.encodeInt 15) `encodesTo` "\x0f"

    RLP.String (RLP.encodeInt 1024) `encodesTo` "\x82\x04\x00"

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
