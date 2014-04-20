{-# LANGUAGE OverloadedStrings #-}
module Etherium.RLPSpec(spec) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Char(ord)
import Data.Word(Word8)
import Data.Monoid

import qualified Etherium.RLP as RLP

import Test.Hspec
import Test.QuickCheck

instance Arbitrary ByteString where 
  arbitrary = do
    bytes <- listOf arbitrary
    return $ BS.pack bytes
  shrink bytes = do
    shrunk <- shrink $ BS.unpack bytes 
    return $ BS.pack shrunk

instance Arbitrary RLP.Item where
  arbitrary = sized $ \n -> 
    if n <= 2 then strGen
    else oneof [strGen]
    where 
      strGen = do
        bytes <- arbitrary
        return . RLP.String $ bytes
      listGen = do 
        list <- listOf arbitrary
        return $ RLP.List list

  shrink (RLP.String bytes) = do
    shrunk <- shrink bytes
    return $ RLP.String shrunk
  shrink (RLP.List list) =
    let shrunkLists = do
          shrunk <- shrink list
          return $ RLP.List shrunk
    in list ++ shrunkLists

spec :: Spec
spec = do
  it "should decode what it encodes" $ property $ \rlp ->
    (RLP.decode . RLP.encode $ rlp) `shouldBe` Right rlp

  describe "handles example conversions" $ do

    "dog" `encodesTo` "\x83\&dog"
    
    (RLP.List ["cat", "dog"]) `encodesTo` "\xc8\x83\&cat\x83\&dog"

    "" `encodesTo` "\x80"

    "\x0f" `encodesTo` "\x0f"

    "\x04\x00" `encodesTo` "\x82\x04\x00"

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
