{-# LANGUAGE OverloadedStrings #-}
module Etherium.RLPSpec(spec) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Char(ord)
import Data.Word(Word8)
import Data.Monoid

import qualified Etherium.RLP as RLP

import Test.Hspec

spec :: Spec
spec = do
  describe "handles example conversions" $ do

    "dog" `encodesTo` "\x83\&dog"
    
    (RLP.List ["cat", "dog"]) `encodesTo` "\xc8\x83\&cat\x83\&dog"

    "" `encodesTo` "\x80"

    "\x0f" `encodesTo` "\x0f"

    "\x04\x00" `encodesTo` "\x82\x04\x00"

    "\x0f" `encodesTo` "\x0f"

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
