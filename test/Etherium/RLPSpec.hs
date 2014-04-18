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

    example "dog" "\x83\&dog"
    
    example 
      (RLP.List ["cat", "dog"])
      "\xc8\x83\&cat\x83\&dog"

    example "" "\x80"

    example "\x0f" "\x0f"

    example "\x04\x00" "\x82\x04\x00"

    example "\x0f" "\x0f"

    -- [ [], [[]], [ [], [[]] ] ]
    example 
      (RLP.List [ (RLP.List [])
                , (RLP.List [RLP.List []]) 
                , (RLP.List [ (RLP.List []) 
                            , (RLP.List [RLP.List []])
                            ])
                ])
      "\xc7\xc0\xc1\xc0\xc3\xc0\xc1\xc0"

    it "should encode lorem ipsum properly" $
      let encoded = RLP.encode "Lorem ipsum dolor sit amet, consectetur adipisicing elit"
      in ( "\xb8\x38Lorem " `BS.isPrefixOf` encoded) && ("elit" `BS.isSuffixOf` encoded )
  where 
    example input output = do
      it ("should encode " <> show input <> " as " <> show output) $ 
        RLP.encode input `shouldBe` output
