{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Etherium.Trie.PathSpec(spec) where

import Prelude hiding (seq)
import Data.Aeson
import qualified Data.ByteString as BS
import Data.ByteString(ByteString)
import Data.Functor
import Data.Monoid
import GHC.Generics

import Etherium.Trie.Path

import Test.Hspec
import Test.QuickCheck
import Etherium.Testing

data HexCase = HexCase
  { seq :: Path
  , term :: Bool
  , out :: ByteString
  } deriving (Show, Generic)

instance FromJSON Word4 where
  parseJSON x = sndWord4 <$> parseJSON x

instance FromJSON HexCase

spec :: Spec
spec = do
  it "should roundtrip paths" $ property $ \x y ->
    (decodePath $ encodePath x y) `shouldBe` Just (x, y) 

  it "should recover the first nibble" $ property $ \x y ->
    (fstWord4 $ x `packByte` y) `shouldBe` x

  it "should recover the second nibble" $ property $ \x y ->
    (sndWord4 $ x `packByte` y) `shouldBe` y

  describe "performs example conversions" $ testCommon "hexencodetest" $ \test -> do
    it ("should encode to " ++ show (out test)) $
      let encoded = encodePath (term test) (seq test)
      in encoded `shouldBe` (out test)
    it ("should decode " ++ show (out test)) $
      let decoded = decodePath (out test)
      in decoded `shouldBe` Just (term test, seq test)
