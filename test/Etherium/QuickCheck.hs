{-# LANGUAGE OverloadedStrings #-}
module Etherium.QuickCheck() where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Char(ord)
import Data.Word(Word8)
import Data.Monoid
import Data.Functor

import Etherium.Trie.Path

import Test.Hspec
import Test.QuickCheck
import Data.String(IsString, fromString)

instance Arbitrary Word4 where
  arbitrary = fstWord4 <$> arbitrary
  shrink word4 = do
    shrunk <- shrink $ asInt word4
    return $ sndWord4 $ fromIntegral shrunk

instance Arbitrary ByteString where 
  arbitrary = BS.pack <$> listOf arbitrary
  shrink = fmap BS.pack . shrink . BS.unpack


