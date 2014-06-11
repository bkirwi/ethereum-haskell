{-# LANGUAGE OverloadedStrings #-}
module Ethereum.Testing(testCommon) where

import Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char(digitToInt)
import Data.HashMap.Strict(toList)
import qualified Data.Text as T
import System.IO.Unsafe(unsafePerformIO) -- Sorry!

import Ethereum.Prelude
import Ethereum.Trie.Path

import Test.Hspec
import Test.QuickCheck
import Data.String(IsString, fromString)

instance Arbitrary Word4 where
  arbitrary = fstWord4 <$> arbitrary
  shrink word4 = do
    shrunk <- shrink $ word4toInt word4
    return $ sndWord4 $ fromIntegral shrunk

instance Arbitrary ByteString where 
  arbitrary = BS.pack <$> listOf arbitrary
  shrink = fmap BS.pack . shrink . BS.unpack


instance FromJSON ByteString where
  parseJSON (String s) = BS.pack . fromHex <$> parseJSON (String s)
    where
      fromHex :: String -> [Word8]
      fromHex (a:b:rest) = fromIntegral (digitToInt a * 0x10 + digitToInt b) : fromHex rest 
      fromHex [] = []

data TestCases a = TestCases [(String, a)]

instance FromJSON a => FromJSON (TestCases a) where
  parseJSON (Object obj) = TestCases <$> mapM parseCase (toList obj)
    where
      parseCase (key, value) = do
        parsedVal <- parseJSON value
        return (T.unpack key, parsedVal)

testCommon :: FromJSON a => String -> (a -> Spec) -> Spec
testCommon filename doTest = 
    let file = unsafePerformIO $ LBS.readFile $ "test-cases/" ++ filename ++ ".json"
        Just (TestCases testCases) = A.decode file 
        doTestCase (name, testCase) = describe name $ doTest testCase
    in mapM_ doTestCase testCases
