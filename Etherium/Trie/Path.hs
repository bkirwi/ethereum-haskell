{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Etherium.Trie.Path (Path, encodePath, decodePath) where

import Control.Error
import Data.Bits
import qualified Data.ByteString as BS

import Etherium.Prelude

type Path = [Word4]

encodePath :: Bool -> Path -> ByteString
encodePath isTerminal path = 
  let pair a (Nothing, xs) = (Just a, xs)
      pair a (Just b, xs)  = (Nothing, a `packWord8` b : xs)
      (odd, body) = foldr pair (Nothing, []) path
      terminalFlag = if isTerminal then 0x20 else 0
      oddFlag = case odd of
        Nothing -> 0x0
        Just w4 -> 0x10 .|. word4to8 w4
      firstByte = terminalFlag .|. oddFlag
  in BS.singleton firstByte <> BS.pack body

decodePath :: ByteString -> Maybe (Bool, Path)
decodePath = fmap decodePair . BS.uncons
  where 
    decodePair (firstByte, body) =
      let isTerminal = (firstByte .&. 0x20) == 0x20
          bodyPath = unpackWord4s body
          path = case firstByte .&. 0x10 of
            0x10 -> (sndWord4 firstByte) : bodyPath
            _ -> bodyPath
      in (isTerminal, path)

