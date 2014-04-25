module Etherium.RLP.Convert(AsRLP, toRLP, fromRLP, fromList, fromString) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Char(ord)
import Data.Word(Word8)
import Data.Monoid
import Data.Functor

import Etherium.RLP

class AsRLP a where
  fromRLP :: Item -> Maybe a
  toRLP :: a -> Item

instance AsRLP Item where
  fromRLP = Just
  toRLP = id

instance AsRLP Int where
  fromRLP = fmap decodeInt . fromString
  toRLP = String . encodeInt

instance AsRLP ByteString where
  fromRLP = fromString
  toRLP = String

fromList :: Item -> Maybe [Item]
fromList (List items) = Just items
fromList (String _) = Nothing

fromString :: Item -> Maybe ByteString
fromString (String bytes) = Just bytes
fromString (List _) = Nothing
