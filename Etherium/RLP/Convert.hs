module Etherium.RLP.Convert(AsRLP, toRLP, fromRLP) where

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

instance AsRLP ByteString where
  fromRLP (String bs) = Just bs
  fromRLP (List _) = Nothing
  toRLP = String

instance AsRLP a => AsRLP [a] where
  fromRLP (List list) = mapM fromRLP list
  fromRLP (String _) = Nothing
  toRLP = List . map toRLP

