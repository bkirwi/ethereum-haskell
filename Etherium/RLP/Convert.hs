{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts #-}
module Etherium.RLP.Convert(AsRLP, toRLP, fromRLP) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Char(ord)
import Data.Word(Word8)
import Data.Monoid
import Data.Functor
import qualified Data.Sequence as Seq
import Data.Foldable(toList)
import GHC.Generics

import Etherium.RLP

class AsRLP a where

  fromRLP :: Item -> Maybe a
  default fromRLP :: (Generic a, G_AsRLP (Rep a)) => Item -> Maybe a
  fromRLP (List item) = do
    (got, remaining) <- g_fromRLP item
    case remaining of
      [] -> Just $ to got
      _ -> Nothing
  fromRLP (String _) = Nothing

  toRLP :: a -> Item
  default toRLP :: (Generic a, G_AsRLP (Rep a)) => a -> Item
  toRLP item = List . g_toRLP $ from item

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

instance AsRLP a => AsRLP (Seq.Seq a) where
  fromRLP item = Seq.fromList <$> fromRLP item
  toRLP = toRLP . toList

instance AsRLP Integer where
  fromRLP (String s) = Just $ decodeInt s
  fromRLP (List _) = Nothing
  toRLP n
    | n >= 0 = String $ encodeInt n
    | otherwise = error "Can't encode a negative integral type"

instance AsRLP Int where
  fromRLP (String s) = Just $ decodeInt s
  fromRLP (List _) = Nothing
  toRLP n
    | n >= 0 = String $ encodeInt n
    | otherwise = error "Can't encode a negative integral type"

instance AsRLP a => AsRLP (Maybe a) where
  fromRLP (String s) | BS.null s = Just Nothing
  fromRLP x = fmap Just $ fromRLP x
  toRLP (Just a) = toRLP a
  toRLP (Nothing) = String BS.empty

-- Generics!

class G_AsRLP f where
  g_fromRLP :: [Item] -> Maybe (f a, [Item])
  g_toRLP :: f a -> [Item]

instance G_AsRLP a => G_AsRLP (M1 i c a) where
  g_fromRLP x = do
    (item, rest) <- g_fromRLP x
    return (M1 item, rest)
  g_toRLP (M1 x) = g_toRLP x

instance AsRLP a => G_AsRLP (K1 i a) where
  g_fromRLP [] = Nothing
  g_fromRLP (x:xs) = do
    item <- fromRLP x
    return (K1 item, xs)
  g_toRLP (K1 x) = [toRLP x]

instance (G_AsRLP a, G_AsRLP b) => G_AsRLP (a :*: b) where
  g_fromRLP list = do
    (a, list') <- g_fromRLP list
    (b, list'') <- g_fromRLP list'
    return (a :*: b, list'')
    
  g_toRLP (a :*: b) = g_toRLP a ++ g_toRLP b
