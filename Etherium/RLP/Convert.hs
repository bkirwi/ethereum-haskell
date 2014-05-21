{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts #-}
module Etherium.RLP.Convert(AsRLP, toRLP, fromRLP, tag, tagInt, Tag(..)) where

import Control.Applicative
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

data Tag a = Tag Item | NoTag

tagInt :: Int -> Tag a
tagInt n = Tag $ toRLP n

class AsRLP a where

  tag :: Tag a
  tag = NoTag

  fromRLP :: Item -> Maybe a
  default fromRLP :: (Generic a, G_AsRLP (Rep a)) => Item -> Maybe a
  fromRLP = fromTagged tag
    where
      fromTagged :: (Generic a, G_AsRLP (Rep a)) => Tag a -> Item -> Maybe a
      fromTagged (Tag t) (List (x:items)) 
        | t == x = fromTagged NoTag $ List items
        | otherwise = Nothing
      fromTagged _ item = to <$> g_fromRLP item

  toRLP :: a -> Item
  default toRLP :: (Generic a, G_AsRLP (Rep a)) => a -> Item
  toRLP = toTagged tag
    where
      toTagged :: (Generic a, G_AsRLP (Rep a)) => Tag a -> a -> Item
      toTagged t item =
        let raw = g_toRLP $ from item
        in case (t, raw) of
          (Tag x, List list) -> List $ x:list
          (_, item) -> item

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
  fromRLP (List []) = Just Nothing
  fromRLP (List [x]) = Just <$> fromRLP x
  fromRLP _ = Nothing
  toRLP (Just x) = List [toRLP x]
  toRLP Nothing = List []

-- Generics!

class G_AsRLP f where
  g_fromRLP :: Item -> Maybe (f a)
  g_toRLP :: f a -> Item

class G_AsRLPList f where
  g_fromRLPList :: [Item] -> Maybe (f a, [Item])
  g_toRLPList :: f a -> [Item]

instance G_AsRLP a => G_AsRLP (M1 i c a) where
  g_fromRLP x = M1 <$> g_fromRLP x
  g_toRLP (M1 x) = g_toRLP x

instance G_AsRLPList a => G_AsRLPList (M1 i c a) where
  g_fromRLPList x = do
    (item, rest) <- g_fromRLPList x
    return (M1 item, rest)
  g_toRLPList (M1 x) = g_toRLPList x

instance AsRLP a => G_AsRLPList (K1 i a) where
  g_fromRLPList (item:rest) = do
    x <- fromRLP item
    return (K1 x, rest)
  g_fromRLPList [] = Nothing
  g_toRLPList (K1 x) = [toRLP x]

instance AsRLP a => G_AsRLP (K1 i a) where
  g_fromRLP item = K1 <$> fromRLP item
  g_toRLP (K1 x) = toRLP x

instance (G_AsRLPList a, G_AsRLPList b) => G_AsRLP (a :*: b) where
  g_fromRLP (List list) = do
    (b, []) <- g_fromRLPList list
    return b
  g_fromRLP _ = Nothing
    
  g_toRLP (pair) = List $ g_toRLPList pair

instance (G_AsRLPList a, G_AsRLPList b) => G_AsRLPList (a :*: b) where
  g_fromRLPList list = do
    (a, list') <- g_fromRLPList list
    (b, list'') <- g_fromRLPList list'
    return (a :*: b, list'')
    
  g_toRLPList (a :*: b) = g_toRLPList a ++ g_toRLPList b

instance (G_AsRLP a, G_AsRLP b) => G_AsRLP (a :+: b) where
  g_fromRLP item = case g_fromRLP item of
    Just left -> Just (L1 left)
    Nothing -> case g_fromRLP item of
      Just right -> Just (R1 right)
      Nothing -> Nothing
  g_toRLP (L1 left) = g_toRLP left
  g_toRLP (R1 right) = g_toRLP right

instance G_AsRLP U1 where
  g_fromRLP (List []) = Just U1
  g_fromRLP _ = Nothing
  g_toRLP _ = List []
