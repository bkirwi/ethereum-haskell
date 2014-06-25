{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts #-}
module Ethereum.RLP.Convert(Convert, toRLP, fromRLP, converter, tagged, basic, general, basicTagged, withTag, Converter(..)) where

import Control.Applicative
import qualified Data.Sequence as Seq
import Data.Foldable(toList)
import GHC.Generics

import Ethereum.Prelude
import Ethereum.RLP.Item

data Converter a = Converter { convertToRLP :: a -> Item, convertFromRLP :: Item -> Maybe a }

class Convert a where
  converter :: Converter a
  default converter :: (Generic a, G_AsRLP (Rep a)) => Converter a
  converter = general

general :: (Generic a, G_AsRLP (Rep a)) => Converter a
general = Converter toX fromX
  where
    toX input = List $ g_toRLP $ from input
    fromX (List list) = do
      (got, []) <- g_fromRLP list
      return $ to got
    fromX _ = Nothing

withTag :: Int -> Converter a -> Converter a
withTag n conv = Converter toX fromX
  where
    tag = toRLP n
    toX input = case convertToRLP conv input of
      List list -> List (tag:list)
      other -> other
    fromX (List (x:xs)) | x == tag = convertFromRLP conv $ List xs
    fromX (List []) = Nothing
    fromX item = convertFromRLP conv item

tagged :: (Generic a, G_AsRLP (Rep a)) => Int -> Converter a
tagged n = withTag n general

basic :: (Generic a, B_AsRLP (Rep a)) => Converter a
basic = Converter toX fromX
  where
    toX input = b_toRLP $ from input
    fromX item = to <$> b_fromRLP item

basicTagged :: (Generic a, B_AsRLP (Rep a)) => Int -> Converter a
basicTagged n = withTag n basic

toRLP :: Convert a => a -> Item
toRLP = convertToRLP converter

fromRLP :: Convert a => Item -> Maybe a
fromRLP = convertFromRLP converter

instance Convert Item where
  converter = Converter id Just

instance Convert ByteString where
  converter = Converter toRLP fromRLP
    where
      fromRLP (String bs) = Just bs
      fromRLP (List _) = Nothing
      toRLP = String

instance Convert a => Convert [a] where
  converter = Converter to from
    where
      from (List list) = mapM fromRLP list
      from (String _) = Nothing
      to = List . map toRLP

instance Convert a => Convert (Seq.Seq a) where
  converter = Converter to from
    where
      from item = Seq.fromList <$> fromRLP item
      to = toRLP . toList

instance Convert Integer where
  converter = Converter toRLP fromRLP
    where
      fromRLP (String s) = decodeInt s
      fromRLP (List _) = Nothing
      toRLP n
        | n >= 0 = String $ encodeInt n
        | otherwise = error "Can't encode a negative integral type"

instance Convert Int where
  converter = Converter toRLP fromRLP
    where
      fromRLP (String s) = decodeInt s
      fromRLP (List _) = Nothing
      toRLP n
        | n >= 0 = String $ encodeInt n
        | otherwise = error "Can't encode a negative integral type"

-- Generics!

class G_AsRLP f where
  g_fromRLP :: [Item] -> Maybe (f a, [Item])
  g_toRLP :: f a -> [Item]

instance G_AsRLP a => G_AsRLP (M1 i c a) where
  g_fromRLP x = do
    (y, rest) <- g_fromRLP x
    return (M1 y, rest)
  g_toRLP (M1 x) = g_toRLP x

instance Convert a => G_AsRLP (K1 i a) where
  g_fromRLP (item : rest) = do
    x <- fromRLP item
    return (K1 x, rest)
  g_fromRLP [] = Nothing
  g_toRLP (K1 x) = [toRLP x]

instance (G_AsRLP a, G_AsRLP b) => G_AsRLP (a :*: b) where
  g_fromRLP list = do
    (a, rest0) <- g_fromRLP list
    (b, rest1) <- g_fromRLP rest0
    return (a :*: b, rest1)
  g_toRLP (a :*: b) = g_toRLP a ++ g_toRLP b

instance (G_AsRLP a, G_AsRLP b) => G_AsRLP (a :+: b) where
  g_fromRLP items = case g_fromRLP items of
    Just (left, rest) -> Just (L1 left, rest)
    Nothing -> case g_fromRLP items of
      Just (right, rest) -> Just (R1 right, rest)
      Nothing -> Nothing
  g_toRLP (L1 left) = g_toRLP left
  g_toRLP (R1 right) = g_toRLP right

instance G_AsRLP U1 where
  g_fromRLP x = Just (U1, x)
  g_toRLP _ = []


-- Basic - unwrapped version

class B_AsRLP f where
  b_fromRLP :: Item -> Maybe (f a)
  b_toRLP :: f a -> Item

instance B_AsRLP a => B_AsRLP (M1 i c a) where
  b_fromRLP x = M1 <$> b_fromRLP x
  b_toRLP (M1 x) = b_toRLP x

instance Convert a => B_AsRLP (K1 i a) where
  b_fromRLP item = K1 <$> fromRLP item
  b_toRLP (K1 x) = toRLP x

instance (B_AsRLP a, B_AsRLP b) => B_AsRLP (a :+: b) where
  b_fromRLP item = case b_fromRLP item of
    Just left -> Just (L1 left)
    Nothing -> case b_fromRLP item of
      Just right -> Just (R1 right)
      Nothing -> Nothing
  b_toRLP (L1 left) = b_toRLP left
  b_toRLP (R1 right) = b_toRLP right

