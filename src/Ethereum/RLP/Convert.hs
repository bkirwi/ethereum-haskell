{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts #-}
module Ethereum.RLP.Convert(Convert, toItem, fromItem, converter, asUnderlying, asProduct, tagged, Converter(..)) where

import Control.Applicative
import qualified Data.Sequence as Seq
import Data.Foldable(toList)
import GHC.Generics hiding (to, from)
import qualified GHC.Generics as G

import Ethereum.Prelude
import Ethereum.RLP.Item

data Converter a = Converter { convertToRLP :: a -> Item, convertFromRLP :: Item -> Maybe a }

toItem :: Convert a => a -> Item
toItem = convertToRLP converter

fromItem :: Convert a => Item -> Maybe a
fromItem = convertFromRLP converter

class Convert a where
  converter :: Converter a
  default converter :: (Generic a, ConvertProduct (Rep a)) => Converter a
  converter = asProduct

asProduct :: (Generic a, ConvertProduct (Rep a)) => Converter a
asProduct = Converter to from
  where
    to input = List $ productToItem $ G.from input
    from (List list) = do
      (got, []) <- productFromItem list
      return $ G.to got
    from _ = Nothing

tagged :: Int -> Converter a -> Converter a
tagged n conv = Converter to from
  where
    tag = toItem n
    to input = case convertToRLP conv input of
      List list -> List (tag:list)
      other -> other
    from (List (x:xs)) | x == tag = convertFromRLP conv $ List xs
    from (List []) = Nothing
    from item = convertFromRLP conv item

asUnderlying :: (Generic a, ConvertUnderlying (Rep a)) => Converter a
asUnderlying = Converter to from
  where
    to input = underlyingToItem $ G.from input
    from item = G.to <$> underlyingFromItem item

instance Convert Item where
  converter = Converter id Just

instance Convert ByteString where
  converter = Converter to from
    where
      from (String bs) = Just bs
      from (List _) = Nothing
      to = String

instance Convert a => Convert [a] where
  converter = Converter to from
    where
      from (List list) = mapM fromItem list
      from (String _) = Nothing
      to = List . map toItem

instance Convert a => Convert (Seq.Seq a) where
  converter = Converter to from
    where
      from item = Seq.fromList <$> fromItem item
      to = toItem . toList

instance Convert Integer where
  converter = Converter to from
    where
      from (String s) = decodeInt s
      from (List _) = Nothing
      to n
        | n >= 0 = String $ encodeInt n
        | otherwise = error "Can't encode a negative integral type"

instance Convert Int where
  converter = Converter to from
    where
      from (String s) = decodeInt s
      from (List _) = Nothing
      to n
        | n >= 0 = String $ encodeInt n
        | otherwise = error "Can't encode a negative integral type"

-- Generics!

class ConvertProduct f where
  productFromItem :: [Item] -> Maybe (f a, [Item])
  productToItem :: f a -> [Item]

instance ConvertProduct a => ConvertProduct (M1 i c a) where
  productFromItem x = do
    (y, rest) <- productFromItem x
    return (M1 y, rest)
  productToItem (M1 x) = productToItem x

instance Convert a => ConvertProduct (K1 i a) where
  productFromItem (item : rest) = do
    x <- fromItem item
    return (K1 x, rest)
  productFromItem [] = Nothing
  productToItem (K1 x) = [toItem x]

instance (ConvertProduct a, ConvertProduct b) => ConvertProduct (a :*: b) where
  productFromItem list = do
    (a, rest0) <- productFromItem list
    (b, rest1) <- productFromItem rest0
    return (a :*: b, rest1)
  productToItem (a :*: b) = productToItem a ++ productToItem b

instance (ConvertProduct a, ConvertProduct b) => ConvertProduct (a :+: b) where
  productFromItem items = case productFromItem items of
    Just (left, rest) -> Just (L1 left, rest)
    Nothing -> case productFromItem items of
      Just (right, rest) -> Just (R1 right, rest)
      Nothing -> Nothing
  productToItem (L1 left) = productToItem left
  productToItem (R1 right) = productToItem right

instance ConvertProduct U1 where
  productFromItem x = Just (U1, x)
  productToItem _ = []


-- Basic - unwrapped version

class ConvertUnderlying f where
  underlyingFromItem :: Item -> Maybe (f a)
  underlyingToItem :: f a -> Item

instance ConvertUnderlying a => ConvertUnderlying (M1 i c a) where
  underlyingFromItem x = M1 <$> underlyingFromItem x
  underlyingToItem (M1 x) = underlyingToItem x

instance Convert a => ConvertUnderlying (K1 i a) where
  underlyingFromItem item = K1 <$> fromItem item
  underlyingToItem (K1 x) = toItem x

instance (ConvertUnderlying a, ConvertUnderlying b) => ConvertUnderlying (a :+: b) where
  underlyingFromItem item = case underlyingFromItem item of
    Just left -> Just (L1 left)
    Nothing -> case underlyingFromItem item of
      Just right -> Just (R1 right)
      Nothing -> Nothing
  underlyingToItem (L1 left) = underlyingToItem left
  underlyingToItem (R1 right) = underlyingToItem right

