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
    to input = List $ productToItems $ G.from input
    from (List list) = G.to <$> productFromItems list
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
  partialFromItems :: [Item] -> Maybe (f a, [Item])
  productToItems :: f a -> [Item]

productFromItems :: ConvertProduct f => [Item] -> Maybe (f a)
productFromItems list = partialFromItems list >>= complete
  where
    complete (a, []) = Just a
    complete _ = Nothing

instance ConvertProduct a => ConvertProduct (M1 i c a) where
  partialFromItems x = do
    (y, rest) <- partialFromItems x
    return (M1 y, rest)
  productToItems (M1 x) = productToItems x

instance Convert a => ConvertProduct (K1 i a) where
  partialFromItems (item : rest) = do
    x <- fromItem item
    return (K1 x, rest)
  partialFromItems [] = Nothing
  productToItems (K1 x) = [toItem x]

instance (ConvertProduct a, ConvertProduct b) => ConvertProduct (a :*: b) where
  partialFromItems list = do
    (a, rest0) <- partialFromItems list
    (b, rest1) <- partialFromItems rest0
    return (a :*: b, rest1)
  productToItems (a :*: b) = productToItems a ++ productToItems b

instance (ConvertProduct a, ConvertProduct b) => ConvertProduct (a :+: b) where
  partialFromItems items = do
    let left = L1 <$> productFromItems items
        right = R1 <$> productFromItems items
    product <- left <|> right
    return (product, [])
  productToItems (L1 left) = productToItems left
  productToItems (R1 right) = productToItems right

instance ConvertProduct U1 where
  partialFromItems x = Just (U1, x)
  productToItems _ = []


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
  underlyingFromItem item = 
    let left = L1 <$> underlyingFromItem item
        right = R1 <$> underlyingFromItem item
    in left <|> right
  underlyingToItem (L1 left) = underlyingToItem left
  underlyingToItem (R1 right) = underlyingToItem right

