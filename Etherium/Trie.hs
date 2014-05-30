module Etherium.Trie (insert, lookup , module ETI) where

import Prelude hiding (lookup)

import Etherium.Prelude
import Etherium.Trie.Internal as ETI

insert :: DB m => Ref -> ByteString -> ByteString -> m Ref
insert ref key val = insertRef ref (unpackWord4s key) val

lookup :: DB m => Ref -> ByteString -> m ByteString
lookup ref bs = lookupPath ref $ unpackWord4s bs 
