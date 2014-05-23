{-# LANGUAGE DeriveGeneric #-}
module Etherium.Block() where

import qualified Data.ByteString as BS
import Data.ByteString(ByteString)
import Data.Functor
import GHC.Generics

import qualified Etherium.RLP as RLP
import Etherium.RLP.Convert
import Etherium.Trie(Digest)

data Block = Block BlockHeader [Transaction] [Uncle] [StackFrame]
  deriving (Show, Generic)

-- Stubs
type Transaction = RLP.Item
type Uncle = RLP.Item

data BlockHeader = BlockHeader
  { parentHash :: ByteString
  , blockNumber :: Integer
  , transactionsDigest :: Digest
  , uncleDigest :: Digest
  , stackDigest :: Digest
  , coinbaseAddress :: ByteString
  , stateRoot :: Digest
  , difficulty :: Integer
  , timestamp :: Integer
  , extraData :: ByteString
  , blockNonce :: ByteString
  } deriving (Show, Generic)

-- Stub
data StackFrame = StackFrame RLP.Item RLP.Item
  deriving (Show, Generic)

data Account = Account
  { accountBalance :: Integer
  , accountNonce :: Integer
  , contractInfo :: ContractInfo
  } deriving (Show, Generic)

data ContractInfo = ContractRoot Digest | External
  deriving (Eq, Show)

instance AsRLP Block
instance AsRLP BlockHeader
instance AsRLP StackFrame
instance AsRLP Account

instance AsRLP ContractInfo where
  fromRLP (RLP.String bs) | BS.null bs = Just External
  fromRLP item = ContractRoot <$> fromRLP item
  toRLP (ContractRoot digest) = toRLP digest
  toRLP External = RLP.String BS.empty
