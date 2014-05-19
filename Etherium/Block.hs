{-# LANGUAGE DeriveGeneric #-}
module Etherium.Block() where

import qualified Data.ByteString as BS
import Data.ByteString(ByteString)
import GHC.Generics

import Etherium.RLP.Convert
import Etherium.Trie(Digest)

data Block = Block BlockHeader [Transaction] [Uncle] [StackFrame]
  deriving (Show, Generic)

-- Stubs
type Transaction = ByteString
type Uncle = ByteString

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
data StackFrame = StackFrame ByteString ByteString
  deriving (Show, Generic)

data Account = Account
  { accountBalance :: Integer
  , accountNonce :: Integer
  , contractRoot :: Maybe Digest
  } deriving (Show, Generic)

instance AsRLP Block
instance AsRLP BlockHeader
instance AsRLP StackFrame
instance AsRLP Account
