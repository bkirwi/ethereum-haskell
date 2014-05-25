{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Etherium.Block() where

import qualified Data.ByteString as BS
import Data.ByteString(ByteString)
import Data.Functor
import GHC.Generics

import qualified Etherium.RLP as RLP
import Etherium.RLP.Convert
import Etherium.Trie(Digest)

newtype Address = Address ByteString
  deriving (Show, AsRLP)

data Block = Block 
  { blockHeader :: BlockHeader
  , uncleList :: [BlockHeader] 
  , receiptList :: [Receipt]
  } deriving (Show, Generic)

data BlockHeader = BlockHeader
  { parentHash :: Digest          -- ^ Hp
  , uncleDigest :: Digest         -- ^ Hu
  , coinbaseAddress :: ByteString -- ^ Hb
  , blockStateRoot :: Digest      -- ^ Hr
  , transactionsRoot :: Digest    -- ^ Ht
  , difficulty :: Integer         -- ^ Hd
  , blockNumber :: Integer        -- ^ Hi
  , blockGasPrice :: Integer      -- ^ Hm
  , blockGasLimit :: Integer
  , blockGasUsed :: Integer
  , timestamp :: Integer
  , extraData :: ByteString
  , blockNonce :: Digest
  } deriving (Show, Generic)

data Account = Account
  { accountBalance :: Integer
  , accountNonce :: Integer
  , contractState :: Digest
  , contractCode :: Digest
  } deriving (Show, Generic)

data Transaction = Transaction
  { transactionNonce :: Integer
  , gasPrice :: Integer
  , gasLimit :: Integer
  , transactionTarget :: Address
  , transactionValue :: Integer
  , transactionPayload :: ByteString
  , transactionW :: Integer
  , transactionR :: Integer
  , transactionS :: Integer
  } deriving (Show, Generic)

data Receipt = Receipt
  { receiptTransaction :: Transaction
  , receiptState :: Digest
  , receiptGas :: Integer
  } deriving (Show, Generic)

instance AsRLP Block
instance AsRLP BlockHeader
instance AsRLP Account
instance AsRLP Transaction
instance AsRLP Receipt
