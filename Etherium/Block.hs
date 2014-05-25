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

type Timestamp = Integer

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
  , blockDifficulty :: Integer    -- ^ Hd
  , blockNumber :: Integer        -- ^ Hi
  , blockGasPrice :: Integer      -- ^ Hm
  , blockGasLimit :: Integer      -- ^ Hl
  , blockGasUsed :: Integer       -- ^ Hu
  , blockTimestamp :: Timestamp   -- ^ Hs
  , extraData :: ByteString       -- ^ Hx
  , blockNonce :: Digest          -- ^ Hn
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

nextDifficulty :: BlockHeader
              -> Timestamp
              -> Integer
nextDifficulty parent ts
  | ts < blockTimestamp parent + 42 = previous + delta
  | otherwise = previous - delta
  where
    previous = blockDifficulty parent
    delta = previous `div` 1024
  
nextGasLimit :: BlockHeader -> Integer
nextGasLimit parent = max limit 10000
  where 
    limit = (1023 * blockGasLimit parent + 6 * blockGasUsed parent `div` 5) `div` 1024

proofOfWork :: BlockHeader
            -> Integer
proofOfWork = error "not implemented" -- yellow paper S 11.5

isValidBlock :: BlockHeader
             -> BlockHeader
             -> Bool
isValidBlock parent current =
  let difficulty = blockDifficulty current
      timestamp = blockTimestamp current
  in proofOfWork current <= (2 ^ 256 `div` difficulty) &&
     difficulty == nextDifficulty parent timestamp &&
     blockGasLimit current == nextGasLimit parent &&
     timestamp > blockTimestamp parent &&
     BS.length (extraData current) <= 1024
