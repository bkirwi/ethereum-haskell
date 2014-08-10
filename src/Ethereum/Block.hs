{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ethereum.Block(Block, BlockHeader, Account, Transaction) where

import qualified Data.ByteString as BS
import GHC.Generics

import Ethereum.Prelude
import qualified Ethereum.RLP as RLP
import Ethereum.Trie(Digest)

newtype Address = Address ByteString
  deriving (Show, RLP.Convertible)

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
  { transactionNonce :: Integer         -- ^ Tn
  , gasPrice :: Integer                 -- ^ Tp
  , gasLimit :: Integer                 -- ^ Tg
  , transactionTarget :: Address        -- ^ Tt
  , transactionValue :: Integer         -- ^ Tv
  , transactionPayload :: ByteString    -- ^ Ti (initial code for contracts) or Td (data payload)
  , transactionW :: Integer
  , transactionR :: Integer
  , transactionS :: Integer
  } deriving (Show, Generic)

data Receipt = Receipt
  { receiptTransaction :: Transaction
  , receiptState :: Digest
  , receiptGas :: Integer
  } deriving (Show, Generic)

instance RLP.Convertible Block
instance RLP.Convertible BlockHeader
instance RLP.Convertible Account
instance RLP.Convertible Transaction
instance RLP.Convertible Receipt

type AccountDB a = DB Address Account a

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
proofOfWork = error "Section 11.5"

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

dataFee, transactionFee :: Integer
transactionFee = 500
dataFee = 5

isValidTransaction :: Account -> Transaction -> Bool
isValidTransaction sender t = 
  let dataSize = fromIntegral . BS.length $ transactionPayload t
      intrinsicGas = dataFee * dataSize + transactionFee -- g0
      v0 = gasLimit t * gasPrice t + transactionValue t
      isValidSignature = True -- Appendix F
  in transactionNonce t == accountNonce sender &&
     intrinsicGas <= gasLimit t &&
     v0 <= accountBalance sender &&
     isValidSignature
