{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ethereum.Wire() where

import Control.Monad
import qualified Data.ByteString as BS
import GHC.Generics

import Ethereum.Prelude
import Ethereum.Block
import Ethereum.RLP.Convert
import qualified Ethereum.RLP as RLP
import Ethereum.Trie(Digest)

-- 'Session control'

data Payload = 
    HelloP Hello
  | DisconnectP Disconnect
  | PingP Ping
  | PongP Pong
  | GetPeersP GetPeers
  | PeersP Peers
  | TransactionsP Transactions
  | BlocksP Blocks
  | GetChainP GetChain
  | NotInChainP NotInChain
  | GetTransactionsP GetTransactions
  deriving (Show, Generic)

instance AsRLP Payload where asRLP = basic

data Hello = Hello
  { protocolVersion :: Int
  , networkId :: Int
  , clientId :: ByteString
  , listenPort :: Int
  , capabilities :: Int
  , nodeId :: ByteString
  } deriving (Show, Generic)

instance AsRLP Hello where asRLP = tagged 0x00

data DisconnectReason = 
    DisconnectRequested
  | TCPError
  | BadProtocol
  | UselessPeer
  | TooManyPeers
  | AlreadyConnected
  | WrongGenesisBlock
  | IncompatibleProtocol
  | ClientQuitting
  deriving (Show, Enum, Bounded)

maybeReason :: Int -> Maybe DisconnectReason
maybeReason n 
  | min <= n && n <= max = Just $ toEnum n
  | otherwise = Nothing
  where
    min = fromEnum (minBound :: DisconnectReason)
    max = fromEnum (maxBound :: DisconnectReason)

instance AsRLP DisconnectReason where
  asRLP = RLPConvert (toRLP . fromEnum) (maybeReason <=< fromRLP)

data Disconnect = Disconnect DisconnectReason
  deriving (Show, Generic)

instance AsRLP Disconnect where asRLP = tagged 0x01

data Ping = Ping deriving (Show, Generic)

instance AsRLP Ping where asRLP = tagged 0x02

data Pong = Pong deriving (Show, Generic)

instance AsRLP Pong where asRLP = tagged 0x03


-- 'Information'

data GetPeers = GetPeers deriving (Show, Generic)

instance AsRLP GetPeers where asRLP = tagged 0x10

data Peer = Peer
  { peerAddress :: ByteString
  , peerPort :: Int
  , peerUniqueId :: ByteString
  } deriving (Show, Generic)

instance AsRLP Peer

data Peers = Peers [Peer] deriving (Show, Generic)

instance AsRLP Peers where asRLP = basicTagged 0x11

data Transactions = Transactions [Transaction] deriving (Show, Generic)

instance AsRLP Transactions where asRLP = basicTagged 0x12

data Blocks = Blocks [Block] deriving (Show, Generic)

instance AsRLP Blocks where asRLP = basicTagged 0x13

data GetChain = GetChain [Digest] Int deriving (Show, Generic)

instance AsRLP GetChain where
  asRLP = withTag 0x14 convertGet
    where
      convertGet = RLPConvert toX fromX
      toX (GetChain digests count) =
        let digestRLP = map toRLP digests
            list = digestRLP ++ [toRLP count]
        in toRLP list
      fromX (RLP.List rest) = do
        guard $ not $ null rest
        digests <- mapM fromRLP $ init rest
        count <- fromRLP $ last rest
        return $ GetChain digests count
      fromX _ = Nothing  

data NotInChain = NotInChain Digest deriving (Show, Generic)

instance AsRLP NotInChain where asRLP = tagged 0x15

data GetTransactions = GetTransactions deriving (Show, Generic)

instance AsRLP GetTransactions where asRLP = tagged 0x16
