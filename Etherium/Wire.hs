{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Etherium.Wire() where

import Control.Monad
import qualified Data.ByteString as BS
import GHC.Generics

import Etherium.Prelude
import Etherium.Block
import Etherium.RLP.Convert
import qualified Etherium.RLP as RLP
import Etherium.Trie(Digest)

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
  deriving (Show, Generic)

instance AsRLP Payload

data Hello = Hello
  { protocolVersion :: Int
  , networkId :: Int
  , clientId :: ByteString
  , listenPort :: Int
  , capabilities :: Int
  , nodeId :: ByteString
  } deriving (Show, Generic)

instance AsRLP Hello where tag = tagInt 0x00

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

data Disconnect = Disconnect DisconnectReason
  deriving (Show)

instance AsRLP Disconnect where 
  toRLP (Disconnect reason) = toRLP [0x01, fromEnum reason]
  fromRLP (RLP.List [tag,reason]) = do
    0x01 :: Int <- fromRLP tag
    enum <- fromRLP reason
    guard $ enum >= fromEnum (minBound :: DisconnectReason)
    guard $ enum <= fromEnum (maxBound :: DisconnectReason)
    return . Disconnect $ toEnum enum
  fromRLP _ = Nothing

data Ping = Ping deriving (Show, Generic)

instance AsRLP Ping where tag = tagInt 0x02

data Pong = Pong deriving (Show, Generic)

instance AsRLP Pong where tag = tagInt 0x03


-- 'Information'

data GetPeers = GetPeers deriving (Show, Generic)

instance AsRLP GetPeers where tag = tagInt 0x10

data Peer = Peer
  { peerAddress :: ByteString
  , peerPort :: Int
  , peerUniqueId :: ByteString
  } deriving (Show, Generic)

instance AsRLP Peer

data Peers = Peers [Peer] deriving (Show, Generic)

instance AsRLP Peers where tag = tagInt 0x11

data Transactions = Transactions [Transaction] deriving (Show, Generic)

instance AsRLP Transactions where tag = tagInt 0x12

data Blocks = Blocks [Block] deriving (Show, Generic)

instance AsRLP Blocks where tag = tagInt 0x13

data GetChain = GetChain [Digest] Int deriving (Show, Generic)

instance AsRLP GetChain where
  toRLP (GetChain digests count) =
    let digestRLP = map toRLP digests
        list = [toRLP (0x14 :: Int)] ++ digestRLP ++ [toRLP count]
    in toRLP list
  fromRLP (RLP.List (tag:rest)) = do
    0x14 :: Int <- fromRLP tag
    guard $ not $ null rest
    digests <- mapM fromRLP $ init rest
    count <- fromRLP $ last rest
    return $ GetChain digests count
  fromRLP _ = Nothing  
