{-# LANGUAGE DeriveGeneric #-}
module Etherium.Wire() where

import qualified Data.ByteString as BS
import Data.ByteString(ByteString)
import Data.Functor
import GHC.Generics

import Etherium.RLP.Convert
import qualified Etherium.RLP as RLP
import Etherium.Trie(Digest)

data Payload = HelloP Hello
             | DisconnectP Disconnect
             | PingP Ping
             | PongP Pong
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

data DisconnectReason = DisconnectRequested
                      | TCPError
                      | BadProtocol
                      | UselessPeer
                      | TooManyPeers
                      | AlreadyConnected
                      | WrongGenesisBlock
                      | IncompatibleProtocol
                      | ClientQuitting
                      deriving (Show, Enum)

instance AsRLP DisconnectReason where
  toRLP = toRLP . fromEnum
  fromRLP = fmap toEnum . fromRLP

data Disconnect = Disconnect (Maybe DisconnectReason)
  deriving (Show, Generic)

instance AsRLP Disconnect where tag = tagInt 0x01

data Ping = Ping deriving (Show, Generic)

instance AsRLP Ping where tag = tagInt 0x02

data Pong = Pong deriving (Show, Generic)

instance AsRLP Pong where tag = tagInt 0x03
