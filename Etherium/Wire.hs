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
             deriving (Show)

data Hello = Hello
  { protocolVersion :: Int
  , networkId :: Int
  , clientId :: ByteString
  , listenPort :: Int
  , capabilities :: Int
  , nodeId :: ByteString
  } deriving (Show, Generic)

data Disconnect = DisconnectRequested
                | TCPError
                | BadProtocol
                | UselessPeer
                | TooManyPeers
                | AlreadyConnected
                | WrongGenesisBlock
                | IncompatibleProtocol
                | ClientQuitting
                deriving (Show, Enum)

instance AsRLP Hello

instance AsRLP Disconnect where
  toRLP = toRLP . fromEnum
  fromRLP = fmap toEnum . fromRLP

instance AsRLP Payload where
  toRLP payload = case payload of
    HelloP h      -> encode 0x00 h
    DisconnectP d -> encode 0x01 d
    where
      encode :: AsRLP a => Int -> a -> RLP.Item
      encode n a = case toRLP a of
        RLP.List list -> RLP.List $ toRLP n : list
        str -> RLP.List [toRLP n, str]

  fromRLP (RLP.List (x:xs)) = do
    pType <- fromRLP x
    let readAs :: AsRLP a => (a -> Payload) -> Maybe Payload
        readAs toPayload = fmap toPayload . fromRLP $ RLP.List xs
    case (pType :: Int) of
      0x00 -> readAs HelloP
      0x01 -> readAs DisconnectP 
  fromRLP _ = Nothing
