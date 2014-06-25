{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ethereum.Wire(
    parseMessage, buildMessage
  , Payload(..), Hello(..), Disconnect(..)
  ) where

import Control.Error
import Control.Monad
import qualified Data.ByteString as BS
import GHC.Generics
import qualified Data.Attoparsec.ByteString as A

import Ethereum.Prelude
import Ethereum.Block
import Ethereum.RLP.Convert
import qualified Ethereum.RLP as RLP
import Ethereum.Trie(Digest)

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

instance RLP.Convert Payload where converter = basic

-- 'Session control'

data Hello = Hello
  { protocolVersion :: Int
  , networkId :: Int
  , clientId :: ByteString
  , capabilities :: Int
  , listenPort :: Int
  , nodeId :: ByteString
  } deriving (Show, Generic)

instance RLP.Convert Hello where converter = tagged 0x00

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

instance RLP.Convert DisconnectReason where
  converter = RLP.Converter (toRLP . fromEnum) (maybeReason <=< fromRLP)

data Disconnect = Disconnect DisconnectReason
  deriving (Show, Generic)

instance RLP.Convert Disconnect where converter = tagged 0x01

data Ping = Ping deriving (Show, Generic)

instance RLP.Convert Ping where converter = tagged 0x02

data Pong = Pong deriving (Show, Generic)

instance RLP.Convert Pong where converter = tagged 0x03


-- 'Information'

data GetPeers = GetPeers deriving (Show, Generic)

instance RLP.Convert GetPeers where converter = tagged 0x10

data Peer = Peer
  { peerAddress :: ByteString
  , peerPort :: Int
  , peerUniqueId :: ByteString
  } deriving (Show, Generic)

instance RLP.Convert Peer

data Peers = Peers [Peer] deriving (Show, Generic)

instance RLP.Convert Peers where converter = basicTagged 0x11

data Transactions = Transactions [Transaction] deriving (Show, Generic)

instance RLP.Convert Transactions where converter = basicTagged 0x12

data Blocks = Blocks [Block] deriving (Show, Generic)

instance RLP.Convert Blocks where converter = basicTagged 0x13

data GetChain = GetChain [Digest] Int deriving (Show, Generic)

instance RLP.Convert GetChain where
  converter = withTag 0x14 convertGet
    where
      convertGet = RLP.Converter toX fromX
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

instance RLP.Convert NotInChain where converter = tagged 0x15

data GetTransactions = GetTransactions deriving (Show, Generic)

instance RLP.Convert GetTransactions where converter = tagged 0x16


-- Serialization

serializationToken :: ByteString
serializationToken = BS.pack [0x22, 0x40, 0x08, 0x91]

parseMessage :: A.Parser ByteString
parseMessage = do
  _ <- A.string serializationToken
  sizeBytes <- A.take 4
  size <- justZ $ decodeInt $ BS.dropWhile (==0) sizeBytes
  A.take size

buildMessage :: ByteString -> ByteString
buildMessage body =
  let size = encodeInt $ BS.length body
      padding = 4 - BS.length size
      paddedSize = BS.replicate padding 0x00 <> size
  in serializationToken <> paddedSize <> body
      
