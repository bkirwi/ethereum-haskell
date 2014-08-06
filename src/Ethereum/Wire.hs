{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ethereum.Wire where

import Control.Error
import Control.Monad
import qualified Data.ByteString as BS
import GHC.Generics(Generic)
import qualified Data.Attoparsec.ByteString as A

import Ethereum.Prelude
import Ethereum.Block
import qualified Ethereum.RLP as RLP
import Ethereum.RLP(tagged, asProduct, asUnderlying)
import Ethereum.Trie(Digest)

data Message = 
    HelloMsg Hello
  | DisconnectMsg Disconnect
  | PingMsg Ping
  | PongMsg Pong
  | GetPeersMsg GetPeers
  | PeersMsg Peers
  | TransactionsMsg Transactions
  | BlocksMsg Blocks
  | GetChainMsg GetChain
  | NotInChainMsg NotInChain
  | GetTransactionsMsg GetTransactions
  deriving (Show, Generic)

instance RLP.Convert Message where converter = asUnderlying

-- 'Session control'

data Hello = Hello
  { protocolVersion :: Int
  , networkId :: Int
  , clientId :: ByteString
  , capabilities :: Int
  , listenPort :: Int
  , nodeId :: ByteString
  } deriving (Show, Generic)

instance RLP.Convert Hello where converter = tagged 0x00 asProduct

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
  converter = RLP.Converter (RLP.toItem . fromEnum) (maybeReason <=< RLP.fromItem)

data Disconnect = Disconnect DisconnectReason
  deriving (Show, Generic)

instance RLP.Convert Disconnect where converter = tagged 0x01 asProduct

data Ping = Ping deriving (Show, Generic)

instance RLP.Convert Ping where converter = tagged 0x02 asProduct

data Pong = Pong deriving (Show, Generic)

instance RLP.Convert Pong where converter = tagged 0x03 asProduct


-- 'Information'

data GetPeers = GetPeers deriving (Show, Generic)

instance RLP.Convert GetPeers where converter = tagged 0x10 asProduct

data Peer = Peer
  { peerAddress :: ByteString
  , peerPort :: Int
  , peerUniqueId :: ByteString
  } deriving (Show, Generic)

instance RLP.Convert Peer

data Peers = Peers [Peer] deriving (Show, Generic)

instance RLP.Convert Peers where converter = tagged 0x11 asUnderlying

data Transactions = Transactions [Transaction] deriving (Show, Generic)

instance RLP.Convert Transactions where converter = tagged 0x12 asUnderlying

data Blocks = Blocks [Block] deriving (Show, Generic)

instance RLP.Convert Blocks where converter = tagged 0x13 asUnderlying

data GetChain = GetChain [Digest] Int deriving (Show, Generic)

instance RLP.Convert GetChain where
  converter = tagged 0x14 convertGet
    where
      convertGet = RLP.Converter to from
      to (GetChain digests count) =
        let digestRLP = map RLP.toItem digests
            list = digestRLP ++ [RLP.toItem count]
        in RLP.toItem list
      from (RLP.List rest) = do
        guard $ not $ null rest
        digests <- mapM RLP.fromItem $ init rest
        count <- RLP.fromItem $ last rest
        return $ GetChain digests count
      from _ = Nothing  

data NotInChain = NotInChain Digest deriving (Show, Generic)

instance RLP.Convert NotInChain where converter = tagged 0x15 asProduct

data GetTransactions = GetTransactions deriving (Show, Generic)

instance RLP.Convert GetTransactions where converter = tagged 0x16 asProduct


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
      
