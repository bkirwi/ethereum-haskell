{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent.Async
import Control.Monad.Error
import Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import Options.Applicative
import Pipes as P
import qualified Pipes.Lift as P
import qualified Pipes.Prelude as P
import qualified Pipes.Attoparsec as PA
import Pipes.Network.TCP

import Ethereum.Prelude
import Ethereum.Wire

main :: IO ()
main = withSocketsDo $ do
  options <- execParser opts 
  let hello = Hello
        { protocolVersion = 0
        , networkId = 0
        , clientId = "https://github.com/bkirwin/etherium-haskell v0.1.0.0"
        , capabilities = 0
        , listenPort = read $ port options
        , nodeId = BS.replicate 64 0
        }
      handle = handler $ HelloMsg hello
  svr <- async $ server handle $ port options
  mapM_ (async . makeClient handle) $ seeds options
  wait svr
  where
    parser = Options
      <$> strOption (long "port" <> value "30303")
      <*> many (strOption (long "bootstrap-host"))
    opts = info parser mempty

data Options = Options
  { port :: String
  , seeds :: [String]
  } deriving (Show)

server :: Handler -> String -> IO ()
server handler port = do
  putStrLn $ "Launching server on port " <> port
  serve "*" port handler

makeClient :: Handler -> String -> IO ()
makeClient handler hostPort = do
  let (host, _ : port) = break (== ':') hostPort
  putStrLn $ "Connecting as client to " <> host <> " at " <> port
  connect host port handler

type Handler = (Socket, SockAddr) -> IO ()

handler :: Message -> Handler
handler hello (socket, sockAddr) = do
  putStrLn $ "Recieved connection from " <> show sockAddr
  let bytes = fromSocket socket 4096
      messages = PA.parsed parseMessage bytes
      runInput = runEffect $ for messages $ \msg -> liftIO $ BS.putStr msg
  runInput
  return ()
  
  {-
  case exit of
    Left (err, _) -> putStrLn $ show err
    Right () -> putStrLn $ "Connection closed: " <> show sockAddr
    -}
