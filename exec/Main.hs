{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Pipes
import qualified Pipes.Prelude as P
import qualified Data.ByteString as BS
import Pipes.Network.TCP

main :: IO ()
main = withSocketsDo $ do
  putStrLn "Hello, world!"
  serve "*" "3030" handler
  where
    handler (socket, sockAddr) = do
      putStrLn $ show sockAddr
      let bytes = fromSocket socket 4096
      runEffect $ for bytes $ \bs -> lift $ BS.putStr bs
