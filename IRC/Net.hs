{-# LANGUAGE OverloadedStrings #-}
module IRC.Net
(
  connectToServ
)
where

import Network
import System.IO

connectToServ :: HostName -> PortNumber -> IO Tunnel
connectToServ host port = withSocketsDo $ do
  h <- connectTo host (PortNumber port) >>= setupHandle
  return $ newTunnel (Server host port h) 1024
