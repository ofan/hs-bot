{-# LANGUAGE OverloadedStrings #-}
module IRC.Net
(
  connectToServ
, disconnectServ
)
where

import Network
import System.IO
import System.Time

import IRC.Tunnel
import IRC.Internal
import IRC.Util

connectToServ :: HostName -> PortNumber -> IO Tunnel
connectToServ host port = withSocketsDo $ do
  h <- connectTo host (PortNumber port) >>= setupHandle
  time <- getClockTime
  newTunnel (Server host port h time) 1024

disconnectServ :: Server -> IO ()
disconnectServ = hClose . sHandle
