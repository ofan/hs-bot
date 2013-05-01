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

import Data.IORef

import IRC.Tunnel
import IRC.Internal
import IRC.Util

connectToServ :: HostName -> PortNumber -> IO Tunnel
connectToServ host port = withSocketsDo $ do
  h <- connectTo host (PortNumber port) >>= setupHandle
  time <- getClockTime
  lastPongTime <- newIORef time
  lastPingTime <- newIORef time
  newTunnel (Server host port h time lastPingTime lastPongTime) 1024

disconnectServ :: Server -> IO ()
disconnectServ = hClose . sHandle
