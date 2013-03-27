{-# LANGUAGE RankNTypes #-}
-- | Proxy definitions
module IRC.Proxy
(
  chanS
, chanC
, chanU
, chanD
, tunnelS
)
where

import Control.Proxy

import IRC.Internal
import IRC.Tunnel
import Control.Concurrent.STM.TQueue

-- | @chanS up down@ is a channel server that will write values from downstream to @up@ channel,
-- and write values from @down@ to downstream.
chanS :: ReadChan -> WriteChan -> PluginS
chanS up down = foreverK go
  where go m = case m of
          Nothing -> do -- Downstream requets for new message
            inmsg <- liftIO $ readChan down
            m' <- respond $ Just inmsg
            go m'
          Just m'' -> do
            liftIO $ writeChan up m''
            n <- respond Nothing
            go n

-- | @chanC up down@ is channel client that will write values from upstream to @down@,
-- and write values from @up@ to upstream.
chanC :: ReadChan -> WriteChan -> PluginC
chanC up down () = forever $ do
  inmsg <- liftIO $ readChan up
  m <- request $ Just inmsg
  case m of
    Nothing -> return ()
    Just m' -> liftIO $ writeChan down m'

-- | @chanU@ writes all values flowing upstream to a channel.
chanU :: ReadChan -> PluginU
chanU up = foreverK go
  where go m = do
          case m of
            Nothing -> request m
            Just m' -> liftIO $ writeChan up m'
          n <- respond ()
          go n

-- | @chanD@ writes all values flowing downsream to a channel
chanD :: WriteChan -> PluginD
chanD down () = forever $ do
  m <- request ()
  case m of
    Nothing -> respond Nothing
    Just m' -> liftIO $ writeChan down m'

-- | A wrapper around 'Tunnel' for Proxy Server, it will return Nothing if the tunnel is empty,
-- that is, it won't block.
tunnelS :: Tunnel -> PluginS
tunnelS t = foreverK go
  where go m = case m of
          Nothing -> do -- Client requests for new message
            inmsg <- liftIO $ tryReadTunnel t
            m'' <- respond inmsg
            go m''
          -- | Write the message to the channel, then send Nothing to downstream, the downstream should
          -- discard received Nothing at all times.
          Just m' -> do
            liftIO $ writeTunnel t m'
            n <- respond Nothing
            go n

