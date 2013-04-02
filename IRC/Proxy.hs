{-# LANGUAGE FlexibleContexts #-}
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
import Control.Monad.IO.Class
import Control.Monad(when)

import Data.Maybe (isNothing)

import IRC.Internal
import IRC.Tunnel
import IRC.Message(Message, TMessage(..))

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
chanU rch = chanFilterB (Just rch) Nothing (const True) (const True)

-- | @chanD@ writes all values flowing downsream to a channel
chanD rch = chanFilterB Nothing (Just rch) (const True) (const True)

-- | @chanFilterB rch wch uf df@ redirects downstream message m to rch if @df m@ is @True@, redirects upstream message to wch if @uf m@ is @True@.
chanFilterB :: Maybe ReadChan  -- ^ Channel for upstream
           -> Maybe ReadChan   -- ^ Channel for downstream
           -> (Message -> Bool) -- ^ Downstream filter
           -> (Message -> Bool) -- ^ Upstream filter
           -> PluginB
chanFilterB uch dch uf df = foreverK go
  where go m = do
          maybe (return ()) (redirect (uch, uf)) m
          m' <- request m
          maybe (return ()) (redirect (dch, df) . msg) m'
          n <- respond m'
          go n
        redirect p m = case p of
           (Nothing, _) -> return ()
           (Just ch, f) -> when (f m) (liftIO $ writeChan ch m)

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

