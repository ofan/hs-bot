module IRC.Tunnel
(
  Tunnel(..)
, newTunnel
, readTunnel
, tryReadTunnel
, writeTunnel
, tryWriteTunnel
, tunnelS
)
where

import Control.Concurrent.STM.TBMChan
import Control.Concurrent.STM
import Control.Concurrent

import Control.Proxy as P hiding (hGetLineS, hPutStrLnD)
import Control.Proxy.Trans.Either
import qualified Control.Exception as E

import IRC.Internal as II
import IRC.Message (Message, TMessage)
import IRC.Error

-- | A tunnel is a connection to the server, it receives and sends message through
-- reader and writer channels.
data Tunnel = Tunnel {
  -- | Bounded connection between the tunnel and the server
  tServer       :: II.Server
  -- | Read end for downstream, write end for upstream
, tReader       :: WriteChan
  -- | Write end for downstream, write end for upstream
, tWriter       :: ReadChan
  -- | ThreadId of the parser which handles incoming messages
, tParserThread :: ThreadId
  -- | ThreadId of the sender which handles outcoming messages
, tSenderThread :: ThreadId
}

instance Eq Tunnel where
  t1 == t2 = tServer t1 == tServer t2

instance Show Tunnel where
  show t = show $ tServer t

-- | Create a singleton to a server.
-- 'newTunnel' server bufsize creates a tunnel with buffer size of bufsize.
newTunnel :: II.Server -> Int -> IO Tunnel
newTunnel server bufsize = do
  rch <- newTBMChanIO bufsize
  wch <- newTBMChanIO bufsize
  rtid <- forkIO $ do
    r <- runProxy $ runEitherK $ hGetLineS (sHandle server) >-> toMessage >-> writeTBMChanS rch
    case r of
      Left e -> E.throw e
      Right _ -> return ()
  wtid <- forkIO $ do
    r <- runProxy $ runEitherK $ readTBMChanS wch >-> toRaw >-> hPutStrLnD (sHandle server)
    case r of
      Left e -> E.throw e
      Right _ -> return ()
  return $ Tunnel server rch wch rtid wtid

-- | Read data from a tunnel, if tunnel is empty, it will block and wait for input.
readTunnel :: Tunnel -> IO  TMessage
readTunnel t = do
  m <- atomically $ readTBMChan $ tReader t
  case m of
    Nothing -> E.throw TunnelIsClosed
    Just m' -> return m'

-- | Read data from a tunnel, since it uses 'TBMChan', notice that this function won't block,
-- if the channel is empty, it will return Nothing
tryReadTunnel :: Tunnel -> IO (Maybe TMessage)
tryReadTunnel t = do
  msg <- atomically $ tryReadTBMChan $ tReader t
  case msg of
    Nothing         -> E.throw TunnelIsClosed
    Just Nothing    -> return Nothing
    Just (Just m)   -> return $ Just m

-- | Write data to a tunnel, since it uses 'TBMChan', the channel may block if it's full.
writeTunnel :: Tunnel -> Message -> IO ()
writeTunnel t m = atomically $ writeTBMChan (tWriter t) m

-- | Write data to a tunnel without retry, return @IO True@ if data is successfully written,
-- otherwise, return @IO False@
tryWriteTunnel :: Tunnel -> Message -> IO Bool
tryWriteTunnel t m = do
  r <- atomically $ tryWriteTBMChan (tWriter t) m
  case r of
    Nothing -> E.throw TunnelIsClosed
    Just rr -> return rr

-- | A wrapper around 'Tunnel' for Proxy Server.
tunnelS :: Tunnel -> II.PluginS
tunnelS t = go
  where go m = case m of
          Nothing -> do -- Client requests for new message
            inmsg <- lift $ tryReadTunnel t
            m'' <- respond inmsg
            go m''
          -- | Write the message to the channel, then send Nothing to downstream, the downstream should
          -- discard received Nothing at all times.
          Just m' -> do
            lift $ writeTunnel t m'
            n <- respond Nothing
            go n

