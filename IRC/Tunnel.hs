module IRC.Tunnel
(
  Tunnel(..)
, newTunnel
, readTunnel
, tryReadTunnel
, writeTunnel
, tryWriteTunnel
, echoThread
)
where

import Control.Concurrent.STM.TBMChan
import Control.Concurrent.STM
import Control.Concurrent
import Control.Proxy as P hiding (hGetLineS, hPutStrLnD)
import Control.Proxy.Trans.Either
import qualified Control.Exception as E
import Control.Monad

import Data.IORef
import System.Time (getClockTime, diffClockTimes, TimeDiff(..))

import IRC.Internal as II
import IRC.Message (TMessage, mkPing)
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
    r <- runProxy $ runEitherK $ hGetLineSE (sHandle server) >-> toMessage >-> timeStamp >-> writeTBMChanS rch
    case r of
      Left e -> E.throw e -- Rethrow exceptions in IO monad
      Right _ -> return ()
  wtid <- forkIO $ do
    r <- runProxy $ runEitherK $ readTBMChanS wch >-> unTimeStamp >-> toRaw >-> hPutStrLnDE (sHandle server)
    case r of
      Left e -> E.throw e -- Rethrow exceptions in IO monad
      Right _ -> return ()
  return $ Tunnel server rch wch rtid wtid

-- | Timer thread
{-timer :: Server -> ReadChan -> WriteChan -> IO ()-}
{-timer h rch wch = forever $ do-}
  {-m <- tryPeekTBMChan rch-}
  {-case m of-}
    {-Just Nothing -> return ()-}
    {-Just m' -> if (tMsgCmd == Command PONG)-}
                {-then getClockTime >>= writeIORef (sLastPong h)-}
                {-else -}

-- | Read data from a tunnel, if tunnel is empty, it will block and wait for input.
readTunnel :: Tunnel -> IO TMessage
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
writeTunnel :: Tunnel -> TMessage -> IO ()
writeTunnel t m = atomically $ writeTBMChan (tWriter t) m

-- | Write data to a tunnel without retry, return @IO True@ if data is successfully written,
-- otherwise, return @IO False@
tryWriteTunnel :: Tunnel -> TMessage -> IO Bool
tryWriteTunnel t m = do
  r <- atomically $ tryWriteTBMChan (tWriter t) m
  case r of
    Nothing -> E.throw TunnelIsClosed
    Just rr -> return rr

-- | Echo thread checks every tunnel in the list, if the sLastPong time exceeded timeout time,
-- it will close that channel and throw a ServerNoResponse exception.
echoThread :: IORef [Tunnel] -> IO ()
echoThread ts = forever $ do
  ts' <- readIORef ts
  time <- getClockTime
  mapM_ (checkServer time) ts'
  threadDelay 1000000
  putStrLn "Tick..."
  where checkServer t tn = do
          lastPing <- readIORef refLastPing
          lastPong <- readIORef refLastPong
          let diffSec = tdSec $ diffClockTimes lastPong lastPing
              in when (diffSec > 6) (throwE ServerNoResponse)
          let pingDiff = tdSec $ diffClockTimes t lastPing
              h = sHost . tServer $ tn
              in when (pingDiff >= 3) (putStrLn ("Ping " ++ h) >> writeTunnel tn (mkPing h) >> writeIORef refLastPing t)
          where refLastPing = sLastPing . tServer $ tn
                refLastPong = sLastPong . tServer $ tn

