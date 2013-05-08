{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- | Proxy definitions
module IRC.Proxy
(
  chanS
, chanC
, chanU
, chanD
, chanFilterB
, chanFilterU
, chanFilterD
, tunnelS
, pongB
, pongC
, loginB
, pongHandlerB
, printMsgB
)
where

import Control.Proxy
import Control.Monad.IO.Class
import Control.Monad(void, when)

import IRC.Internal
import IRC.Tunnel
import IRC.Message

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
chanU rch = foreverK go
  where go m = do
          case m of
            Nothing -> request m
            Just m' -> liftIO (writeChan rch m')
          n <- respond ()
          go n

-- | @chanD@ writes all values flowing downsream to a channel
chanD :: ReadChan -> PluginD
chanD rch () = forever $ do
  m <- request ()
  case m of
    Nothing -> respond Nothing
    Just m' -> liftIO $ writeChan rch m'

-- | Specialized version of 'chanFilterB'

-- | @chanFilterB rch wch uf df@ redirects downstream message m to rch if @df m@ is @True@, redirects upstream message to wch if @uf m@ is @True@.
chanFilterB :: Maybe ReadChan  -- ^ Channel for upstream
           -> Maybe ReadChan   -- ^ Channel for downstream
           -> (TMessage -> Bool) -- ^ Downstream filter
           -> (TMessage -> Bool) -- ^ Upstream filter
           -> PluginB
chanFilterB uch dch uf df = foreverK go
  where go m = do
          maybe (return ()) (redirect (uch, uf)) m
          m' <- request m
          maybe (return ()) (redirect (dch, df)) m'
          n <- respond m'
          go n
        redirect p m = case p of
           (Nothing, _) -> return ()
           (Just ch, f) -> when (f m) (liftIO $ writeChan ch m)

-- | @chanFilterU ch f@ sends all messages to ch if predicates @f m@ is true.
chanFilterU :: ReadChan -> (TMessage -> Bool) -> PluginU
chanFilterU ch f = foreverK go
  where go m = do
          maybe (request m) redirect m
          n <- respond ()
          go n
        redirect n = when (f n) (liftIO $ writeChan ch n)

-- | @chanFilterD ch f@ sends all messages to ch if predicates @f m@ is true.
chanFilterD :: ReadChan -> (TMessage -> Bool) -> PluginD
chanFilterD ch f () = forever $ do
  m <- request ()
  maybe (respond m) redirect m
  where redirect n = when (f n) (liftIO $ writeChan ch n)

-- | Echo proxy
pongB :: PluginB
pongB = foreverK go
  where go m = do
          m' <- requestMsg m
          if tMsgCmd m' == Command PING
            then liftP ask >>= go . pong
            else respondMsg (Just m') >>= go . Just
        pong h = Just $ mkPong (sHost h)

-- | The client version of Echo proxy
pongC :: PluginC
pongC () = forever $ do
  m <- requestMsg Nothing
  h <- liftP ask
  when (tMsgCmd m == Command PING) (void $ request $ Just $ mkPong (sHost h))

-- | PONG message handler
pongHandlerB :: PluginB
pongHandlerB = foreverK go
  where go m = do
          m' <- requestMsg m
          if tMsgCmd m' == Command PONG
            then liftP ask >>= liftIO . updatePongTS >> requestMsg Nothing >>= go . Just
            else respondMsg (Just m') >>= go . Just

-- | A wrapper around 'Tunnel' for Proxy Server, it will return Nothing if the tunnel is empty,
-- that is, it won't block.
tunnelS :: Tunnel -> PluginS
tunnelS t = foreverK go
  where go m = do
          _ <- case m of
                Nothing -> return Nothing
                Just m' -> liftIO (writeTunnel t m') >> respond Nothing >>= go
          inmsg <- liftIO $ readTunnel t
          n <- respond (Just inmsg)
          go n

-- | Print out all messages
printMsgB :: PluginB
printMsgB = foreverK go
  where go m = do
          case m of
            Nothing -> return ()
            Just m' -> liftIO (putStr "<- " >> rawPrint m')
          n <- request m
          case n of
            Nothing -> void $ go Nothing
            Just n' -> liftIO (putStr "-> " >> rawPrint n')
          k <- respond n
          go k

-- | Login proxy, becomes a transparent proxy after sending NICK and USER command.
loginB :: Nickname -> Username -> PluginB
loginB n u = foreverK go
  where go m = do
          m' <- request m
          case m' of
            Nothing -> void $ go Nothing
            Just mm -> isAuth mm
          n' <- respondMsg m'
          go (Just n')
        sendID = do
          _ <- request (Just $ mkNick n)
          {-liftIO $ putStrLn "Sent nick..."-}
          _ <- request (Just $ mkUser u "0" "hs-bot")
          {-liftIO $ putStrLn "Sent user..."-}
          return ()
        isAuth m = maybe (return ()) matchMsg (tMsgTrailing m)
        matchMsg m = when (m == "*** Looking up your hostname...") sendID
