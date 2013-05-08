{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module IRC.Internal
(
  Server(..)
, ReadChan
, WriteChan
, Msg
-- * Proxy types
, Plugin
, PluginS
, PluginU
, PluginD
, PluginB
, PluginC
, PluginP
, PluginR
-- * Proxy primitives
, readChan
, writeChan
, readTBMChanS
, writeTBMChanS
, hGetLineSE
, hPutStrLnDE
, toMessage
, toRaw
, timeStamp
, unTimeStamp
-- * Blocking request and respond
, requestMsg
, respondMsg
-- * Helper functions
, runPlugin
-- * Utils
, updatePongTS
-- * Reexports
, module Control.Proxy.Trans.Reader
)
where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Monoid ((<>))
import Data.IORef
import Control.Concurrent.STM.TBMChan
import Control.Monad
import Control.Monad.STM
import Control.Monad.IO.Class
import qualified Control.Exception as E (try)
import Control.Proxy hiding (Server)
import Control.Proxy.Trans.Either
import Control.Proxy.Trans.Reader

import qualified Data.Attoparsec.Text as Parser
import System.Time

import Network (HostName, PortNumber)
import System.IO (Handle, hIsEOF)

import IRC.Parser
import IRC.Message (Message, TMessage(..), rawShow)
import IRC.Error

{- Type and data declarations -}
-- ````````````````````````````
-- | The channel that the tunnel reads from
type ReadChan = TBMChan TMessage
-- | The channel that the tunnel writes to
type WriteChan = TBMChan TMessage

type Msg = Maybe TMessage

-- | A specialized Proxy type for all plugins and components that communicate with IRC servers.
type Plugin a' a b' b = forall p m r. (Proxy p, Monad m, MonadIO m, MonadIOP p) =>
  b' -> EitherP SomeException (ReaderP Server p) a' a b' b m r

{- Receive from upstream -+   +------ Receive from downstream
                          |   |
    Send to upstream -+   |   |    +- Send to downstream
                      |   |   |    |                   -}
type PluginS = Plugin C   ()  Msg Msg
type PluginD = Plugin ()  Msg ()  Msg
type PluginU = Plugin Msg ()  Msg ()
type PluginB = Plugin Msg Msg Msg Msg
type PluginC = Plugin Msg Msg ()  C
type PluginR = Plugin ()  Msg ()  C
type PluginP = Plugin C   ()  ()  Msg

data Server = Server {
  -- | Server host name
  sHost   :: HostName
  -- | Server port number
, sPort   :: PortNumber
  -- | Handle to the socket
, sHandle :: Handle
  -- | The time the connection established with the server
, sConnTime :: ClockTime
  -- | The time of last PING command sent.
, sLastPing :: IORef ClockTime
  -- | The time of last PONG command received.
, sLastPong :: IORef ClockTime
} deriving (Show, Eq)

instance Show (IORef ClockTime) where
  show _ = "<IORef ClockTime>"

{- Internal methods -}
-- ``````````````````

-- | A wrapper around 'readTBMChan', it throws an exceptions if the channel is closed.
readChan :: TBMChan a -> IO a
readChan ch = do
  m <- atomically $ readTBMChan ch
  case m of
    Nothing -> throwE ChannelIsClosed
    Just m' -> return m'

-- | IO version of 'writeTBMChan'
writeChan :: TBMChan a -> a -> IO ()
writeChan ch m = atomically $ writeTBMChan ch m

-- | A wrapper around STM chan for Producer, it reads from a TChan and passes it down pipeline.
readTBMChanS :: (Proxy p) =>
  TBMChan v -> () -> Producer (EitherP SomeException p) v IO ()
readTBMChanS ch () = forever $ do
  m <- lift $ readChan ch
  respond m

-- | Write values to a TBMChan.
writeTBMChanS :: (Proxy p) =>
  TBMChan v -> () -> Consumer (EitherP SomeException p) v IO ()
writeTBMChanS ch () = forever $ do
  m <- request ()
  lift $ atomically $ writeTBMChan ch m

-- | Take lines of stream from a 'Handle' and sends down the pipe.
hGetLineSE :: (Proxy p) =>
  Handle -> () ->  Producer (EitherP SomeException p) T.Text IO ()
hGetLineSE h () = go
  where go = do
          eof <- lift $ hIsEOF h
          unless eof $ do
            str <- lift $ E.try $ T.hGetLine h
            case str of
              Left e -> throwP e  -- Re-throw exception from IO monad
              Right m -> respond m >> go
          when eof $ return ()

-- | Take lines of 'Text' stream from downstream and write it to the 'Handle'.
hPutStrLnDE :: (Proxy p) =>
  Handle -> () -> Consumer (EitherP SomeException p) T.Text IO ()
hPutStrLnDE h () = forever $ do
  str <- request ()
  e <- lift $ E.try $ T.hPutStrLn h str
  case e of
    Left e' -> throwP e' -- Re-throw exceptions from IO monad
    Right m -> return m

-- | Parse each raw IRC message and send the result down in the pipeline.
-- Automatically append CRLF at the end of the message, since hGetLine won't reserve it.
toMessage :: (Proxy p) =>
  () -> EitherP SomeException p () T.Text () Message IO ()
toMessage () = forever $ do
  s <- request ()
  case Parser.parse message (s <> "\r\n") of
    -- TODO: make sure that parsed message is fully evaluated
    Parser.Done _ m   -> respond m
    Parser.Fail t c e -> throwP $ SomeException $ ParserFail e (T.unpack t) c
    Parser.Partial _  -> throwP $ SomeException ParserNeedMoreInput

-- | Convert Message to raw IRC messages.
toRaw :: (Proxy p) => () -> Pipe p Message T.Text IO ()
toRaw () = runIdentityP $ forever $ do
  m <- request ()
  respond (rawShow m)

-- | Add timestamp to a message
timeStamp :: (Proxy p) => () -> Pipe p Message TMessage IO ()
timeStamp () = runIdentityP $ forever $ do
  m <- request ()
  lift getClockTime >>= respond . TMessage m

-- | Extract the message body ad discard timestamp
unTimeStamp :: (Proxy p) => () -> Pipe p TMessage Message IO ()
unTimeStamp () = runIdentityP $ forever $ do
  (TMessage m _) <- request ()
  respond m

-- | Update the pong timestamp
updatePongTS :: Server -> IO ()
updatePongTS h = getClockTime >>= writeIORef (sLastPong h)

-- | Request next message from upstream, and passing @k@ as argument, retrying if no next message.
requestMsg :: (Proxy p, Monad m, Monad (p Msg Msg b' b m)) => Msg -> p Msg Msg b' b m TMessage
requestMsg = go
  where go k = do
          m <- request k
          case m of
            Just m' -> return m'
            Nothing -> request Nothing >>= go

-- | Respond message to downstream, passing @k@ as argument, retrying if no next message.
respondMsg :: (Proxy p, Monad m, Monad (p a' a Msg Msg m)) => Msg -> p a' a Msg Msg m TMessage
respondMsg = go
  where go k = do
          m <- respond k
          case m of
            Just m' -> return m'
            Nothing -> respond Nothing >>= go

{- Helper functions -}
-- ``````````````````
runPlugin :: Monad m =>i-> (() -> EitherP e (ReaderP i ProxyFast) a' () () b m r) -> m (Either e r)
runPlugin s = runProxy . runReaderK s . runEitherK

