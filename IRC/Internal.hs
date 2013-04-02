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
import System.Time (ClockTime(..), getClockTime)

import Network (HostName, PortNumber)
import System.IO (Handle, hIsEOF)

import IRC.Parser
import IRC.Message (Message, TMessage(..), rawShow)
import IRC.Error

{- Type and data declarations -}
-- ````````````````````````````
-- | The channel that the tunnel reads from
type ReadChan = TBMChan Message
-- | The channel that the tunnel writes to
type WriteChan = TBMChan TMessage

-- | A specialized Proxy type for all plugins and components that communicate with IRC servers.
type Plugin a' a b' b = forall p m r. (Proxy p, Monad m, MonadIO m, MonadIOP p) =>
  b' -> EitherP SomeException (ReaderP Server p) a' a b' b m r

{-  Receive from upstream ------------+                +----------------- Receive from downstream
                                      |                |
    Send to upstream -+               |                |               +- Send to downstream
                      |               |                |               |                   -}
type PluginS = Plugin C               ()               (Maybe Message) (Maybe TMessage)
type PluginD = Plugin ()              (Maybe TMessage) ()              (Maybe TMessage)
type PluginU = Plugin (Maybe Message) ()               (Maybe Message) ()
type PluginB = Plugin (Maybe Message) (Maybe TMessage) (Maybe Message) (Maybe TMessage)
type PluginC = Plugin (Maybe Message) (Maybe TMessage) ()              C
type PluginR = Plugin ()              (Maybe TMessage) ()              C
type PluginP = Plugin C               ()               ()              (Maybe TMessage)

data Server = Server {
  -- | Server host name
  sHost   :: HostName
  -- | Server port number
, sPort   :: PortNumber
  -- | Handle to the socket
, sHandle :: Handle
  -- | The time the connection established with the server
, sConnTime :: ClockTime
  -- | The time of last PONG command received.
, sLastPong :: IO (IORef ClockTime)
} deriving (Show, Eq)

instance Show (IO (IORef ClockTime)) where
  show _ = "<IORef ClockTime>"

instance Eq (IO (IORef ClockTime)) where
  _ == _ = True

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
  () -> EitherP SomeException p () T.Text () TMessage IO ()
toMessage () = forever $ do
  s <- request ()
  case Parser.parse message (s <> "\n") of
    -- TODO: make sure that parsed message is fully evaluated
    Parser.Done _ m   -> lift getClockTime >>= (respond . TMessage m)
    Parser.Fail t c e -> throwP $ SomeException $ ParserFail e (T.unpack t) c
    Parser.Partial _  -> throwP $ SomeException ParserNeedMoreInput

-- | Convert Message to raw IRC messages.
toRaw :: (Proxy p) => () -> Pipe p Message T.Text IO ()
toRaw () = runIdentityP $ forever $ do
  m <- request ()
  respond (rawShow m)

