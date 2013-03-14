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
, Plugin
, PluginS
, PluginU
, PluginD
, PluginB
, PluginC
, readTBMChanS
, writeTBMChanS
, hGetLineS
, hPutStrLnD
, toMessage
, toRaw
)
where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Monoid ((<>))
import Control.Concurrent.STM.TBMChan
import Control.Monad
import Control.Monad.STM
import Control.Exception (try, SomeException(..))
import Control.Proxy hiding (Server, hGetLineS, hPutStrLnD)
import Control.Proxy.Trans.Either

import qualified Data.Attoparsec.Text as Parser
import Data.Typeable (Typeable)
import Data.Time (getZonedTime)

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
type Plugin a' a b' b = forall r p. (Proxy p) =>
    b' -> EitherP SomeException p a' a b' b IO r

{-  Receive from upstream ------------+                +----------------- Receive from downstream
                                      |                |
    Send to upstream -+               |                |               +- Send to downstream
                      |               |                |               |                   -}
type PluginS = Plugin C               ()               (Maybe Message) (Maybe TMessage)
type PluginD = Plugin ()              (Maybe TMessage) ()              (Maybe TMessage)
type PluginU = Plugin (Maybe Message) ()               (Maybe Message) ()
type PluginB = Plugin (Maybe Message) (Maybe TMessage) (Maybe Message) (Maybe TMessage)
type PluginC = Plugin (Maybe Message) (Maybe TMessage) ()              C

data Server = Server {
  sHost   :: HostName
, sPort   :: PortNumber
, sHandle :: Handle
} deriving (Show, Eq, Typeable)

{- Internal methods -}
-- ``````````````````
-- | A wrapper around STM chan for Producer, it reads from a TChan and passes it down pipeline.
readTBMChanS :: (Proxy p) =>
  TBMChan v -> () -> Producer (EitherP SomeException p) v IO ()
readTBMChanS ch () = forever $ do
  m <- lift $ atomically $ readTBMChan ch
  case m of
    Nothing -> throw $ SomeException ChannelIsClosed
    Just m' -> respond m'

-- | Write values to a TBMChan.
writeTBMChanS :: (Proxy p) =>
  TBMChan v -> () -> Consumer (EitherP SomeException p) v IO ()
writeTBMChanS ch () = forever $ do
  m <- request ()
  lift $ atomically $ writeTBMChan ch m

-- | Take lines of stream from a 'Handle' and sends down the pipe.
hGetLineS :: (Proxy p) =>
  Handle -> () ->  Producer (EitherP SomeException p) T.Text IO ()
hGetLineS h () = forever $ do
    eof <- lift $ hIsEOF h
    unless eof $ do
        str <- lift $ try $ T.hGetLine h
        case str of
          Left e -> throw e  -- Re-throw exception from IO monad
          Right m -> respond m

-- | Take lines of 'Text' stream from downstream and write it to the 'Handle'.
hPutStrLnD :: (Proxy p) =>
  Handle -> () -> Consumer (EitherP SomeException p) T.Text IO ()
hPutStrLnD h () = forever $ do
  str <- request ()
  e <- lift $ try $ T.hPutStrLn h str
  case e of
    Left e' -> throw e' -- Re-throw exceptions from IO monad
    Right m -> return m

-- | Parse each raw IRC message and send the result down in the pipeline.
-- Automatically append CRLF at the end of the message, since hGetLine won't reserve it.
toMessage :: (Proxy p) =>
  () -> EitherP SomeException p () T.Text () TMessage IO ()
toMessage () = forever $ do
  s <- request ()
  case Parser.parse message (s <> "\n") of
    -- TODO: make sure that parsed message is fully evaluated
    Parser.Done _ m   -> lift getZonedTime >>= (respond . TMessage m)
    Parser.Fail t c e -> throw $ SomeException $ ParserFail e (T.unpack t) c
    Parser.Partial _  -> throw $ SomeException ParserNeedMoreInput

-- | Convert Message to raw IRC messages.
toRaw :: (Proxy p) => () -> Pipe p Message T.Text IO ()
toRaw () = runIdentityP $ forever $ do
  m <- request ()
  respond (rawShow m)

