{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module IRC.Core
(
  echoC
, loginB
)
where

import Control.Monad
import Control.Proxy
import Control.Proxy.Trans.Reader

import IRC.Message
import IRC.Internal

-- | Echo pipe, sends ping periodically to the server('Tunnel'), disconnect from the server if times out.
echoC :: PluginC
echoC () = forever $ do
  h <- liftP ask
  m <- request $ Just $ mkPing (sHost h)
  case m of
    Nothing -> return ()
    Just (TMessage m' _) -> when (command m' == Command PING) $ void $ request $ Just $ mkPong (sHost h)

-- | Login proxy, becomes a transparent proxy after sending NICK and USER command.
loginB :: Nickname -> Username -> PluginB
loginB n u = \m -> do
  _ <- request (Just $ mkNick n)
  _ <- request (Just $ mkUser u "0" "*")
  go m
  where go m = do
          m' <- request m
          n' <- respond m'
          go n'

