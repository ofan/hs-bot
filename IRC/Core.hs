{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module IRC.Core
(
 loginB
,echoB
)
where

import Control.Proxy
import Control.Proxy.Trans.Reader

import IRC.Message
import IRC.Internal

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

