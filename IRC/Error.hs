{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | IRC Errors
module IRC.Error
(
  TunnelError(..)
, ParserError(..)
, IRCError(..)
, SomeException(..)
, Exception
, throwP
, throwE
)where

import Control.Exception as E
import Data.Typeable
import Control.Proxy
import Control.Proxy.Trans.Either as P

data TunnelError = TunnelIsClosed
                 | SpawnThreadFailed
                 deriving (Show, Eq, Typeable, Enum)

data ParserError = ParserFail String String [String]
                 | ParserNeedMoreInput
                 deriving (Eq, Typeable)

data IRCError = ChannelIsClosed
              deriving (Eq, Typeable, Show, Enum)

throwP :: (Monad m, Proxy p) =>e -> EitherP e p a' a b' b m r
throwP = P.throw

throwE :: forall e a. Exception e => e -> a
throwE = E.throw

instance Show ParserError where
  show (ParserFail e l c) = e ++ "\nLeftover:" ++ l ++ "\nContext:\n" ++ show c ++ "\n"
  show ParserNeedMoreInput = "Parser has returned partial result,\
      \ the data from upstream might be broken, abandonning the result."

instance Exception ParserError
instance Exception TunnelError
instance Exception IRCError
