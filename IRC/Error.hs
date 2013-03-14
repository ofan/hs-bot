{-# LANGUAGE DeriveDataTypeable #-}
-- | IRC Errors
module IRC.Error where

import Control.Exception
import Data.Typeable

data TunnelError = TunnelIsClosed
                 | SpawnThreadFailed
                 | ChannelIsClosed
                 deriving (Show, Eq, Typeable, Enum)

data ParserError = ParserFail String String [String]
                 | ParserNeedMoreInput
                 deriving (Eq, Typeable)

instance Show ParserError where
  show (ParserFail e l c) = e ++ "\nLeftover:" ++ l ++ "\nContext:\n" ++ show c ++ "\n"
  show ParserNeedMoreInput = "Parser has returned partial result,\
      \ the data from upstream might be broken, abandonning the result."

instance Exception ParserError
instance Exception TunnelError
