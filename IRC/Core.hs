module IRC.Core
(
)
where

import Control.Concurrent
import Control.Concurrent.STM.TBMChan
import Control.Monad

import Data.Conduit
import Data.Conduit.TMChan
import qualified Data.ByteString.Lazy.Char8 as B

import IRC.Parser
import IRC.Message
import IRC.Net
import IRC.Error
import IRC.Tunnel

-- | Echo pipe, sends ping periodically to the server('Tunnel'), disconnect from the server if times out.
