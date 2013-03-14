-- Utility functions
module IRC.Util
(
  setupHandle
)
where

import System.IO

setupHandle ::  Handle -> IO Handle
setupHandle h = do
  hSetNewlineMode h (NewlineMode CRLF CRLF)
  hSetBuffering h LineBuffering
  return h
