{-# LANGUAGE OverloadedStrings #-}
import IRC.FParser
import IRC.FMessage as M

import System.Environment
import System.IO
import Data.Attoparsec.Text (IResult(..), parse)
import qualified Data.Text as T
import qualified Data.Text.IO as TO
import Control.Monad
import System.Exit

parseLine :: Handle -> IO ()
parseLine inh = TO.hGetLine inh >>= \x ->
  case parse message x of
    Done r res -> if (not . T.null) r
                   then putStrLn $ "Parse error, remaining" ++ T.unpack r
                   else TO.putStrLn $ M.rawShow $ M.command res
    Fail _ _ e -> putStrLn $ "Parse error: " ++ e
    _ -> putStrLn "Need more input"

parseLineToFile :: Handle -> Handle -> IO ()
parseLineToFile inh outh =
  liftM (parse message . (`T.append` "\r\n")) (TO.hGetLine inh) >>=
    \x -> case x of
      Done _ r -> TO.hPutStrLn outh $ rawShow r
      Fail _ _ e -> putStrLn ("Parse error: " ++ e) >> exitFailure
      _ -> putStrLn "Need more input" >> exitFailure

main ::  IO ()
main = do
  args <- getArgs
  if length args < 2
    then putStrLn "Need <input file> and <output file>" >> exitSuccess
    else
      let inf = head args
          outf = head $ tail args
      in
        withFile inf ReadMode $ \h ->
          hSetNewlineMode h (NewlineMode CRLF CRLF) >>
          if head outf == '-'
          then forever $ parseLine h
          else withFile outf WriteMode $ \hh -> do
            hSetNewlineMode hh (NewlineMode CRLF CRLF)
            forever $ parseLineToFile h hh
