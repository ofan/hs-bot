{-# LANGUAGE OverloadedStrings #-}
import IRC.FParser
import IRC.FMessage as M

import System.Environment
import System.IO
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8 (IResult(..))
import qualified Data.ByteString.Char8 as B
import Control.Monad
import System.Exit

parseLine :: Handle -> IO ()
parseLine inh = B.hGetLine inh >>= \x ->
  case (parse (many' message) . (`B.append` "\n")) x of
    Done r res -> if (not . B.null) r
                   then putStrLn $ "Parse error, remaining" ++ B.unpack r
                   else print $ M.command $ head res
    Fail _ _ e -> putStrLn $ "Parse error: " ++ e
    _ -> putStrLn "Need more input"

parseLineToFile :: Handle -> Handle -> IO ()
parseLineToFile inh outh = B.hGetLine inh >>= hPrint outh . parse (many' message) . (`B.append` "\n")

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
        if head outf == '-'
        then forever $ parseLine h
        else withFile outf WriteMode $ \hh -> forever $ parseLineToFile h hh
