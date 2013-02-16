{-# LANGUAGE OverloadedStrings #-}
import IRC.FParser
import IRC.FMessage as M

import System.Environment
import System.IO
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text.Lazy (Result(..))
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as I
import Control.Monad
import System.Exit

parseLine :: Handle -> IO ()
parseLine inh = I.hGetLine inh >>= \x ->
  case (parse (many' message) . (`L.append` "\n")) x of
    Done r res -> if (not . L.null) r
                   then putStrLn $ "Parse error, remaining" ++ L.unpack r
                   else print $ M.command $ head res
    _ -> putStrLn "Need more input"

parseLineToFile :: Handle -> Handle -> IO ()
parseLineToFile inh outh = I.hGetLine inh >>= hPrint outh . parse (many' message) . (`L.append` "\n")

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
