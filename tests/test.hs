import IRC.Parser
import IRC.Message as M

import System.Environment
import System.IO
import System.Exit
import Control.Monad
import Text.ParserCombinators.Parsec.Prim

parseLine ::  Handle -> IO ()
parseLine inh = hGetLine inh >>= \x ->
  case (parse (many message) "" . (++"\n")) x of
    Left _ -> putStrLn "Parse error" >> return ()
    Right m -> print $ M.command $ head m

parseLineToFile ::  Handle -> Handle -> IO ()
parseLineToFile inh outh = hGetLine inh >>= hPrint outh . parse (many message) "" . (++"\n")

main ::  IO b
main = do
  args <- getArgs
  if length args < 2
  then putStrLn "Need <input file> and <output file>" >> exitSuccess
  else
    let input = head args
        output = head $ tail args
    in
      withFile input ReadMode $ \h ->
        if head output == '-'
        then forever $ parseLine h
        else withFile output WriteMode $ \hh -> forever $ parseLineToFile h hh
