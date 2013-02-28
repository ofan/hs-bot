{-# LANGUAGE OverloadedStrings #-}
-- Copyright (C) 2013, Ryan Feng

-- | This is the parser module for IRC protocol,
-- RFC 2812 <http://www.irchelp.org/irchelp/rfc/rfc2812.txt>
module IRC.FParser
  ( command
  , hostname
  , ipv4
  , ipv6
  , message
  , nickname
  , nickPrefix
  , prefix
  , servPrefix
  , userHost
  , userIP
  , username
  )
where

import IRC.FMessage hiding
  ( userName
  , prefix
  , command
  , params
  , servName
  , nickName
  , user
  , userHost
  , host
  , ipAddr
  , cloaks
  , ip
  , trailing)
import Prelude hiding (takeWhile)
import Data.Char (isAlphaNum, isAlpha, isHexDigit)
import Control.Applicative
import Control.Monad (liftM)
import qualified Data.Text as T
import Data.Attoparsec.Text
  ( takeWhile
  , takeWhile1
  , char
  , Parser
  , many'
  , option
  , string
  , try
  , sepBy1'
  , (<?>)
  )

-- * Protocol definitions

-- ** Lexemes
-- | SPACE character in IRC protocol
space :: Parser Char
space = char ' '

-- | Digit filter
isDigit :: Char -> Bool
isDigit = (`elem` "0123456789")

-- | Parse nospcrlfcl
nospcrlfcl :: Parser T.Text
nospcrlfcl = takeWhile1 (`notElem` ":\SP\NUL\CR\LF")

-- | Trailing characters, could be empty
trailing :: Parser T.Text
trailing = takeWhile (`notElem` "\NUL\CR\LF")

-- | Middle characters, non-empty
middle :: Parser T.Text
middle = do
  x <- nospcrlfcl
  y <- takeWhile (`notElem` "\SP\NUL\CR\LF")
  return (T.append x y)

-- | Parse username
username :: Parser T.Text
username = takeWhile1 (`notElem` "\NUL\CR\LF\SP@")

-- | Nickname string, it seems like many IRC service providers tend to support unusual characters in nickname,
-- | the nickname combinator just takes all characters before '!' in the prefix.
nickname :: Parser T.Text
nickname = takeWhile1 (\x -> x `elem` "[]\\`_^{}|-" || isAlphaNum x)

-- | Three-digit command code
cmdDigits :: Parser T.Text
cmdDigits = takeWhile1 isDigit

-- | Parameter of a command
param :: Parser Param
param = do
  mid <- many' $ space >> middle
  par <- option T.empty $ string " :" >> trailing
  let par' = if T.null par then Nothing else Just par
  return $ Param mid par'

-- | CRLF sequence
crlf :: Parser T.Text
crlf = string "\CR\LF"

-- | Hostname string
hostname :: Parser Host
hostname = takeWhile $ \x -> isAlphaNum x || x == '.' || x == '-'

-- | Parse ip addresses
userIP :: Parser UserHost
userIP = liftM UserIP $ try ipv4 <|> ipv6

-- | Parse IPv4 addresses
ipv4 :: Parser IPAddr
ipv4 = do
  ip <- takeWhile1 (\x -> isDigit x || x == '.') <* space
  {-_ <- lookAhead $ try space-}
  return $ IPv4 ip

-- | Parse IPv6 addresses
ipv6 :: Parser IPAddr
ipv6 = do
  ip <- takeWhile1 (\x -> isHexDigit x || x == ':') <* space
  {-_ <- lookAhead $ try space-}
  return $ IPv6 ip

-- | Parse user's hostname
userHost :: Parser UserHost
userHost = liftM Hostname hostname <* space

-- | Parse group cloaks
groupCloaks :: Parser UserHost
groupCloaks = liftM GroupCloak $ sepBy1' hostname (char '/') <* space

-- ** Message parsing

-- | A message is the unit of the protocol when exchanging information
-- between clients and servers.
-- > message    =  [ ":" prefix SPACE ] command [ params ] crlf
message :: Parser Message
message = do
  pre <- option NullPrefix prefix
  com <- command
  par <- param
  _   <- crlf
  return $ Message pre com par

-- | The prefix of a message contains the original source of it.
-- It is optional, servers use prefix to identify themselves,
-- clients usually don't need to send prefix, if do so, a client
-- should only use its registered nick name as the prefix.
-- > prefix     =  servername / ( nickname [ [ "!" user ] "@" host ] )
prefix :: Parser Prefix
prefix = char ':' *> option NullPrefix (nickPrefix <|> servPrefix)

-- | Parse nickname prefix
nickPrefix :: Parser Prefix
nickPrefix = do
  nick <- nickname
  usr <- option NullUser $ liftM User $ char '!' >> username
  host <- char '@' *> option NullHost (userIP <|> userHost <|> groupCloaks)
  return $ UserPrefix nick usr host

-- | Parse server prefix
servPrefix :: Parser Prefix
servPrefix = liftM ServPrefix $ hostname <* space

-- | Command
command :: Parser Command
command = takeWhile1 isAlpha <|> cmdDigits <?> "a command"
