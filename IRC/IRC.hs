-- Copyright (C) 2013, Ryan Feng

-- | This is the parser module for IRC protocol,
-- RFC 2812 <http://www.irchelp.org/irchelp/rfc/rfc2812.txt>
module IRC where

import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Char hiding (space)
import Control.Monad (liftM, liftM2)

type User       = String
type Host       = String
type Command    = String
type MSG        = String
type Nickname   = String
type Servname   = String

data Message    = Message (Maybe Prefix) Command [Param]
                deriving (Show, Eq)

data Prefix     = ServPrefix Servname
                | UserPrefix Nickname (Maybe User) (Maybe UserHost)
                deriving (Show, Eq)

data Param      = Param String
                deriving (Show, Eq)

data UserHost   = Hostname Host
                | UserIP IPAddr
                | GroupCloak [String]
                deriving (Show, Eq)

data IPAddr     = IPv4 String
                | IPv6 String
                deriving (Show, Eq)

-- * Protocol definitions

-- ** Lexemes
-- | SPACE character in IRC protocol
space :: Parser Char
space = char ' '

-- | Trailing characters, could be empty
trailing :: Parser String
trailing = many1 $ noneOf "\NUL\CR\LF"

-- | Middle characters, non-empty
middle :: Parser String
middle = liftM2 (:) (noneOf ":\SP\NUL\CR\LF") $
  many $ noneOf "\SP\NUL\CR\LF"

-- | Special characters
special :: Parser Char
special = oneOf "[]\\`_^{|}"

-- | Parse username
username :: Parser String
username = many1 $ noneOf "\NUL\CR\LF\SP@"

-- | Nickname string
nickname :: Parser String
nickname = liftM2 (:) (letter <|> special) $
  many (letter <|> special <|> char '-')

-- | Three-digit command code
cmdDigits :: Parser String
cmdDigits = sequence [digit, digit, digit]

-- | Parameter of a command
param :: Parser Param
param = do
  _ <- space
  par <- option "" $ try (char ':' >> trailing) <|> middle
  return $ Param par

-- | CRLF sequence
crlf :: Parser String
crlf = string "\CR\LF"

-- | Hostname string
hostname :: Parser Host
hostname = liftM2 (:) alphaNum $ many (alphaNum <|> oneOf ".-")

-- | Parse ip addresses
userIP :: Parser UserHost
userIP = liftM UserIP $ try ipv4 <|> ipv6

-- | Parse IPv4 addresses
ipv4 :: Parser IPAddr
ipv4 = do
  ip <- many1 (digit <|> char '.')
  _ <- lookAhead $ try space
  return $ IPv4 ip

-- | Parse IPv6 addresses
ipv6 :: Parser IPAddr
ipv6 = do
  ip <- many1 (hexDigit <|> char ':')
  _ <- lookAhead $ try space
  return $ IPv6 ip

-- | Parse user's hostname
userHost :: Parser UserHost
userHost = liftM Hostname hostname

-- | Parse group cloaks
groupCloaks :: Parser UserHost
groupCloaks = liftM GroupCloak $ sepBy1 hostname (char '/')

-- ** Message parsing

-- | A message is the unit of the protocol when exchanging information
-- between clients and servers.
-- > message    =  [ ":" prefix SPACE ] command [ params ] crlf
message :: Parser Message
message = do
  pre <- optionMaybe prefix
  com <- command
  par <- manyTill param crlf
  return $ Message pre com par

-- | The prefix of a message contains the original source of it.
-- It is optional, servers use prefix to identify themselves,
-- clients usually don't need to send prefix, if do so, a client
-- should only use its registered nick name as the prefix.
-- > prefix     =  servername / ( nickname [ [ "!" user ] "@" host ] )
prefix :: Parser Prefix
prefix = do
  _ <- char ':'
  pre <- try nickPrefix <|> servPrefix
  _ <- space
  return pre

-- | Parse nickname prefix
nickPrefix :: Parser Prefix
nickPrefix = do
  nick <- nickname
  usr <- optionMaybe (char '!' >> username)
  host <- char '@' >> optionMaybe (try userIP <|> try groupCloaks <|> userHost)
  return $ UserPrefix nick usr host

-- | Parse server prefix
servPrefix :: Parser Prefix
servPrefix = liftM ServPrefix hostname

-- | Command
command :: Parser Command
command = try (many1 letter) <|> cmdDigits <?> "a command"
