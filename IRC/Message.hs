module IRC.Message where

type User       = String
type Host       = String
type Command    = String
type MSG        = String
type Nickname   = String
type Servname   = String
type Middle     = String

data Message    = Message (Maybe Prefix) Command [Param]
                deriving (Show, Eq)

data Prefix     = ServPrefix Servname
                | UserPrefix Nickname (Maybe User) (Maybe UserHost)
                deriving (Show, Eq)

data Param      = Param ([Middle], Maybe String)
                deriving (Show, Eq)

data UserHost   = Hostname Host
                | UserIP IPAddr
                | GroupCloak [String]
                deriving (Show, Eq)

data IPAddr     = IPv4 String
                | IPv6 String
                deriving (Show, Eq)

