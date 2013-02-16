{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module IRC.Message where

type User       = String
type Host       = String
type Command    = String
type MSG        = String
type Nickname   = String
type Servname   = String
type Middle     = String

data Message    = Message { prefix  :: Maybe Prefix
                          , command :: Command
                          , params  :: Param
                          }
                deriving (Eq)

data Prefix     = ServPrefix { servName :: Servname }
                | UserPrefix { nickname :: Nickname
                             , user     :: Maybe User
                             , userHost :: Maybe UserHost
                             }
                deriving (Eq)

data Param      = Param { cmdList :: [Middle]
                        , trailing :: Maybe String
                        }
                deriving (Eq)

data UserHost   = Hostname   { host   :: Host }
                | UserIP     { ipAddr :: IPAddr }
                | GroupCloak { cloak  :: [String] }
                deriving (Show, Eq)

data IPAddr     = IPv4 { ipv4 :: String }
                | IPv6 { ipv6 :: String }
                deriving (Eq)

instance Show Message where
  show m = "Message {\n\t"
        ++ "Prefix : "  ++ show (prefix m)  ++ "\n\t"
        ++ "Command: "  ++ show (command m) ++ "\n\t"
        ++ "Params : "  ++ show (params m)  ++ "\n\t"
        ++ "}"

instance Show [Message] where
  show = concatMap ((++"\n") . show)

instance Show Param where
  show (Param pars (Just tr)) = show pars ++ ", '" ++ tr ++ "'"
  show (Param pars _) = show pars

instance Show (Maybe Prefix) where
  show Nothing = "<empty>"
  show (Just (ServPrefix h)) = "Hostname \"" ++ h ++ "\""
  show (Just (UserPrefix n u h)) = "User " ++ n ++ "(" ++ show u ++ ")" ++ "@" ++ show h

instance Show (Maybe User) where
  show Nothing = "<empty>"
  show (Just u) = u

instance Show (Maybe UserHost) where
  show Nothing = "<empty>"
  show (Just (Hostname h)) = h
  show (Just (UserIP ip))  = show ip
  show (Just (GroupCloak c)) = show c

instance Show IPAddr where
  show (IPv4 ip) = ip
  show (IPv6 ip) = ip

