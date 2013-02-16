{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverlappingInstances #-}
module IRC.FMessage where

import Data.Text

type Host     = Text
type Command  = Text
type MSG      = Text
type Nickname = Text
type Servname = Text
type Middle   = Text

data User     = User { userName :: Text }
              | NullUser
              deriving (Eq)

data Message  = Message { prefix  :: Prefix
                        , command :: Command
                        , params  :: Param
                        }
              deriving (Eq)

data Prefix   = ServPrefix { servName :: Servname }
              | UserPrefix { nickName :: Nickname
                           , user     :: User
                           , userHost :: UserHost
                           }
              | NullPrefix
              deriving (Eq)

data Param    = Param { cmdList  :: [Middle]
                      , trailing :: Maybe Text
                      }
              deriving (Eq)

data UserHost = Hostname   { host   :: Host }
              | UserIP     { ipAddr :: IPAddr }
              | GroupCloak { cloaks :: [Text] }
              | NullHost
              deriving (Eq)

data IPAddr   = IPv4 { ip :: Text }
              | IPv6 { ip :: Text }
              deriving (Eq)

instance Show Message where
  show m = "Message {\n\t"
        ++ "Prefix : "  ++ show (prefix m)  ++ "\n\t"
        ++ "Command: "  ++ show (command m) ++ "\n\t"
        ++ "Params : "  ++ show (params m)  ++ "\n\t"
        ++ "}"

instance Show [Message] where
  show = Prelude.concatMap ((++"\n") . show)

instance Show Param where
  show (Param pars (Just tr)) = show pars ++ ", '" ++ unpack tr ++ "'"
  show (Param pars _) = show pars

instance Show Prefix where
  show (ServPrefix h) = "Hostname \"" ++ unpack h ++ "\""
  show (UserPrefix n u h) = "User " ++ unpack n ++ "(" ++ show u ++ ")" ++ "@" ++ show h
  show NullPrefix = "<empty>"

instance Show UserHost where
  show (Hostname h) = unpack h
  show (UserIP i) = unpack $ ip i
  show (GroupCloak c) = show c
  show _ = "<empty>"

instance Show IPAddr where
  show (IPv4 i) = show i
  show (IPv6 i) = show i

instance Show User where
  show (User u) = unpack u
  show NullUser = "<empty>"
