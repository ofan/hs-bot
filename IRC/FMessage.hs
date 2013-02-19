{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverlappingInstances #-}
module IRC.FMessage where

import Data.ByteString.Char8 as B
import Data.Monoid ((<>))

type Host     = ByteString
type Command  = ByteString
type MSG      = ByteString
type Nickname = ByteString
type Servname = ByteString
type Middle   = ByteString

data User     = User { userName :: ByteString }
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
                      , trailing :: Maybe ByteString
                      }
              deriving (Eq)

data UserHost = Hostname   { host   :: Host }
              | UserIP     { ipAddr :: IPAddr }
              | GroupCloak { cloaks :: [ByteString] }
              | NullHost
              deriving (Eq)

data IPAddr   = IPv4 { ip :: ByteString }
              | IPv6 { ip :: ByteString }
              deriving (Eq)

-- Show instances
-- ``````````````
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

-- | RawShow class converts message into raw IRC message
class RawShow a where
  rawShow :: a -> ByteString

-- RawShow instances
-- `````````````````
instance RawShow Param where
  rawShow (Param m (Just t)) = B.unwords m <> ":" <> t
  rawShow (Param m Nothing)  = B.unwords m

instance RawShow Prefix where
  rawShow NullPrefix         = empty
  rawShow (ServPrefix s)     = ":" <> s
  rawShow (UserPrefix n u h) = ":" <> n <> rawShow u <> rawShow h

instance RawShow User where
  rawShow NullUser = empty
  rawShow (User u) = "!" <> u

instance RawShow UserHost where
  rawShow NullHost       = empty
  rawShow (Hostname h)   = "@" <> h
  rawShow (UserIP i)     = "@" <> ip i
  rawShow (GroupCloak c) = "@" <> intercalate "/" c

instance RawShow Command where
  rawShow = id

instance RawShow Message where
  rawShow (Message pre cmd pars) = B.unwords [rawShow pre, rawShow cmd, rawShow pars]
