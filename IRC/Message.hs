{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverlappingInstances #-}
module IRC.Message
(
-- * Message types
  Host
, Msg
, Nickname
, Servname
, Username
, Middle
, Channel
, Mask
, Mode
, User(..)
, Command(..)
, Message(..)
, TMessage(..)
, Prefix(..)
, Param(..)
, UserHost(..)
, IPAddr(..)
, CommandCode(..)
, MsgTarget
-- * Helper functions
, commandCodeMap
, rawShow
, clientMessage
, msgCmd
, msgServ
, msgNick
, msgUser
, msgHost
, msgCmdList
, msgTrailing
, msgHostName
, msgUserIP
, msgUserIPAddr
, msgCloak
, tMsgCmd
, tMsgServ
, tMsgNick
, tMsgUser
, tMsgHost
, tMsgCmdList
, tMsgTrailing
, tMsgHostName
, tMsgUserIP
, tMsgUserIPAddr
, tMsgCloak
-- * Message Constructors
, mkNames
, mkUser
, mkWhowas
, mkList
, mkWhois
, mkInfo
, mkIsOn
, mkKick
, mkNamesOne
, mkNick
, mkUserHost
, mkVersion
, mkAway
, mkChannelMode
, mkInvite
, mkIsOnOne
, mkJoin
, mkKickOne
, mkMOTD
, mkNotice
, mkOper
, mkPart
, mkPing
, mkPong
, mkPass
, mkPrivMsg
, mkQuit
, mkTopic
, mkTime
, mkUserMode
, mkUserHostOne
, mkWhowasOne
, mkListOne
, mkWho
, mkWhoisOne
-- * Helper functions
, rawPrint
)
where

import Data.Time.Exts.Unix (UnixDateTimeMillis(..))
import Data.Text as T
import Data.Text.IO as T
import qualified Data.HashMap.Strict as M
import Data.Monoid ((<>))
import Network (HostName)

type Host     = Text
type Msg      = Text
type Nickname = Text
type Servname = Text
type Username = Text
type Middle   = Text
type Channel  = Text
type Mask     = Text
type Mode     = Text

-- | A helper type alias that constructs a client message with a fake timestamp, a client message has no prefix.
clientMessage ::  Command -> Param -> TMessage
clientMessage c p = addTimeStamp $ Message NullPrefix c p


data Command  = Command CommandCode
              | Unknown Text
              deriving (Show, Eq)

data User     = User { userName :: Text }
              | NullUser
              deriving (Eq)

data Message  = Message { prefix    :: Prefix
                        , command   :: Command
                        , params    :: Param
                        }
              deriving (Eq)

data TMessage = TMessage { msg       :: Message
                         , timestamp :: UnixDateTimeMillis
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

data MsgTarget = UserTarget User
               | ChanTarget Text
               deriving (Eq, Show)

data CommandCode  = USER
                  | NICK
                  | PASS
                  | OPER
                  | MODE
                  | SERVICE
                  | QUIT
                  | SQUIT
                  | JOIN
                  | PART
                  | TOPIC
                  | NAMES
                  | LIST
                  | INVITE
                  | KICK
                  | PRIVMSG
                  | NOTICE
                  | MOTD
                  | LUSERS
                  | VERSION
                  | STATS
                  | LINKS
                  | TIME
                  | CONNECT
                  | TRACE
                  | ADMIN
                  | INFO
                  | SERVLIST
                  | SQUERY
                  | WHO
                  | WHOIS
                  | WHOWAS
                  | KILL
                  | PING
                  | PONG
                  | ERROR
                  | AWAY
                  | REHASH
                  | DIE
                  | RESTART
                  | SUMMON
                  | USERS
                  | WALLOPS
                  | USERHOST
                  | ISON
                  | RPL_WELCOME
                  | RPL_YOURHOST
                  | RPL_CREATED
                  | RPL_MYINFO
                  | RPL_BOUNCE
                  | RPL_USERHOST
                  | RPL_ISON
                  | RPL_AWAY
                  | RPL_UNAWAY
                  | RPL_NOWAWAY
                  | RPL_WHOISUSER
                  | RPL_WHOISSERVER
                  | RPL_WHOISOPERATOR
                  | RPL_WHOISIDLE
                  | RPL_WHOISACCOUNT
                  | RPL_WHOISSECURE
                  | RPL_WHOISCERT
                  | RPL_WHOISFROM
                  | RPL_ENDOFWHOIS
                  | RPL_WHOISCHANNELS
                  | RPL_WHOWASUSER
                  | RPL_ENDOFWHOWAS
                  | RPL_LISTSTART
                  | RPL_LIST
                  | RPL_LISTEND
                  | RPL_UNIQOPIS
                  | RPL_CHANNELMODEIS
                  | RPL_NOTOPIC
                  | RPL_TOPIC
                  | RPL_INVITING
                  | RPL_SUMMONING
                  | RPL_INVITELIST
                  | RPL_ENDOFINVITELIST
                  | RPL_EXCEPTLIST
                  | RPL_ENDOFEXCEPTLIST
                  | RPL_VERSION
                  | RPL_WHOREPLY
                  | RPL_ENDOFWHO
                  | RPL_NAMREPLY
                  | RPL_ENDOFNAMES
                  | RPL_LINKS
                  | RPL_ENDOFLINKS
                  | RPL_BANLIST
                  | RPL_ENDOFBANLIST
                  | RPL_INFO
                  | RPL_ENDOFINFO
                  | RPL_MOTDSTART
                  | RPL_MOTD
                  | RPL_ENDOFMOTD
                  | RPL_YOUREOPER
                  | RPL_REHASHING
                  | RPL_YOURESERVICE
                  | RPL_TIME
                  | RPL_USERSSTART
                  | RPL_USERS
                  | RPL_ENDOFUSERS
                  | RPL_NOUSERS
                  | RPL_TRACELINK
                  | RPL_TRACECONNECTING
                  | RPL_TRACEHANDSHAKE
                  | RPL_TRACEUNKNOWN
                  | RPL_TRACEOPERATOR
                  | RPL_TRACEUSER
                  | RPL_TRACESERVER
                  | RPL_TRACESERVICE
                  | RPL_TRACENEWTYPE
                  | RPL_TRACECLASS
                  | RPL_TRACERECONNECT
                  | RPL_TRACELOG
                  | RPL_TRACEEND
                  | RPL_STATSLINKINFO
                  | RPL_STATSCOMMANDS
                  | RPL_ENDOFSTATS
                  | RPL_STATSUPTIME
                  | RPL_STATSOLINE
                  | RPL_UMODEIS
                  | RPL_SERVLIST
                  | RPL_SERVLISTEND
                  | RPL_LUSERCLIENT
                  | RPL_LUSEROP
                  | RPL_LUSERUNKNOWN
                  | RPL_LUSERCHANNELS
                  | RPL_LUSERME
                  | RPL_ADMINME
                  | RPL_ADMINLOC1
                  | RPL_ADMINLOC2
                  | RPL_ADMINEMAIL
                  | RPL_TRYAGAIN
                  | ERR_NOSUCHNICK
                  | ERR_NOSUCHSERVER
                  | ERR_NOSUCHCHANNEL
                  | ERR_CANNOTSENDTOCHAN
                  | ERR_TOOMANYCHANNELS
                  | ERR_WASNOSUCHNICK
                  | ERR_TOOMANYTARGETS
                  | ERR_NOSUCHSERVICE
                  | ERR_NOORIGIN
                  | ERR_NORECIPIENT
                  | ERR_NOTEXTTOSEND
                  | ERR_NOTOPLEVEL
                  | ERR_WILDTOPLEVEL
                  | ERR_BADMASK
                  | ERR_UNKNOWNCOMMAND
                  | ERR_NOMOTD
                  | ERR_NOADMININFO
                  | ERR_FILEERROR
                  | ERR_NONICKNAMEGIVEN
                  | ERR_ERRONEUSNICKNAME
                  | ERR_NICKNAMEINUSE
                  | ERR_NICKCOLLISION
                  | ERR_UNAVAILRESOURCE
                  | ERR_USERNOTINCHANNEL
                  | ERR_NOTONCHANNEL
                  | ERR_USERONCHANNEL
                  | ERR_NOLOGIN
                  | ERR_SUMMONDISABLED
                  | ERR_USERSDISABLED
                  | ERR_NOTREGISTERED
                  | ERR_NEEDMOREPARAMS
                  | ERR_ALREADYREGISTRED
                  | ERR_NOPERMFORHOST
                  | ERR_PASSWDMISMATCH
                  | ERR_YOUREBANNEDCREEP
                  | ERR_YOUWILLBEBANNED
                  | ERR_KEYSET
                  | ERR_CHANNELISFULL
                  | ERR_UNKNOWNMODE
                  | ERR_INVITEONLYCHAN
                  | ERR_BANNEDFROMCHAN
                  | ERR_BADCHANNELKEY
                  | ERR_BADCHANMASK
                  | ERR_NOCHANMODES
                  | ERR_BANLISTFULL
                  | ERR_NOPRIVILEGES
                  | ERR_CHANOPRIVSNEEDED
                  | ERR_CANTKILLSERVER
                  | ERR_RESTRICTED
                  | ERR_UNIQOPPRIVSNEEDED
                  | ERR_NOOPERHOST
                  | ERR_UMODEUNKNOWNFLAG
                  | ERR_USERSDONTMATCH
                  deriving (Eq, Enum, Show)

-- | Command code list
commandCodeList :: [(T.Text, CommandCode)]
commandCodeList = [("USER",     USER)
                  ,("NICK",     NICK)
                  ,("PASS",     PASS)
                  ,("MODE",     MODE)
                  ,("OPER",     OPER)
                  ,("SERVICE",  SERVICE)
                  ,("QUIT",     QUIT)
                  ,("SQUIT",    SQUIT)
                  ,("JOIN",     JOIN)
                  ,("PART",     PART)
                  ,("TOPIC",    TOPIC)
                  ,("NAMES",    NAMES)
                  ,("LIST",     LIST)
                  ,("INVITE",   INVITE)
                  ,("KICK",     KICK)
                  ,("PRIVMSG",  PRIVMSG)
                  ,("NOTICE",   NOTICE)
                  ,("MOTD",     MOTD)
                  ,("LUSERS",   LUSERS)
                  ,("VERSION",  VERSION)
                  ,("STATS",    STATS)
                  ,("LINKS",    LINKS)
                  ,("TIME",     TIME)
                  ,("CONNECT",  CONNECT)
                  ,("TRACE",    TRACE)
                  ,("ADMIN",    ADMIN)
                  ,("INFO",     INFO)
                  ,("SERVLIST", SERVLIST)
                  ,("SQUERY",   SQUERY)
                  ,("WHO",      WHO)
                  ,("WHOIS",    WHOIS)
                  ,("WHOWAS",   WHOWAS)
                  ,("KILL",     KILL)
                  ,("PING",     PING)
                  ,("PONG",     PONG)
                  ,("ERROR",    ERROR)
                  ,("AWAY",     AWAY)
                  ,("REHASH",   REHASH)
                  ,("DIE",      DIE)
                  ,("RESTART",  RESTART)
                  ,("SUMMON",   SUMMON)
                  ,("USERS",    USERS)
                  ,("WALLOPS",  WALLOPS)
                  ,("USERHOST", USERHOST)
                  ,("ISON",     ISON)
                  ,("001",      RPL_WELCOME)
                  ,("002",      RPL_YOURHOST)
                  ,("003",      RPL_CREATED)
                  ,("004",      RPL_MYINFO)
                  ,("005",      RPL_BOUNCE)
                  ,("302",      RPL_USERHOST)
                  ,("303",      RPL_ISON)
                  ,("301",      RPL_AWAY)
                  ,("305",      RPL_UNAWAY)
                  ,("306",      RPL_NOWAWAY)
                  ,("311",      RPL_WHOISUSER)
                  ,("312",      RPL_WHOISSERVER)
                  ,("313",      RPL_WHOISOPERATOR)
                  ,("317",      RPL_WHOISIDLE)
                  ,("671",      RPL_WHOISSECURE)
                  ,("276",      RPL_WHOISCERT)
                  ,("378",      RPL_WHOISFROM)
                  ,("330",      RPL_WHOISACCOUNT)
                  ,("318",      RPL_ENDOFWHOIS)
                  ,("319",      RPL_WHOISCHANNELS)
                  ,("314",      RPL_WHOWASUSER)
                  ,("369",      RPL_ENDOFWHOWAS)
                  ,("321",      RPL_LISTSTART)
                  ,("322",      RPL_LIST)
                  ,("323",      RPL_LISTEND)
                  ,("325",      RPL_UNIQOPIS)
                  ,("324",      RPL_CHANNELMODEIS)
                  ,("331",      RPL_NOTOPIC)
                  ,("332",      RPL_TOPIC)
                  ,("341",      RPL_INVITING)
                  ,("342",      RPL_SUMMONING)
                  ,("346",      RPL_INVITELIST)
                  ,("347",      RPL_ENDOFINVITELIST)
                  ,("348",      RPL_EXCEPTLIST)
                  ,("349",      RPL_ENDOFEXCEPTLIST)
                  ,("351",      RPL_VERSION)
                  ,("352",      RPL_WHOREPLY)
                  ,("315",      RPL_ENDOFWHO)
                  ,("353",      RPL_NAMREPLY)
                  ,("366",      RPL_ENDOFNAMES)
                  ,("364",      RPL_LINKS)
                  ,("365",      RPL_ENDOFLINKS)
                  ,("367",      RPL_BANLIST)
                  ,("368",      RPL_ENDOFBANLIST)
                  ,("371",      RPL_INFO)
                  ,("374",      RPL_ENDOFINFO)
                  ,("375",      RPL_MOTDSTART)
                  ,("372",      RPL_MOTD)
                  ,("376",      RPL_ENDOFMOTD)
                  ,("381",      RPL_YOUREOPER)
                  ,("382",      RPL_REHASHING)
                  ,("383",      RPL_YOURESERVICE)
                  ,("391",      RPL_TIME)
                  ,("392",      RPL_USERSSTART)
                  ,("393",      RPL_USERS)
                  ,("394",      RPL_ENDOFUSERS)
                  ,("395",      RPL_NOUSERS)
                  ,("200",      RPL_TRACELINK)
                  ,("201",      RPL_TRACECONNECTING)
                  ,("202",      RPL_TRACEHANDSHAKE)
                  ,("203",      RPL_TRACEUNKNOWN)
                  ,("204",      RPL_TRACEOPERATOR)
                  ,("205",      RPL_TRACEUSER)
                  ,("206",      RPL_TRACESERVER)
                  ,("207",      RPL_TRACESERVICE)
                  ,("208",      RPL_TRACENEWTYPE)
                  ,("209",      RPL_TRACECLASS)
                  ,("210",      RPL_TRACERECONNECT)
                  ,("261",      RPL_TRACELOG)
                  ,("262",      RPL_TRACEEND)
                  ,("211",      RPL_STATSLINKINFO)
                  ,("212",      RPL_STATSCOMMANDS)
                  ,("219",      RPL_ENDOFSTATS)
                  ,("242",      RPL_STATSUPTIME)
                  ,("243",      RPL_STATSOLINE)
                  ,("221",      RPL_UMODEIS)
                  ,("234",      RPL_SERVLIST)
                  ,("235",      RPL_SERVLISTEND)
                  ,("251",      RPL_LUSERCLIENT)
                  ,("252",      RPL_LUSEROP)
                  ,("253",      RPL_LUSERUNKNOWN)
                  ,("254",      RPL_LUSERCHANNELS)
                  ,("255",      RPL_LUSERME)
                  ,("256",      RPL_ADMINME)
                  ,("257",      RPL_ADMINLOC1)
                  ,("258",      RPL_ADMINLOC2)
                  ,("259",      RPL_ADMINEMAIL)
                  ,("401",      ERR_NOSUCHNICK)
                  ,("402",      ERR_NOSUCHSERVER)
                  ,("403",      ERR_NOSUCHCHANNEL)
                  ,("404",      ERR_CANNOTSENDTOCHAN)
                  ,("405",      ERR_TOOMANYCHANNELS)
                  ,("406",      ERR_WASNOSUCHNICK)
                  ,("407",      ERR_TOOMANYTARGETS)
                  ,("408",      ERR_NOSUCHSERVICE)
                  ,("409",      ERR_NOORIGIN)
                  ,("411",      ERR_NORECIPIENT)
                  ,("412",      ERR_NOTEXTTOSEND)
                  ,("413",      ERR_NOTOPLEVEL)
                  ,("414",      ERR_WILDTOPLEVEL)
                  ,("415",      ERR_BADMASK)
                  ,("421",      ERR_UNKNOWNCOMMAND)
                  ,("422",      ERR_NOMOTD)
                  ,("423",      ERR_NOADMININFO)
                  ,("424",      ERR_FILEERROR)
                  ,("431",      ERR_NONICKNAMEGIVEN)
                  ,("432",      ERR_ERRONEUSNICKNAME)
                  ,("433",      ERR_NICKNAMEINUSE)
                  ,("436",      ERR_NICKCOLLISION)
                  ,("437",      ERR_UNAVAILRESOURCE)
                  ,("441",      ERR_USERNOTINCHANNEL)
                  ,("442",      ERR_NOTONCHANNEL)
                  ,("443",      ERR_USERONCHANNEL)
                  ,("444",      ERR_NOLOGIN)
                  ,("445",      ERR_SUMMONDISABLED)
                  ,("446",      ERR_USERSDISABLED)
                  ,("451",      ERR_NOTREGISTERED)
                  ,("461",      ERR_NEEDMOREPARAMS)
                  ,("462",      ERR_ALREADYREGISTRED)
                  ,("463",      ERR_NOPERMFORHOST)
                  ,("464",      ERR_PASSWDMISMATCH)
                  ,("465",      ERR_YOUREBANNEDCREEP)
                  ,("466",      ERR_YOUWILLBEBANNED)
                  ,("467",      ERR_KEYSET)
                  ,("471",      ERR_CHANNELISFULL)
                  ,("472",      ERR_UNKNOWNMODE)
                  ,("473",      ERR_INVITEONLYCHAN)
                  ,("474",      ERR_BANNEDFROMCHAN)
                  ,("475",      ERR_BADCHANNELKEY)
                  ,("476",      ERR_BADCHANMASK)
                  ,("477",      ERR_NOCHANMODES)
                  ,("478",      ERR_BANLISTFULL)
                  ,("481",      ERR_NOPRIVILEGES)
                  ,("482",      ERR_CHANOPRIVSNEEDED)
                  ,("483",      ERR_CANTKILLSERVER)
                  ,("484",      ERR_RESTRICTED)
                  ,("485",      ERR_UNIQOPPRIVSNEEDED)
                  ,("491",      ERR_NOOPERHOST)
                  ,("501",      ERR_UMODEUNKNOWNFLAG)
                  ,("502",      ERR_USERSDONTMATCH)]

-- | Command code map
commandCodeMap :: M.HashMap T.Text CommandCode
commandCodeMap = M.fromList commandCodeList

-- Message constructors
-- ````````````````````

-- | Helper functions
msgCmd         :: Message  -> Command
msgServ        :: Message  -> Servname
msgNick        :: Message  -> Nickname
msgUser        :: Message  -> User
msgHost        :: Message  -> UserHost
msgCmdList     :: Message  -> [Middle]
msgTrailing    :: Message  -> Maybe Text
msgHostName    :: Message  -> Host
msgUserIP      :: Message  -> IPAddr
msgUserIPAddr  :: Message  -> Text
msgCloak       :: Message  -> [Text]
tMsgCmd        :: TMessage -> Command
tMsgServ       :: TMessage -> Servname
tMsgNick       :: TMessage -> Nickname
tMsgUser       :: TMessage -> User
tMsgHost       :: TMessage -> UserHost
tMsgCmdList    :: TMessage -> [Middle]
tMsgTrailing   :: TMessage -> Maybe Text
tMsgHostName   :: TMessage -> Host
tMsgUserIP     :: TMessage -> IPAddr
tMsgUserIPAddr :: TMessage -> Text
tMsgCloak      :: TMessage -> [Text]

msgCmd         = command
msgServ        = servName . prefix
msgNick        = nickName . prefix
msgUser        = user     . prefix
msgHost        = userHost . prefix
msgCmdList     = cmdList  . params
msgTrailing    = trailing . params
msgHostName    = host     . msgHost
msgUserIP      = ipAddr   . msgHost
msgUserIPAddr  = ip       . msgUserIP
msgCloak       = cloaks   . msgHost
tMsgCmd        = command  . msg
tMsgServ       = servName . prefix    . msg
tMsgNick       = nickName . prefix    . msg
tMsgUser       = user     . prefix    . msg
tMsgHost       = userHost . prefix    . msg
tMsgCmdList    = cmdList  . params    . msg
tMsgTrailing   = trailing . params    . msg
tMsgHostName   = host     . msgHost   . msg
tMsgUserIP     = ipAddr   . msgHost   . msg
tMsgUserIPAddr = ip       . msgUserIP . msg
tMsgCloak      = cloaks   . msgHost   . msg

-- | Add fake timestamp to a message
addTimeStamp :: Message -> TMessage
addTimeStamp m = TMessage m (UnixDateTimeMillis 0)

-- | Contruct a PING message
mkPing :: HostName -> TMessage
mkPing h = clientMessage (Command PING) (Param [rawShow h] Nothing)

-- | Construct a PONG message
mkPong :: HostName -> TMessage
mkPong h = clientMessage (Command PONG) (Param [rawShow h] Nothing)

-- | Construct a USER message
mkUser :: Text   -- ^ Registered user name
         -> Mode -- ^ User modes
         -> Msg  -- ^ User's real name
         -> TMessage
mkUser u m r = clientMessage (Command USER) (Param [u, m, "*"] (Just r))

-- | Construct a PASS message
mkPass :: Text -> TMessage
mkPass p = clientMessage (Command PASS) (Param [p] Nothing)

-- | Construct a OPER message
mkOper :: Text   -- ^ User name
         -> Text -- ^ Password
         -> TMessage
mkOper u p = clientMessage (Command OPER) (Param [u, p] Nothing)

-- | Construct a NICK message
mkNick :: Nickname -> TMessage
mkNick n = clientMessage (Command NICK) (Param [n] Nothing)

-- | Construct a MODE message
mkUserMode :: Nickname -> Mode -> TMessage
mkUserMode u m = clientMessage (Command MODE) (Param [u, m] Nothing)

-- | Construct a PRIVMSG message, the target could be a user nickname or channel
mkPrivMsg :: MsgTarget -> Msg -> TMessage
mkPrivMsg t m = clientMessage (Command PRIVMSG) (Param [rawShow t] (Just m))

-- | Construct a NOTICE message, the target could be a user nickname or channel
mkNotice :: MsgTarget -> Msg -> TMessage
mkNotice t m = clientMessage (Command NOTICE) (Param [rawShow t] (Just m))

-- | Construct a MOTD (TMessage Of The Day) querying message.
mkMOTD :: TMessage
mkMOTD = clientMessage (Command MOTD) (Param [] Nothing)

-- | Constuct a VERSION query message.
mkVersion :: Maybe HostName -> TMessage
mkVersion Nothing = clientMessage (Command VERSION) (Param [] Nothing)
mkVersion (Just h) = clientMessage (Command VERSION) (Param [rawShow h] Nothing)

-- | Construct a TIME query message
mkTime :: TMessage
mkTime = clientMessage (Command TIME) (Param [] Nothing)

-- | Construct an INFO message.
mkInfo :: Maybe HostName -> TMessage
mkInfo Nothing = clientMessage (Command INFO) (Param [] Nothing)
mkInfo (Just h) = clientMessage (Command INFO) (Param [rawShow h] Nothing)

-- | Construct a WHO query message
mkWho :: Nickname -> TMessage
mkWho n = clientMessage (Command WHO) (Param [n] Nothing)

-- | Construct a JOIN message
mkJoin :: [Channel] -> TMessage
mkJoin ch = clientMessage (Command JOIN) (Param ch Nothing)

-- | Constuct a PART message
mkPart :: [Channel] -> Maybe Msg -> TMessage
mkPart ch m = clientMessage (Command JOIN) (Param [intercalate "," ch] m)

-- | Construct a QUIT message
mkQuit :: Maybe Text -> TMessage
mkQuit m = clientMessage (Command QUIT) (Param [] m)

-- | Construct a channel mode message
mkChannelMode :: Channel -> Mode -> Text -> TMessage
mkChannelMode ch m p = clientMessage (Command MODE) (Param [ch, m, p] Nothing)

-- | Construct a TOPIC message
mkTopic :: Channel -> Maybe Text -> TMessage
mkTopic ch t = clientMessage (Command TOPIC) (Param [ch] t)

-- | Construct a NAMES message
mkNames :: [Channel] -> TMessage
mkNames ch = clientMessage (Command NAMES) (Param [intercalate "," ch] Nothing)

-- | Construct a NAMES message with only one channel parameter
mkNamesOne :: Channel -> TMessage
mkNamesOne ch = mkNames [ch]

-- | Constuct a LIST message
mkList :: [Channel] -> TMessage
mkList ch = clientMessage (Command LIST) (Param [intercalate "," ch] Nothing)

-- | Construct a LIST message with only one channel parameter
mkListOne :: Channel -> TMessage
mkListOne ch = mkList [ch]

-- | Constuct a INVITE message
mkInvite :: Nickname -> Channel -> TMessage
mkInvite n ch = clientMessage (Command INVITE) (Param [n, ch] Nothing)

-- | Constuct a KICK message
mkKick :: Channel -> [Nickname] -> TMessage
mkKick ch n = clientMessage (Command KICK) (Param [ch, intercalate "," n] Nothing)

-- | Construct a KICK message with only one nickname
mkKickOne :: Channel -> Nickname -> TMessage
mkKickOne ch n = mkKick ch [n]

-- | Construct a WHOIS message
mkWhois :: [Mask] -> TMessage
mkWhois m = clientMessage (Command WHOIS) (Param [intercalate "," m] Nothing)

-- | Construct a WHOIS message with only one mask
mkWhoisOne :: Mask -> TMessage
mkWhoisOne m = mkWhois [m]

-- | Construct a WHOWAS message
mkWhowas :: [Nickname] -> TMessage
mkWhowas n = clientMessage (Command WHOWAS) (Param [intercalate "," n] Nothing)

-- | Construct a WHOWAS message with only one nickname
mkWhowasOne :: Nickname -> TMessage
mkWhowasOne n = mkWhowas [n]

-- | Construct a AWAY message
mkAway :: Maybe Msg -> TMessage
mkAway m = clientMessage (Command AWAY) (Param [] m)

-- | Construct a USERHOST message
mkUserHost :: [Nickname] -> TMessage
mkUserHost u = clientMessage (Command USERHOST) (Param [intercalate "," u] Nothing)

-- | Construct a USERHOST message with only one nickname
mkUserHostOne :: Nickname -> TMessage
mkUserHostOne u = mkUserHost [u]

-- | Construct a ISON message
mkIsOn :: [Nickname] -> TMessage
mkIsOn n = clientMessage (Command ISON) (Param [T.unwords n] Nothing)

-- | Construct a ISON message with only one nickname
mkIsOnOne :: Nickname -> TMessage
mkIsOnOne n = mkIsOn [n]

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
  rawShow :: a -> Text

-- RawShow instances
-- `````````````````
instance RawShow Param where
  rawShow (Param m (Just t)) = T.unwords m <> " :" <> t
  rawShow (Param m Nothing)  = T.unwords m

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
  rawShow (Command c) = rawShow c
  rawShow (Unknown m) = "<Unknown \"" <> m <> "\">"

instance RawShow CommandCode where
  rawShow = T.pack . show

instance RawShow UnixDateTimeMillis where
  rawShow = T.pack . show

instance RawShow TMessage where
  rawShow = rawShow . msg

instance RawShow Message where
  rawShow (Message NullPrefix cmd pars) = T.unwords [rawShow cmd, rawShow pars]
  rawShow (Message pre cmd pars) = T.unwords [rawShow pre, rawShow cmd, rawShow pars]

instance RawShow MsgTarget where
  rawShow (UserTarget u) = rawShow u
  rawShow (ChanTarget c) = c

instance RawShow String where
  rawShow = T.pack

instance RawShow a => RawShow (Maybe a) where
  rawShow Nothing  = "Nothing"
  rawShow (Just a) = "Just " <> rawShow a

instance (RawShow a, RawShow b) => RawShow (Either a b) where
  rawShow (Left a)  = "Left" <> rawShow a
  rawShow (Right b) = "Right" <> rawShow b

-- Some useful functions
rawPrint :: RawShow a => a -> IO ()
rawPrint = T.putStrLn . rawShow
