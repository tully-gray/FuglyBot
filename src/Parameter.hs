module Parameter where

import           Data.Char    (toLower)

data Parameter = Nick | Owner | DictFile | UserCommands | RejoinKick
               | MaxChanMsg | NumThreads | NSetSize | SentenceTries
               | SentenceLength | ParseLength | Learning | StrictLearn
               | StrictTopic | Autoname | AllowPM | Debug
               | Topic | Randoms | ReplaceWords | Timer | Delay
               | Greetings | Actions | UnknownParam | Parameter {
                 nick        :: String,
                 owner       :: String,
                 fuglyDir    :: FilePath,
                 dictFile    :: String,
                 userCmd     :: Bool,   -- allow users to issue commands
                 rejoinKick  :: Int,    -- rejoin channels after being kicked
                 maxChanMsg  :: Int,    -- maximum words in a single message
                 numThreads  :: Int,    -- maximum number of threads
                 nSetSize    :: Int,    -- neural network set size
                 sTries      :: Int,    -- sentence tries
                 sLength     :: Int,    -- sentence length
                 pLength     :: Int,    -- parse length
                 learning    :: String, -- allowed learning regexp
                 strictLearn :: Bool,   -- strict learning using GF
                 strictTopic :: Bool,   -- stay on topic or not
                 autoName    :: Bool,
                 allowPM     :: Bool,
                 debug       :: Bool,
                 topic       :: String,
                 randoms     :: Int,
                 replaceWord :: Bool,
                 timer       :: Int,
                 delay       :: Int,
                 greetings   :: Int,
                 actions     :: Int
                 }
               deriving (Eq, Ord, Show)

allParams :: [Parameter]
allParams = [Nick ..]

instance Enum Parameter where
    toEnum 1  = Nick
    toEnum 2  = Owner
    toEnum 3  = DictFile
    toEnum 4  = UserCommands
    toEnum 5  = RejoinKick
    toEnum 6  = MaxChanMsg
    toEnum 7  = NumThreads
    toEnum 8  = NSetSize
    toEnum 9  = SentenceTries
    toEnum 10 = SentenceLength
    toEnum 11 = ParseLength
    toEnum 12 = Learning
    toEnum 13 = StrictLearn
    toEnum 14 = StrictTopic
    toEnum 15 = Autoname
    toEnum 16 = AllowPM
    toEnum 17 = Debug
    toEnum 18 = Topic
    toEnum 19 = Randoms
    toEnum 20 = ReplaceWords
    toEnum 21 = Timer
    toEnum 22 = Delay
    toEnum 23 = Greetings
    toEnum 24 = Actions
    toEnum 25 = UnknownParam
    toEnum _  = UnknownParam
    fromEnum Nick           = 1
    fromEnum Owner          = 2
    fromEnum DictFile       = 3
    fromEnum UserCommands   = 4
    fromEnum RejoinKick     = 5
    fromEnum MaxChanMsg     = 6
    fromEnum NumThreads     = 7
    fromEnum NSetSize       = 8
    fromEnum SentenceTries  = 9
    fromEnum SentenceLength = 10
    fromEnum ParseLength    = 11
    fromEnum Learning       = 12
    fromEnum StrictLearn    = 13
    fromEnum StrictTopic    = 14
    fromEnum Autoname       = 15
    fromEnum AllowPM        = 16
    fromEnum Debug          = 17
    fromEnum Topic          = 18
    fromEnum Randoms        = 19
    fromEnum ReplaceWords   = 20
    fromEnum Timer          = 21
    fromEnum Delay          = 22
    fromEnum Greetings      = 23
    fromEnum Actions        = 24
    fromEnum UnknownParam   = 25
    fromEnum _              = 25
    enumFrom i = enumFromTo i UnknownParam
    enumFromThen i j = enumFromThenTo i j UnknownParam

readParam :: String -> Parameter
readParam a | (map toLower a) == "nick"            = Nick
readParam a | (map toLower a) == "owner"           = Owner
readParam a | (map toLower a) == "dictfile"        = DictFile
readParam a | (map toLower a) == "usercmd"         = UserCommands
readParam a | (map toLower a) == "usercmds"        = UserCommands
readParam a | (map toLower a) == "usercommands"    = UserCommands
readParam a | (map toLower a) == "rejoinkick"      = RejoinKick
readParam a | (map toLower a) == "maxchanmsg"      = MaxChanMsg
readParam a | (map toLower a) == "numthreads"      = NumThreads
readParam a | (map toLower a) == "nsetsize"        = NSetSize
readParam a | (map toLower a) == "stries"          = SentenceTries
readParam a | (map toLower a) == "sentencetries"   = SentenceTries
readParam a | (map toLower a) == "slen"            = SentenceLength
readParam a | (map toLower a) == "slength"         = SentenceLength
readParam a | (map toLower a) == "sentencelength"  = SentenceLength
readParam a | (map toLower a) == "plen"            = ParseLength
readParam a | (map toLower a) == "plength"         = ParseLength
readParam a | (map toLower a) == "parselength"     = ParseLength
readParam a | (map toLower a) == "learning"        = Learning
readParam a | (map toLower a) == "strictlearn"     = StrictLearn
readParam a | (map toLower a) == "slearn"          = StrictLearn
readParam a | (map toLower a) == "stricttopic"     = StrictTopic
readParam a | (map toLower a) == "stopic"          = StrictTopic
readParam a | (map toLower a) == "autoname"        = Autoname
readParam a | (map toLower a) == "allowpm"         = AllowPM
readParam a | (map toLower a) == "debug"           = Debug
readParam a | (map toLower a) == "topic"           = Topic
readParam a | (map toLower a) == "randoms"         = Randoms
readParam a | (map toLower a) == "rwords"          = ReplaceWords
readParam a | (map toLower a) == "replacewords"    = ReplaceWords
readParam a | (map toLower a) == "timer"           = Timer
readParam a | (map toLower a) == "delay"           = Delay
readParam a | (map toLower a) == "greetings"       = Greetings
readParam a | (map toLower a) == "actions"         = Actions
readParam _                                        = UnknownParam
