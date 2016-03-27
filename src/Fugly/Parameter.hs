module Fugly.Parameter where

import           Data.Char    (toLower)

data Parameter = Nick | Owners | DictFile | UserCommands | RejoinKick
               | MaxChanMsg | NumThreads | NSetSize | SentenceTries
               | SentenceLength | ParseLength | Learning | StrictLearn
               | StrictTopic | Autoname | AllowPM | Debug
               | Topic | Randoms | ReplaceWords | Timer | Delay
               | Greetings | Actions | MatchChance | UnknownParam | Parameter {
                 nick        :: String,
                 owners      :: [String],
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
                 actions     :: Int,
                 matchChance :: Int
                 }
               deriving (Eq, Ord, Show)

allParams :: [Parameter]
allParams = [Nick ..]

instance Enum Parameter where
    toEnum 1  = Nick
    toEnum 2  = Owners
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
    toEnum 25 = MatchChance
    toEnum 26 = UnknownParam
    toEnum _  = UnknownParam
    fromEnum Nick           = 1
    fromEnum Owners         = 2
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
    fromEnum MatchChance    = 25
    fromEnum UnknownParam   = 26
    fromEnum _              = 26
    enumFrom i = enumFromTo i UnknownParam
    enumFromThen i j = enumFromThenTo i j UnknownParam

readParam :: String -> Parameter
readParam a | (map toLower a) == "nick"            = Nick
readParam a | (map toLower a) == "owners"          = Owners
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
readParam a | (map toLower a) == "matchchance"     = MatchChance
readParam a | (map toLower a) == "mchance"         = MatchChance
readParam _                                        = UnknownParam

readParamsFromList :: [String] -> Parameter
readParamsFromList a = Parameter
    {nick="", owners=[""], fuglyDir="", dictFile="",
     userCmd=read (a!!0), rejoinKick=read (a!!1), maxChanMsg=read (a!!2),
     numThreads=read (a!!3), nSetSize=read (a!!4), sTries=read (a!!5),
     sLength=read (a!!6), pLength=read (a!!7), learning=read (a!!8) :: String,
     strictLearn=read (a!!9), strictTopic=read (a!!10), autoName=read (a!!11),
     allowPM=read (a!!12), debug=read (a!!13), topic=read (a!!14) :: String,
     randoms=read (a!!15), replaceWord=read (a!!16), timer=read (a!!17),
     delay=read (a!!18), greetings=read (a!!19), actions=read (a!!20),
     matchChance=read (a!!21)}

paramsToList :: Parameter -> [String]
paramsToList (Parameter _ _ _ _ uc rk mcm nt ss st sl
    pl l stl stt an apm d t r rw ti dl g a mc) =
    [show uc, show rk, show mcm, show nt, show ss, show st, show sl,
    show pl, show l, show stl, show stt, show an, show apm, show d,
    show t, show r, show rw, show ti, show dl, show g, show a, show mc]
paramsToList _ = []
