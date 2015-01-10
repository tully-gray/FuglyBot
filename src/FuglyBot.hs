import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map.Lazy as Map
import Network.Socket
import Network.Socks5
import System.Environment
import System.IO
import System.IO.Error
import qualified System.Random as Random
import Text.Regex.Posix
import Prelude

import FuglyLib
import NLP.WordNet.PrimTypes (allPOS, allForm)

data Bot = Bot {
    sock :: Handle,
    params :: Parameter,
    fugly :: Fugly
    }

data Parameter = Nick | Owner | UserCommands | RejoinKick | ThreadTime | MaxChanMsg
               | SentenceTries | SentenceLength | ParseLength | Learning | StrictLearn
               | Autoname | AllowPM | Topic | Randoms | UnknownParam
               | Parameter {
                 nick        :: String,
                 owner       :: String,
                 fuglydir    :: FilePath,
                 usercmd     :: Bool,
                 rejoinkick  :: Int,
                 maxchanmsg  :: Int,
                 stries      :: Int,
                 slength     :: Int,
                 plength     :: Int,
                 learning    :: Bool,
                 strictlearn :: Bool,
                 autoname    :: Bool,
                 allowpm     :: Bool,
                 topic       :: String,
                 randoms     :: Int,
                 threadtime  :: Int
                 }
               deriving (Eq, Ord, Show)

allParams :: [Parameter]
allParams = [Nick ..]

instance Enum Parameter where
    toEnum 1 = Nick
    toEnum 2 = Owner
    toEnum 3 = UserCommands
    toEnum 4 = RejoinKick
    toEnum 5 = MaxChanMsg
    toEnum 6 = SentenceTries
    toEnum 7 = SentenceLength
    toEnum 8 = ParseLength
    toEnum 9 = Learning
    toEnum 10 = StrictLearn
    toEnum 11 = Autoname
    toEnum 12 = AllowPM
    toEnum 13 = Topic
    toEnum 14 = Randoms
    toEnum 15 = ThreadTime
    toEnum 16 = UnknownParam
    toEnum _  = UnknownParam
    fromEnum Nick           = 1
    fromEnum Owner          = 2
    fromEnum UserCommands   = 3
    fromEnum RejoinKick     = 4
    fromEnum MaxChanMsg     = 5
    fromEnum SentenceTries  = 6
    fromEnum SentenceLength = 7
    fromEnum ParseLength    = 8
    fromEnum Learning       = 9
    fromEnum StrictLearn    = 10
    fromEnum Autoname       = 11
    fromEnum AllowPM        = 12
    fromEnum Topic          = 13
    fromEnum Randoms        = 14
    fromEnum ThreadTime     = 15
    fromEnum UnknownParam   = 16
    fromEnum _              = 16
    enumFrom i = enumFromTo i UnknownParam
    enumFromThen i j = enumFromThenTo i j UnknownParam

readParam :: String -> Parameter
readParam a | (map toLower a) == "nick"            = Nick
readParam a | (map toLower a) == "owner"           = Owner
readParam a | (map toLower a) == "usercmd"         = UserCommands
readParam a | (map toLower a) == "usercmds"        = UserCommands
readParam a | (map toLower a) == "usercommands"    = UserCommands
readParam a | (map toLower a) == "rejoinkick"      = RejoinKick
readParam a | (map toLower a) == "maxchanmsg"      = MaxChanMsg
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
readParam a | (map toLower a) == "autoname"        = Autoname
readParam a | (map toLower a) == "allowpm"         = AllowPM
readParam a | (map toLower a) == "topic"           = Topic
readParam a | (map toLower a) == "randoms"         = Randoms
readParam a | (map toLower a) == "threadtime"      = ThreadTime
readParam a | (map toLower a) == "ttime"           = ThreadTime
readParam _                                        = UnknownParam

main :: IO ()
main = do
    bracket start stop loop
  where
    loop :: (MVar Bot, MVar ()) -> IO ()
    loop st = do catch (evalStateT run st)
                   (\e -> do let err = show (e :: SomeException)
                             evalStateT (hPutStrLnLock stderr ("Exception in main: " ++ err)) st
                             return ())

start :: IO (MVar Bot, MVar ())
start = do
    args <- cmdLine
    let servn    = args !! 0
    let port     = args !! 1
    let hints = defaultHints {addrFlags = [AI_NUMERICSERV]}
    serv <- getAddrInfo (Just hints) (Just servn) (Just port)
    let nick'    = cleanStringWhite isAscii (args !! 2)
    let owner'   = args !! 3
    let topic'   = args !! 5
    let fdir     = args !! 6 :: FilePath
    let wndir    = args !! 7 :: FilePath
    let gfdir    = args !! 8 :: FilePath
    let socks5   = args !! 10
    let s5hostn  = takeWhile (\x -> x /= ':') socks5
    let s5port   = tail $ dropWhile (\x -> x /= ':') socks5
    let channels = words $ args !! 4
    let passwd   = args !! 9
    s5serv <- getAddrInfo (Just hints) (Just s5hostn) (Just s5port)
    s <- socket AF_INET Stream defaultProtocol
    _ <- setSocketOption s KeepAlive 0
    _ <- if null socks5 then connect s (addrAddress $ head serv)
         else socksConnectAddr s (addrAddress $ head s5serv) (addrAddress $ head serv)
    sh <- socketToHandle s ReadWriteMode
    hSetBuffering sh NoBuffering
    (f, p) <- initFugly fdir wndir gfdir topic'
    let b = if null p then
              Bot sh (Parameter nick' owner' fdir False 10 400 20 7 0 True False True False topic' 50 30) f
              else Bot sh ((readParamsFromList p){nick=nick', owner=owner', fuglydir=fdir, topic=topic'}) f
    bot <- newMVar b
    lock <- newMVar ()
    evalStateT (write sh "NICK" nick') (bot, lock)
    evalStateT (write sh "USER" (nick' ++ " 0 * :user")) (bot, lock)
    _ <- forkIO (do
                    threadDelay 20000000
                    if not $ null passwd then evalStateT (replyMsg b "nickserv" [] ("IDENTIFY " ++ passwd)) (bot, lock) else return ()
                    evalStateT (joinChannel sh "JOIN" channels) (bot, lock))
    return (bot, lock)

stop :: (MVar Bot, MVar ()) -> IO ()
stop (bot, lock) = do
    Bot{sock=s, params=p@(Parameter{fuglydir=fd, topic=t}), fugly=f} <- readMVar bot
    hClose s
    stopFugly lock fd f t (paramsToList p)

run :: StateT (MVar Bot, MVar ()) IO b
run = do
    st <- get
    Bot{sock=s} <- lift $ readMVar $ fst st
    -- forever $ do lift (hGetLine s >>= (\l -> do evalStateT (hPutStrLnLock stdout l) st >> return l) >>= (\ll -> listenIRC st s ll))
    forever $ do lift (hGetLine s >>= (\ll -> listenIRC st s ll))
    where
      listenIRC st s l = do
        let b = fst st
        bot@Bot{params=p@(Parameter{nick=bn, owner=o})} <- readMVar b
        let ll = words l
        let lll = take 2 $ drop 1 ll
        if "PING :" `isPrefixOf` l then do
          evalStateT (write s "PONG" (':' : drop 6 l)) st >> return ()
          else if "433 " `isPrefixOf` (unwords (drop 1 ll)) then do
            evalStateT (write s "NICK" (bn ++ "_")) st >> swapMVar b bot{params=p{nick=(bn ++ "_")}}
              >> return ("Nickname is already in use.") >>= (\x -> evalStateT (replyMsg bot o [] x) st)
               else if (length ll > 2) && (fHead [] lll) == "NICK" && getNick ll == bn then do
                 (do nb <- evalStateT (changeNick [] lll) st ; swapMVar b nb) >> return ()
                    else do
                      (catch (evalStateT (processLine $ words l) st >> return ())
                       (\e -> do let err = show (e :: SomeException)
                                 evalStateT (hPutStrLnLock stderr ("Exception in processLine: " ++ err)) st
                                 putMVar b bot
                                 return ()))

cmdLine :: IO [String]
cmdLine = do
    args <- getArgs
    let l            = length args
    let serverPos    = (maximum' $ elemIndices "-server" args) + 1
    let server       = if l > serverPos then args !! serverPos else "irc.freenode.net"
    let portPos      = (maximum' $ elemIndices "-port" args) + 1
    let port         = if l > portPos then args !! portPos else "6667"
    let nickPos      = (maximum' $ elemIndices "-nick" args) + 1
    let nick'        = if l > nickPos then args !! nickPos else "fuglybot"
    let ownerPos     = (maximum' $ elemIndices "-owner" args) + 1
    let owner'       = if l > ownerPos then args !! ownerPos else "shadowdaemon"
    let channelPos   = (maximum' $ elemIndices "-channel" args) + 1
    let channel      = if l > channelPos then args !! channelPos else "#fuglybot"
    let topicPos     = (maximum' $ elemIndices "-topic" args) + 1
    let topic'       = if l > topicPos then args !! topicPos else "default"
    let fuglydirPos  = (maximum' $ elemIndices "-fuglydir" args) + 1
    let fuglydir'    = if l > fuglydirPos then args !! fuglydirPos
                                            else "fugly"
    let wndirPos     = (maximum' $ elemIndices "-wndir" args) + 1
    let wndir        = if l > wndirPos then args !! wndirPos
                                            else "/usr/share/wordnet/dict/"
    let gfdirPos     = (maximum' $ elemIndices "-gfdir" args) + 1
    let gfdir        = if l > gfdirPos then args !! gfdirPos
                                            else "gf"
    let passwdPos    = (maximum' $ elemIndices "-passwd" args) + 1
    let passwd       = if l > passwdPos then args !! passwdPos else ""
    let socks5Pos    = (maximum' $ elemIndices "-socks" args) + 1
    let socks5       = if l > socks5Pos then args !! socks5Pos else ""
    return (server : port : nick' : owner' : channel : topic' : fuglydir' : wndir : gfdir : passwd : socks5 : [])
  where
    maximum' [] = 1000
    maximum' a  = maximum a

changeNick :: [String] -> [String] -> StateT (MVar Bot, MVar ()) IO Bot
changeNick (_:_) (_:_) = do
    st <- get
    bot <- lift $ readMVar $ fst st
    return bot
changeNick (x:_) [] = do
    st <- get
    bot@(Bot{sock=s}) <- lift $ readMVar $ fst st
    let new = cleanStringWhite isAscii x
    write s "NICK" new
    return bot
changeNick [] line = do
    st <- get
    bot@(Bot{params=p@(Parameter{nick=n})}) <- lift $ readMVar $ fst st
    lift $ testNick st bot n line
  where
    testNick :: (MVar Bot, MVar ()) -> Bot -> String -> [String] -> IO Bot
    testNick _ bot [] _ = return bot
    testNick _ bot _ [] = return bot
    testNick st' bot@(Bot{params=p@(Parameter{owner = o})}) old line'
        | (x == "NICK") = return ("Nick change successful.") >>= (\x' -> evalStateT (replyMsg bot o [] x') st')
                          >> return bot{params=p{nick = drop 1 y}}
        | otherwise     = return ("Nick change failed!") >>= (\x' -> evalStateT (replyMsg bot o [] x') st')
                          >> return bot{params=p{nick = old}}
      where
        x = fHead [] line'
        y = fLast [] line'
    testNick _ bot _ _ = return bot

joinChannel :: Handle -> String -> [String] -> StateT (MVar Bot, MVar ()) IO ()
joinChannel _ _  []    = return () :: StateT (MVar Bot, MVar ()) IO ()
joinChannel h [] b     = joinChannel h "join" b
joinChannel h a (x:xs) = do
    if a == "JOIN" || a == "PART" then do
      write h a x
      joinChannel h a xs
        else return ()

readParamsFromList :: [String] -> Parameter
readParamsFromList a = Parameter{nick="", owner="", fuglydir="", usercmd=read (a!!0),
                                 rejoinkick=read (a!!1), maxchanmsg=read (a!!2), stries=read (a!!3),
                                 slength=read (a!!4), plength=read (a!!5), learning=read (a!!6),
                                 strictlearn=read (a!!7), autoname=read (a!!8), allowpm=read (a!!9),
                                 topic="", randoms=read (a!!10), threadtime=read (a!!11)}

paramsToList :: Parameter -> [String]
paramsToList (Parameter _ _ _ uc rk mcm st sl pl l stl an apm _ r tt) = [show uc, show rk, show mcm, show st,
                                                                        show sl, show pl, show l, show stl,
                                                                        show an, show apm, show r, show tt]
paramsToList _ = []

changeParam :: Bot -> String -> String -> String -> String -> StateT (MVar Bot, MVar ()) IO Bot
changeParam bot@(Bot{params=p@(Parameter{nick=botnick, owner=owner', fuglydir=fdir, topic=t}),
                     fugly=f@(Fugly{dict=dict', ban=ban', FuglyLib.match=match'})}) chan nick' param value = do
    st <- get
    case (readParam param) of
      Nick           -> changeNick (value : "" : []) []
      Owner          -> replyMsg' value                 "Owner"               >>= (\x -> return bot{params=p{owner=x}})
      UserCommands   -> replyMsg' (readBool value)      "User commands"       >>= (\x -> return bot{params=p{usercmd=x}})
      RejoinKick     -> replyMsg' (readInt 1 4096 value) "Rejoin kick time"   >>= (\x -> return bot{params=p{rejoinkick=x}})
      MaxChanMsg     -> replyMsg' (readInt 9 450 value) "Max channel message" >>= (\x -> return bot{params=p{maxchanmsg=x}})
      SentenceTries  -> replyMsg' (readInt 5 4096 value) "Sentence tries"     >>= (\x -> return bot{params=p{stries=x}})
      SentenceLength -> replyMsg' (readInt 2 256 value) "Sentence length"     >>= (\x -> return bot{params=p{slength=x}})
      ParseLength    -> replyMsg' (readInt 0 256 value) "Parse length"        >>= (\x -> return bot{params=p{plength=x}})
      Learning       -> replyMsg' (readBool value)     "Learning"             >>= (\x -> return bot{params=p{learning=x}})
      StrictLearn    -> replyMsg' (readBool value)     "Strict learn"         >>= (\x -> return bot{params=p{strictlearn=x}})
      Autoname       -> replyMsg' (readBool value)     "Autoname"             >>= (\x -> return bot{params=p{autoname=x}})
      AllowPM        -> replyMsg' (readBool value)     "Allow PM"             >>= (\x -> return bot{params=p{allowpm=x}})
      Topic          -> do (d, b, m, pl) <- lift $ catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then
                                                                       return (Map.empty, [], [], (paramsToList p))
                                                                       else
                                                                         return (dict', ban', match', (paramsToList p)))
                                               (loadDict fdir value) (\_ -> return (Map.empty, [], [], (paramsToList p)))
                           _ <- lift $ saveDict (snd st) f fdir t (paramsToList p)
                           let pp = (readParamsFromList pl){nick=botnick, owner=owner', fuglydir=fdir}
                           replyMsg' value "Topic" >>= (\x -> return bot{params=pp{topic=x}, fugly=f{dict=d, ban=b, FuglyLib.match=m}})
      Randoms        -> replyMsg' (readInt 0 100 value) "Randoms"             >>= (\x -> return bot{params=p{randoms=x}})
      ThreadTime     -> replyMsg' (readInt 0 360 value) "Thread time"         >>= (\x -> return bot{params=p{threadtime=x}})
      _              -> return bot
  where
    replyMsg' v msg = do replyMsg bot chan nick' (msg ++ " set to " ++ show v ++ ".") >> return v
    readInt min' max' a
      | aa < min' = min'
      | aa > max' = max'
      | otherwise = aa
      where
        aa = read a
    readBool a
      | (map toLower a) == "true"    = True
      | (map toLower a) == "yes"     = True
      | (map toLower a) == "on"      = True
      | (map toLower a) == "false"   = False
      | (map toLower a) == "no"      = False
      | (map toLower a) == "off"     = False
      | otherwise                    = False
changeParam bot _ _ _ _ = return bot

getMsg :: [String] -> [String]
getMsg [] = []
getMsg msg
    | p == "PRIVMSG" = (drop 1 (msg!!3)) : (drop 4 msg)
    | otherwise      = []
  where
    p = if (length $ drop 1 msg) > 0 then head $ drop 1 msg else "FOO"

getNick :: [String] -> String
getNick []  = []
getNick msg
    | (length msg) > 0 = drop 1 $ takeWhile (/= '!') $ head msg
    | otherwise        = []

getChannel :: [String] -> String
getChannel [] = []
getChannel msg
    | (length $ drop 2 msg) > 0 = head $ drop 2 msg
    | otherwise                 = []

spokenTo :: String -> [String] -> Bool
spokenTo _ []         = False
spokenTo n b
    | c == n          = True
    | c == (n ++ ":") = True
    | otherwise       = False
  where
    c = head b

beenKicked :: String -> [String] -> String
beenKicked _ [] = []
beenKicked n a
    | (head $ drop 1 a) == "KICK" = if (head $ drop 3 a) == n then getChannel a else []
    | otherwise                   = []

rejoinChannel :: Handle -> String -> Int -> StateT (MVar Bot, MVar ()) IO ()
rejoinChannel _ []   _  = return () :: StateT (MVar Bot, MVar ()) IO ()
rejoinChannel h chan rk = do
    if rk == 0 then return () else rejoin' rk chan h >> return ()
  where
    rejoin' rk' chan' h' = do
      st <- get
      lift $ forkIO (threadDelay (rk' * 1000000) >>
                     evalStateT (hPutStrLnLock h' ("JOIN " ++ chan' ++ "\r")) st)

processLine :: [String] -> StateT (MVar Bot, MVar ()) IO ()
processLine [] = return ()
processLine line = do
    st <- get :: StateT (MVar Bot, MVar ()) IO (MVar Bot, MVar ())
    let b = fst st
    bot@(Bot{sock=s, params=p@(Parameter{nick=n, rejoinkick=rk})}) <- lift $ takeMVar b
    let bk = beenKicked n line
    if (not $ null bk) then do (rejoinChannel s bk rk >> lift (putMVar b bot) >> return ())
      else if null msg then lift $ putMVar b bot >> return ()
         else if chan == n then do nb <- prvcmd bot ; lift $ putMVar b nb >> return ()
              else if spokenTo n msg then if null (tail msg) then lift $ putMVar b bot >> return ()
                                          else if (head $ head $ tail msg) == '!'
                                               then do nb <- execCmd bot chan who (tail msg)
                                                       lift $ putMVar b nb >> return ()
                                               else do nb <- reply bot chan who (tail msg)
                                                       lift $ putMVar b nb >> return ()
                   else do nb <- reply bot chan [] msg ; lift $ putMVar b nb >> return ()
  where
    msg  = getMsg line
    who  = getNick line
    chan = getChannel line
    prvcmd bot = if (length $ head msg) > 0 then
                   if (head $ head msg) == '!' then execCmd bot who who msg
                   else reply bot [] who msg
                 else reply bot [] who msg

reply :: Bot -> String -> String -> [String] -> StateT (MVar Bot, MVar ()) IO Bot
reply bot _ _ [] = return bot
reply bot@(Bot{sock=s, params=p@(Parameter botnick owner' _ _ _ _ stries'
                                 slen plen learning' slearn autoname' allowpm' _ randoms' ttime),
           fugly=f@(Fugly {pgf=pgf', FuglyLib.match=match'})}) chan nick' msg = do
    st <- get :: StateT (MVar Bot, MVar ()) IO (MVar Bot, MVar ())
    let mmsg = if null $ head msg then msg
                 else case fLast ' ' $ head msg of
                   ':' -> tail msg
                   ',' -> tail msg
                   _   -> msg
    fmsg <- lift $ asReplaceWords f $ map cleanString mmsg
    let parse = if slearn then gfParseBool pgf' plen $ unwords fmsg else True
    let matchon = map toLower (" " ++ intercalate " | " (botnick : match') ++ " ")
    mm <- lift $ chooseWord fmsg
    tId <- lift $ (if null chan then
                     if allowpm' then
                       sentenceReply st s f nick' [] randoms' stries' slen plen mm
                     else forkIO $ return ()
                   else if null nick' then
                          if map toLower (unwords msg) =~ matchon then
                            sentenceReply st s f chan chan randoms' stries' slen plen mm
                          else forkIO $ return ()
                        else sentenceReply st s f chan nick' randoms' stries' slen plen mm)
    if ttime > 0 then
      lift $ forkIO (do threadDelay $ ttime * 1000000 ; evalStateT (hPutStrLnLock stderr ("Killed thread: " ++ show tId)) st ; killThread tId) >> return ()
      else return ()
    if ((nick' == owner' && null chan) || parse) && learning' then do
      nd <- lift $ insertWords (snd st) f autoname' fmsg
      hPutStrLnLock stdout ("> parse: " ++ unwords fmsg)
      return bot{fugly=f{dict=nd}} else
      return bot
reply bot _ _ _ = return bot

execCmd :: Bot -> String -> String -> [String] -> StateT (MVar Bot, MVar ()) IO Bot
execCmd b _ _ [] = lift $ return b
execCmd b _ [] _ = lift $ return b
execCmd b [] _ _ = lift $ return b
execCmd b chan nick' (x:xs) = do
    st <- get
    lift $ catch (execCmd' b st) (\e -> do let err = show (e :: SomeException)
                                           evalStateT (hPutStrLnLock stderr ("Exception in execCmd: " ++ err)) st
                                           return b)
  where
    execCmd' :: Bot -> (MVar Bot, MVar ()) -> IO Bot
    execCmd' bot@(Bot{sock=s, params=p@(Parameter botnick owner' fdir
                                        usercmd' rkick maxcmsg
                                        stries' slen plen learn slearn
                                        autoname' allowpm' topic' randoms' ttime),
                      fugly=f@(Fugly dict' pgf' wne' aspell' ban' match')}) st
      | usercmd' == False && nick' /= owner' = return bot
      | x == "!quit" =
        if nick' == owner' then case (length xs) of
          0 -> do evalStateT (write s "QUIT" ":Bye") st >> return bot
          _ -> do evalStateT (write s "QUIT" (":" ++ unwords xs)) st >> return bot
        else return bot
      | x == "!save" = if nick' == owner' then catch (saveDict (snd st) f fdir topic' (paramsToList p))
                                               (\e -> do let err = show (e :: SomeException)
                                                         evalStateT (hPutStrLnLock stderr ("Exception in saveDict: " ++ err)) st
                                                         return ())
                                               >> evalStateT (replyMsg bot chan nick' "Saved dict file!") st
                                               >> return bot else return bot
      | x == "!load" = if nick' == owner' then do
           (nd, nb, nm, np) <- catch (loadDict fdir topic') (\e -> do let err = show (e :: SomeException)
                                                                      evalStateT (hPutStrLnLock stderr ("Exception in loadDict: " ++ err)) st
                                                                      return (dict', ban', match', paramsToList p))
           evalStateT (replyMsg bot chan nick' "Loaded dict file!") st
           return bot{params=(readParamsFromList np){nick=botnick, owner=owner', fuglydir=fdir, topic=topic'},
                      fugly=(Fugly nd pgf' wne' aspell' nb nm)}
                       else return bot
      | x == "!join" = if nick' == owner' then evalStateT (joinChannel s "JOIN" xs) st >>
                                             return bot else return bot
      | x == "!part" = if nick' == owner' then evalStateT (joinChannel s "PART" xs) st >>
                                             return bot else return bot
      | x == "!nick" = if nick' == owner' then evalStateT (changeNick xs []) st else return bot
      | x == "!readfile" = if nick' == owner' then case (length xs) of
          1 -> catch (insertFromFile (snd st) bot (xs!!0)) (\e -> do let err = show (e :: SomeException)
                                                                     evalStateT (hPutStrLnLock stderr ("Exception in insertFromFile: " ++ err)) st
                                                                     return bot)
          _ -> evalStateT (replyMsg bot chan nick' "Usage: !readfile <file>") st >>
               return bot else return bot
      | x == "!showparams" =
          if nick' == owner' then case (length xs) of
            0 -> evalStateT (replyMsg bot chan nick' ("nick: " ++ botnick ++ "  owner: " ++ owner' ++
                   "  usercommands: " ++ show usercmd' ++ "  rejoinkick: "
                   ++ show rkick ++ "  maxchanmsg: " ++ show maxcmsg
                   ++ "  sentencetries: " ++ show stries' ++ "  sentencelength: "
                   ++ show slen ++ "  parselength: " ++ show plen
                   ++ "  learning: " ++ show learn ++ "  strictlearn: " ++ show slearn
                   ++ "  autoname: " ++ show autoname' ++ "  allowpm: " ++ show allowpm'
                   ++ "  topic: " ++ topic' ++ "  randoms: " ++ show randoms'
                   ++ "  threadtime: " ++ show ttime)) st >> return bot
            _ -> evalStateT (replyMsg bot chan nick' "Usage: !showparams") st >> return bot
          else return bot
      | x == "!setparam" =
            if nick' == owner' then case (length xs) of
              2 -> evalStateT (changeParam bot chan nick' (xs!!0) (xs!!1)) st
              _ -> evalStateT (replyMsg bot chan nick' "Usage: !setparam <parameter> <value>") st >> return bot
            else return bot
      | x == "!params" =
              if nick' == owner' then evalStateT (replyMsg bot chan nick' (init (concat $ map (++ " ")
                                      $ map show $ init allParams))) st >> return bot
              else return bot
      | x == "!dict" =
          case (length xs) of
            2 -> (dictLookup f (xs!!0) (xs!!1)) >>= (\x' -> evalStateT (replyMsg bot chan nick' x') st) >> return bot
            1 -> (dictLookup f (xs!!0) []) >>= (\x' -> evalStateT (replyMsg bot chan nick' x') st) >> return bot
            _ -> evalStateT (replyMsg bot chan nick' "Usage: !dict <word> [part-of-speech]") st >> return bot
      | x == "!wordlist" =
          let num = if read (xs!!0) > (100 :: Int) then 100 :: Int else read (xs!!0) in
          case (length xs) of
            1 -> evalStateT (replyMsg bot chan nick' (unwords $ listWordsCountSort2 dict' num)
                 >> replyMsg bot chan nick' ("Total word count: " ++ (show $ Map.size dict'))) st
                 >> return bot
            _ -> evalStateT (replyMsg bot chan nick' "Usage: !wordlist <number>") st >> return bot
      | x == "!word" = case (length xs) of
            1 -> evalStateT (replyMsg bot chan nick' (listWordFull dict' (xs!!0))) st >> return bot
            _ -> evalStateT (replyMsg bot chan nick' "Usage: !word <word>") st >> return bot
      | x == "!insertword" = if nick' == owner' then
          case (length xs) of
            2 -> do ww <- insertWordRaw (snd st) f (xs!!1) [] [] (xs!!0)
                    if isJust $ Map.lookup (xs!!1) dict' then
                      evalStateT (replyMsg bot chan nick' ("Word " ++ (xs!!1) ++ " already in dict.")) st >> return bot
                      else
                      evalStateT (replyMsg bot chan nick' ("Inserted word " ++ (xs!!1) ++ ".")) st >>
                      return bot{fugly=f{dict=ww}}
            _ -> evalStateT (replyMsg bot chan nick' "Usage: !insertword <pos> <word>") st >> return bot
                         else return bot
      | x == "!dropword" = if nick' == owner' then
          case (length xs) of
            1 -> if isJust $ Map.lookup (xs!!0) dict' then
                   evalStateT (replyMsg bot chan nick' ("Dropped word " ++ (xs!!0) ++ ".")) st >>
                   return bot{fugly=f{dict=dropWord dict' (xs!!0)}}
                   else {-- Drop the word anyway because it might be a before or after word. --}
                     evalStateT (replyMsg bot chan nick' ("Word " ++ (xs!!0) ++ " not in dict.")) st >>
                     return bot{fugly=f{dict=dropWord dict' (xs!!0)}}
            _ -> evalStateT (replyMsg bot chan nick' "Usage: !dropword <word>") st >> return bot
                         else return bot
      | x == "!dropafter" = if nick' == owner' then
          case (length xs) of
            2 -> if (isJust $ Map.lookup (xs!!0) dict') && (isJust $ Map.lookup (xs!!1) dict') then
                   evalStateT (replyMsg bot chan nick' ("Dropped word " ++ (xs!!1) ++ " after word " ++ (xs!!0) ++ ".")) st >>
                   return bot{fugly=f{dict=dropAfter (dropBefore dict' (xs!!1) (xs!!0)) (xs!!0) (xs!!1)}}
                   else {-- Drop the association anyway, but report errors. --}
                     evalStateT (replyMsg bot chan nick' ("Word " ++ (xs!!0) ++ " or word " ++ (xs!!1) ++ " not in dict.")) st >>
                     return bot{fugly=f{dict=dropAfter (dropBefore dict' (xs!!1) (xs!!0)) (xs!!0) (xs!!1)}}
            _ -> evalStateT (replyMsg bot chan nick' "Usage: !dropafter <word> <after-word>") st >> return bot
                         else return bot
      | x == "!ageword" = if nick' == owner' then
          case (length xs) of
            {-- This has to be caught here? --}
            2 -> do num <- catch (if (read (xs!!1) :: Int) > 50 then return 50 else return (read (xs!!1) :: Int))
                              (\e -> do let err = show (e :: SomeException)
                                        evalStateT (hPutStrLnLock stderr ("Exception in ageword command: read: " ++ err)) st
                                        return 0)
                    if isJust $ Map.lookup (xs!!0) dict' then
                      evalStateT (replyMsg bot chan nick' ("Aged word " ++ (xs!!0) ++ ".")) st >>
                      return bot{fugly=f{dict=ageWord dict' (xs!!0) num}}
                      else
                      evalStateT (replyMsg bot chan nick' ("Word " ++ (xs!!0) ++ " not in dict.")) st >> return bot
            _ -> evalStateT (replyMsg bot chan nick' "Usage: !ageword <word> <number>") st
                 >> return bot
                         else return bot
      | x == "!agewords" = if nick' == owner' then
          case (length xs) of
            {-- This has to be caught here? --}
            1 -> do num <- catch (if (read (xs!!0) :: Int) > 50 then return 50 else return (read (xs!!0) :: Int))
                              (\e -> do let err = show (e :: SomeException)
                                        evalStateT (hPutStrLnLock stderr ("Exception in agewords command: read: " ++ err)) st
                                        return 0)
                    evalStateT (replyMsg bot chan nick' ("Aged all words...")) st
                    return bot{fugly=f{dict=ageWords dict' num}}
            _ -> evalStateT (replyMsg bot chan nick' "Usage: !agewords <number>") st
                 >> return bot
                         else return bot
      | x == "!cleanwords" = if nick' == owner' then
          case (length xs) of
            0 -> do nd <- fixWords aspell' dict'
                    evalStateT (replyMsg bot chan nick' ("Cleaned some words...")) st
                    return bot{fugly=f{dict=nd}}
            _ -> evalStateT (replyMsg bot chan nick' "Usage: !cleanwords") st
                 >> return bot
                         else return bot
      | x == "!banword" = if nick' == owner' then
          case (length xs) of
            2 -> if (xs!!0) == "add" then
                   if elem (xs!!1) ban' then
                     evalStateT (replyMsg bot chan nick' ("Word " ++ (xs!!1) ++ " already banned.")) st >> return bot
                     else
                     evalStateT (replyMsg bot chan nick' ("Banned word " ++ (xs!!1) ++ ".")) st >>
                     return bot{fugly=f{dict=dropWord dict' (xs!!1), ban=nub $ ban' ++ [(xs!!1)]}}
                 else if (xs!!0) == "delete" then
                   if not $ elem (xs!!1) ban' then
                     evalStateT (replyMsg bot chan nick' ("Word " ++ (xs!!1) ++ " not in ban list.")) st >> return bot
                     else
                     evalStateT (replyMsg bot chan nick' ("Unbanned word " ++ (xs!!1) ++ ".")) st >>
                     return bot{fugly=f{ban=nub $ delete (xs!!1) ban'}}
                 else evalStateT (replyMsg bot chan nick' "Usage: !banword <list|add|delete> <word>") st >> return bot
            1 -> if (xs!!0) == "list" then
                   evalStateT (replyMsg bot chan nick' ("Banned word list: " ++ unwords ban')) st >> return bot
                 else evalStateT (replyMsg bot chan nick' "Usage: !banword <list|add|delete> <word>") st >> return bot
            _ -> evalStateT (replyMsg bot chan nick' "Usage: !banword <list|add|delete> <word>") st >> return bot
                         else return bot
      | x == "!matchword" = if nick' == owner' then
          case (length xs) of
            2 -> if (xs!!0) == "add" then
                   if elem (xs!!1) match' then
                     evalStateT (replyMsg bot chan nick' ("Word " ++ (xs!!1) ++ " already matched.")) st >> return bot
                     else
                     evalStateT (replyMsg bot chan nick' ("Matching word " ++ (xs!!1) ++ ".")) st >>
                     return bot{fugly=f{FuglyLib.match=nub $ match' ++ [(xs!!1)]}}
                 else if (xs!!0) == "delete" then
                   if not $ elem (xs!!1) match' then
                     evalStateT (replyMsg bot chan nick' ("Word " ++ (xs!!1) ++ " not in match list.")) st >> return bot
                     else
                     evalStateT (replyMsg bot chan nick' ("No longer matching word " ++ (xs!!1) ++ ".")) st >>
                     return bot{fugly=f{FuglyLib.match=nub $ delete (xs!!1) match'}}
                 else evalStateT (replyMsg bot chan nick' "Usage: !matchword <list|add|delete> <word>") st >> return bot
            1 -> if (xs!!0) == "list" then
                    evalStateT (replyMsg bot chan nick' ("Matched word list: " ++ unwords match')) st >> return bot
                 else evalStateT (replyMsg bot chan nick' "Usage: !matchword <list|add|delete> <word>") st >> return bot
            _ -> evalStateT (replyMsg bot chan nick' "Usage: !matchword <list|add|delete> <word>") st >> return bot
                         else return bot
      | x == "!namelist" =
          let num = if read (xs!!0) > (100 :: Int) then 100 :: Int else read (xs!!0) in
          case (length xs) of
            1 -> evalStateT (replyMsg bot chan nick' (unwords $ listNamesCountSort2 dict' num)) st
                 >> evalStateT (replyMsg bot chan nick' ("Total name count: " ++ (show $ length $
                                             filter (\x' -> wordIs x' == "name") $ Map.elems dict'))) st
                 >> return bot
            _ -> evalStateT (replyMsg bot chan nick' "Usage: !namelist <number>") st >> return bot
      | x == "!name" = case (length xs) of
            1 -> evalStateT (replyMsg bot chan nick' (listWordFull dict' (xs!!0))) st >> return bot
            _ -> evalStateT (replyMsg bot chan nick' "Usage: !name <name>") st >> return bot
      | x == "!insertname" = if nick' == owner' then
          case (length xs) of
            1 -> do ww <- insertName (snd st) f (xs!!0) [] [] True
                    if isJust $ Map.lookup (xs!!0) dict' then
                      evalStateT (replyMsg bot chan nick' ("Name " ++ (xs!!0) ++ " already in dict.")) st >> return bot
                      else
                      evalStateT (replyMsg bot chan nick' ("Inserted name " ++ (xs!!0) ++ ".")) st >>
                      return bot{fugly=f{dict=ww}}
            _ -> evalStateT (replyMsg bot chan nick' "Usage: !insertname <name>") st >> return bot
                           else return bot
      | x == "!internalize" = if nick' == owner' then
                                if length xs > 1 then do evalStateT (replyMsg bot chan nick' ("Internalizing...")) st
                                                         internalize st bot (read (xs!!0)) $ unwords $ tail xs
                                else
                                  evalStateT (replyMsg bot chan nick' "Usage: !internalize <tries> <msg>") st >> return bot
                           else return bot
      | x == "!talk" = if nick' == owner' then
          if length xs > 2 then do
            tId <- sentenceReply st s f (xs!!0) (xs!!1) randoms' stries' slen plen (drop 2 xs)
            if ttime > 0 then
              forkIO (do threadDelay $ ttime * 1000000 ; evalStateT (hPutStrLnLock stderr ("Killed thread: " ++ show tId)) st ; killThread tId) >> return bot
              else return bot
          else evalStateT (replyMsg bot chan nick' "Usage: !talk <channel> <nick> <msg>") st >> return bot
                     else return bot
      | x == "!raw" = if nick' == owner' then
          if (length xs) > 0 then evalStateT (write s (xs!!0)(unwords $ tail xs)) st >> return bot
          else evalStateT (replyMsg bot chan nick' "Usage: !raw <msg>") st >> return bot
               else return bot
      | x == "!related" = case (length xs) of
            3 -> (wnRelated wne' (xs!!0) (xs!!1) (xs!!2)) >>= (\x' -> evalStateT (replyMsg bot chan nick' x') st) >> return bot
            2 -> (wnRelated wne' (xs!!0) (xs!!1) []) >>= (\x' -> evalStateT (replyMsg bot chan nick' x') st) >> return bot
            1 -> (wnRelated wne' (xs!!0) [] []) >>= (\x' -> evalStateT (replyMsg bot chan nick' x') st) >> return bot
            _ -> evalStateT (replyMsg bot chan nick' "Usage: !related <word> [form] [part-of-speech]") st >> return bot
      | x == "!closure" = case (length xs) of
            3 -> (wnClosure wne' (xs!!0) (xs!!1) (xs!!2)) >>= (\x' -> evalStateT (replyMsg bot chan nick' x') st) >> return bot
            2 -> (wnClosure wne' (xs!!0) (xs!!1) []) >>= (\x' -> evalStateT (replyMsg bot chan nick' x') st) >> return bot
            1 -> (wnClosure wne' (xs!!0) [] []) >>= (\x' -> evalStateT (replyMsg bot chan nick' x') st) >> return bot
            _ -> evalStateT (replyMsg bot chan nick' "Usage: !closure <word> [form] [part-of-speech]") st >> return bot
      | x == "!meet" = case (length xs) of
            3 -> (wnMeet wne' (xs!!0) (xs!!1) (xs!!2)) >>= (\x' -> evalStateT (replyMsg bot chan nick' x') st) >> return bot
            2 -> (wnMeet wne' (xs!!0) (xs!!1) []) >>= (\x' -> evalStateT (replyMsg bot chan nick' x') st) >> return bot
            _ -> evalStateT (replyMsg bot chan nick' "Usage: !meet <word> <word> [part-of-speech]") st >> return bot
      | x == "!parse" = case (length xs) of
            0 -> evalStateT (replyMsg bot chan nick' "Usage: !parse <msg>") st >> return bot
            _ -> (mapM (\x' -> evalStateT (replyMsg bot chan nick' x') st) $ take 3 (gfParseC pgf' (unwords $ take 12 xs))) >> return bot
      | x == "!forms"  = case (length xs) of
            0 -> evalStateT (replyMsg bot chan nick' (concat $ map (++ " ") $ map show allForm)) st >> return bot
            _ -> evalStateT (replyMsg bot chan nick' "Usage: !forms") st >> return bot
      | x == "!parts"  = case (length xs) of
            0 -> evalStateT (replyMsg bot chan nick' (concat $ map (++ " ") $ map show allPOS)) st >> return bot
            _ -> evalStateT (replyMsg bot chan nick' "Usage: !parts") st >> return bot
      | x == "!random" = case (length xs) of
            1 -> evalStateT (replyMsg bot chan nick' (gfRandom pgf' (read (xs!!0)))) st >> return bot
            _ -> evalStateT (replyMsg bot chan nick' "Usage: !random <number>") st >> return bot
      | x == "!gfcats" = case (length xs) of
            0 -> return (unwords $ gfCategories pgf') >>= (\x' -> evalStateT (replyMsg bot chan nick' x') st) >> return bot
            _ -> evalStateT (replyMsg bot chan nick' "Usage: !gfcats") st >> return bot
      | x == "!gflin" = case (length xs) of
            0 -> evalStateT (replyMsg bot chan nick' "Usage: !gflin <msg>") st >> return bot
            _ -> evalStateT (replyMsg bot chan nick' (gfLin pgf' $ unwords xs)) st >> return bot
      | x == "!gfshowexpr" = case (length xs) of
            2 -> evalStateT (replyMsg bot chan nick' (gfShowExpr pgf' (xs!!0) (read(xs!!1)))) st >> return bot
            _ -> evalStateT (replyMsg bot chan nick' "Usage: !gfshowexpr <type> <num>") st >> return bot
      | x == "!asreplace" = case (length xs) of
            0 -> evalStateT (replyMsg bot chan nick' "Usage: !asreplace <msg>") st >> return bot
            _ -> do ww <- asReplaceWords f xs ; evalStateT (replyMsg bot chan nick' $ unwords ww) st >> return bot
      | x == "!wnreplace" = case (length xs) of
            0 -> evalStateT (replyMsg bot chan nick' "Usage: !wnreplace <msg>") st >> return bot
            _ -> do ww <- wnReplaceWords f randoms' xs ; evalStateT (replyMsg bot chan nick' $ unwords ww) st >> return bot
      | x == "!isname" = case (length xs) of
            1 -> do n <- asIsName aspell' (xs!!0)
                    evalStateT (replyMsg bot chan nick' (show n)) st
                    return bot
            _ -> evalStateT (replyMsg bot chan nick' "Usage: !isname <word>") st >> return bot
      | x == "!test" = if nick' == owner' then
            evalStateT (replyMsg bot chan nick' (unwords $ map show $ take 750 $ iterate succ (0 :: Int))) st >> return bot
            else return bot
      | otherwise  = if nick' == owner' then evalStateT (replyMsg bot chan nick'
                       ("Commands: !dict !word !wordlist !insertword !dropword !dropafter "
                       ++ "!banword !matchword !namelist !name !insertname !isname !closure "
                       ++ "!meet !parse !gfcats !gflin !gfshowexpr !asreplace !wnreplace "
                       ++ "!related !random !forms !parts !ageword(s) !cleanwords !internalize "
                       ++ "!params !setparam !showparams !nick !join !part !talk !raw "
                       ++ "!quit !readfile !load !save")) st >> return bot
                     else evalStateT (replyMsg bot chan nick' ("Commands: !dict !word !wordlist !name "
                       ++ "!closure !meet !parse !related !random !forms !parts")) st >> return bot
    execCmd' bot _ = return bot

sentenceReply :: (MVar Bot, MVar ()) -> Handle -> Fugly -> String -> String -> Int -> Int -> Int -> Int -> [String] -> IO ThreadId
sentenceReply st h fugly'@(Fugly{pgf=pgf'}) chan nick' rand stries' slen plen m = forkIO (do
    num <- Random.getStdRandom (Random.randomR (1, 3 :: Int)) :: IO Int
    bloop <- Random.getStdRandom (Random.randomR (0, 4 :: Int)) :: IO Int
    r <- gfRandom2 pgf'
    let rr = filter (\x -> x =~ "NP") $ words r
    x1 <- f ((sentence (snd st) fugly' rand stries' slen plen m) ++ [return r]) [] num 0
    x2 <- f ((sentence (snd st) fugly' rand stries' slen plen m)) [] num 0
    let ww = unwords $ if rand > 50 && rr == (\\) rr (words r) then x1 else x2
    evalStateT (do if null ww then return ()
                     else if null nick' then hPutStrLnLock h ("PRIVMSG " ++ (chan ++ " :" ++ ww) ++ "\r") >>
                                             hPutStrLnLock stdout ("> PRIVMSG " ++ (chan ++ " :" ++ ww))
                          else if nick' == chan || bloop == 0 then hPutStrLnLock h ("PRIVMSG " ++ (chan ++ " :" ++ ww) ++ "\r") >>
                                                                   hPutStrLnLock stdout ("> PRIVMSG " ++ (chan ++ " :" ++ ww))
                               else hPutStrLnLock h ("PRIVMSG " ++ (chan ++ " :" ++ nick' ++ ": " ++ ww) ++ "\r") >>
                                    hPutStrLnLock stdout ("> PRIVMSG " ++ (chan ++ " :" ++ nick' ++ ": " ++ ww))) st)
  where
    f :: [IO String] -> [String] -> Int -> Int -> IO [String]
    f []     a _ _ = return a
    f (x:xs) a n i = do
      xx <- x
      if i >= n then return a
        else if null xx then f xs a n i
        else f xs ([xx ++ " "] ++ a) n (i + 1)

replyMsg :: Bot -> String -> String -> String -> StateT (MVar Bot, MVar ()) IO ()
replyMsg bot@(Bot{sock=s, params=p@(Parameter{maxchanmsg=mcm})}) chan nick' msg
    | null nick'      = if length msg > mcm then do
      write s "PRIVMSG" (chan ++ " :" ++ (take mcm msg))
      lift $ threadDelay 2000000
      replyMsg bot chan [] (drop mcm msg) else
        write s "PRIVMSG" (chan ++ " :" ++ msg)
    | chan == nick'   = if length msg > mcm then do
      write s "PRIVMSG" (nick' ++ " :" ++ (take mcm msg))
      lift $ threadDelay 2000000
      replyMsg bot chan nick' (drop mcm msg) else
        write s "PRIVMSG" (nick' ++ " :" ++ msg)
    | otherwise      = if length msg > mcm then do
      write s "PRIVMSG" (chan ++ " :" ++ nick' ++ ": " ++ (take mcm msg))
      lift $ threadDelay 2000000
      replyMsg bot chan nick' (drop mcm msg) else
        write s "PRIVMSG" (chan ++ " :" ++ nick' ++ ": " ++ msg)
replyMsg _ _ _ _ = return ()

insertFromFile :: (MVar ()) -> Bot -> FilePath -> IO Bot
insertFromFile _ b [] = return b
insertFromFile st bot@(Bot{params=p@(Parameter{autoname=a}), fugly=f}) file = do
    ff <- readFile file
    fmsg <- asReplaceWords f $ map cleanString $ words ff
    n <- insertWords st f a fmsg
    return bot{fugly=f{dict=n}}
insertFromFile _ b _ = return b

internalize :: (MVar Bot, MVar ()) -> Bot -> Int -> String -> IO Bot
internalize _ b 0 _   = return b
internalize _ b _ []  = return b
internalize st b n msg = internalize' st b n 0 msg
  where
    internalize' :: (MVar Bot, MVar ()) -> Bot -> Int -> Int -> String -> IO Bot
    internalize' _ bot _ _ [] = return bot
    internalize' st' bot@(Bot{params=p@(Parameter{autoname=aname, stries=tries, slength=slen, plength=plen, randoms=rands}), fugly=f}) num i imsg = do
      mm <- chooseWord $ words imsg
      sen <- getSentence $ sentence (snd st') f rands tries slen plen mm
      nd <- insertWords (snd st') f aname $ words sen
      r <- Random.getStdRandom (Random.randomR (0, 2)) :: IO Int
      if i >= num then return bot
        else if r == 0 then evalStateT (hPutStrLnLock stdout ("> internalize: " ++ msg)) st >> internalize' st bot{fugly=f{dict=nd}} num (i + 1) msg
             else evalStateT (hPutStrLnLock stdout ("> internalize: " ++ sen)) st >> internalize' st bot{fugly=f{dict=nd}} num (i + 1) sen
    internalize' _ bot _ _ _ = return bot
    getSentence []     = return []
    getSentence (x:xs) = do
      ww <- x
      if null ww then getSentence xs
        else return ww

write :: Handle -> [Char] -> [Char] -> StateT (MVar Bot, MVar ()) IO ()
write _ [] _  = return ()
write _ _ []  = return ()
write sock' s msg = do
  hPutStrLnLock sock'  (s ++ " " ++ msg ++ "\r")
  hPutStrLnLock stdout ("> " ++ s ++ " " ++ msg)

hPutStrLnLock :: Handle -> String -> StateT (MVar Bot, MVar ()) IO ()
hPutStrLnLock s m = do
  st <- get :: StateT (MVar Bot, MVar ()) IO (MVar Bot, MVar ())
  let l = snd st
  lock <- lift $ takeMVar l
  lift (do hPutStrLn s m ; putMVar l lock)
