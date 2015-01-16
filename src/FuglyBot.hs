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
               | Autoname | AllowPM | Topic | Randoms | ReplaceWords | UnknownParam
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
                 rwords      :: Bool,
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
    toEnum 15 = ReplaceWords
    toEnum 16 = ThreadTime
    toEnum 17 = UnknownParam
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
    fromEnum ReplaceWords   = 15
    fromEnum ThreadTime     = 16
    fromEnum UnknownParam   = 17
    fromEnum _              = 17
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
readParam a | (map toLower a) == "rwords"          = ReplaceWords
readParam a | (map toLower a) == "replacewords"    = ReplaceWords
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
              Bot sh (Parameter nick' owner' fdir False 10 400 20 7 0 True False True False topic' 50 False 30) f
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
                 (do nb <- evalStateT (changeNick lll) st ; swapMVar b nb) >> return ()
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

changeNick :: [String] -> StateT (MVar Bot, MVar ()) IO Bot
changeNick [] = do
    st <- get
    bot <- lift $ readMVar $ fst st
    return bot
changeNick line = do
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
                                 topic="", randoms=read (a!!10), rwords=read (a!!11),
                                 threadtime=read (a!!12)}

paramsToList :: Parameter -> [String]
paramsToList (Parameter _ _ _ uc rk mcm st sl pl l stl an apm _ r rw tt) = [show uc, show rk, show mcm, show st,
                                                                            show sl, show pl, show l, show stl,
                                                                            show an, show apm, show r, show rw,
                                                                            show tt]
paramsToList _ = []

changeParam :: Bot -> String -> String -> String -> String -> StateT (MVar Bot, MVar ()) IO Bot
changeParam bot@(Bot{sock=s, params=p@(Parameter{nick=botnick, owner=owner', fuglydir=fdir, topic=t}),
                     fugly=f@(Fugly{dict=dict', ban=ban', FuglyLib.match=match'})}) chan nick' param value = do
    st <- get
    case (readParam param) of
      Nick           -> (write s "NICK" $ cleanStringWhite isAscii value)     >> return bot
      Owner          -> replyMsg' value                 "Owner"               >>= (\x -> return bot{params=p{owner=x}})
      UserCommands   -> replyMsg' (readBool value)      "User commands"       >>= (\x -> return bot{params=p{usercmd=x}})
      RejoinKick     -> replyMsg' (readInt 1 4096 value) "Rejoin kick time"   >>= (\x -> return bot{params=p{rejoinkick=x}})
      MaxChanMsg     -> replyMsg' (readInt 9 450 value) "Max channel message" >>= (\x -> return bot{params=p{maxchanmsg=x}})
      SentenceTries  -> replyMsg' (readInt 1 4096 value) "Sentence tries"     >>= (\x -> return bot{params=p{stries=x}})
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
      ReplaceWords   -> replyMsg' (readBool value)      "Replace words"       >>= (\x -> return bot{params=p{rwords=x}})
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
    | c == (n ++ ",") = True
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
                                 slen plen learning' slearn autoname' allowpm'
                                 _ randoms' rwords' ttime),
           fugly=f@(Fugly{pgf=pgf', FuglyLib.match=match'})}) chan nick' msg = do
    st <- get :: StateT (MVar Bot, MVar ()) IO (MVar Bot, MVar ())
    let mmsg = if null $ head msg then msg
                 else case fLast ' ' $ head msg of
                   ':' -> tail msg
                   ',' -> tail msg
                   _   -> msg
    let fmsg = map cleanString mmsg
    let parse = if slearn then gfParseBool pgf' 7 $ unwords fmsg else True
    let matchon = map toLower (" " ++ intercalate " | " (botnick : match') ++ " ")
    mm <- lift $ chooseWord fmsg
    tId <- lift $ (if null chan then
                     if allowpm' then
                       sentenceReply st s f nick' [] rwords' randoms' stries' slen plen mm
                     else forkIO $ return ()
                   else if null nick' then
                          if map toLower (unwords msg) =~ matchon then
                            sentenceReply st s f chan chan rwords' randoms' stries' slen plen mm
                          else forkIO $ return ()
                        else sentenceReply st s f chan nick' rwords' randoms' stries' slen plen mm)
    if ttime > 0 then
      lift $ forkIO (do threadDelay $ ttime * 1000000 ; evalStateT (hPutStrLnLock stderr ("> debug: killed thread: " ++ show tId)) st ; killThread tId) >> return ()
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
                                        autoname' allowpm' topic' randoms' rwords' ttime),
                      fugly=f@(Fugly dict' pgf' wne' aspell' ban' match')}) st
      | usercmd' == False && nick' /= owner' = return bot
      | x == "!quit" = if nick' == owner' then case (length xs) of
          0 -> do evalStateT (write s "QUIT" ":Bye") st >> return bot
          _ -> do evalStateT (write s "QUIT" (":" ++ unwords xs)) st >> return bot
                       else return bot
      | x == "!save" = if nick' == owner' then
                         catch (saveDict (snd st) f fdir topic' (paramsToList p))
                         (\e -> do let err = show (e :: SomeException)
                                   evalStateT (hPutStrLnLock stderr ("Exception in saveDict: " ++ err)) st
                                   return ())
                         >> replyMsgT st bot chan nick' "Saved dict file!" >> return bot
                       else return bot
      | x == "!load" = if nick' == owner' then do
           (nd, nb, nm, np) <- catch (loadDict fdir topic') (\e -> do let err = show (e :: SomeException)
                                                                      evalStateT (hPutStrLnLock stderr ("Exception in loadDict: " ++ err)) st
                                                                      return (dict', ban', match', paramsToList p))
           replyMsgT st bot chan nick' "Loaded dict file!"
           return bot{params=(readParamsFromList np){nick=botnick, owner=owner', fuglydir=fdir, topic=topic'},
                      fugly=(Fugly nd pgf' wne' aspell' nb nm)}
                       else return bot
      | x == "!join" = if nick' == owner' then evalStateT (joinChannel s "JOIN" xs) st >> return bot else return bot
      | x == "!part" = if nick' == owner' then evalStateT (joinChannel s "PART" xs) st >> return bot else return bot
      | x == "!nick" = if nick' == owner' then case (length xs) of
          1 -> evalStateT ((\x' -> write s "NICK" $ cleanStringWhite isAscii x') (xs!!0)) st >> return bot
          _ -> replyMsgT st bot chan nick' "Usage: !nick <nick>" >> return bot
                       else return bot
      | x == "!readfile" = if nick' == owner' then case (length xs) of
          1 -> catch (insertFromFile (snd st) bot (xs!!0)) (\e -> do let err = show (e :: SomeException)
                                                                     evalStateT (hPutStrLnLock stderr ("Exception in insertFromFile: " ++ err)) st
                                                                     return bot)
          _ -> replyMsgT st bot chan nick' "Usage: !readfile <file>" >> return bot
                           else return bot
      | x == "!internalize" = if nick' == owner' then
          if length xs > 1 then do replyMsgT st bot chan nick' ("Internalizing...")
                                   internalize st bot (read (xs!!0)) $ unwords $ tail xs
          else
            replyMsgT st bot chan nick' "Usage: !internalize <tries> <msg>" >> return bot
                              else return bot
      | x == "!showparams" = if nick' == owner' then case (length xs) of
          0 -> replyMsgT st bot chan nick' ("nick: " ++ botnick ++ "  owner: " ++ owner' ++
                   "  usercommands: " ++ show usercmd' ++ "  rejoinkick: "
                   ++ show rkick ++ "  maxchanmsg: " ++ show maxcmsg
                   ++ "  sentencetries: " ++ show stries' ++ "  sentencelength: "
                   ++ show slen ++ "  parselength: " ++ show plen
                   ++ "  learning: " ++ show learn ++ "  strictlearn: " ++ show slearn
                   ++ "  autoname: " ++ show autoname' ++ "  allowpm: " ++ show allowpm'
                   ++ "  topic: " ++ topic' ++ "  randoms: " ++ show randoms'
                   ++ "  replacewords: " ++ show rwords' ++ "  threadtime: " ++ show ttime) >> return bot
          _ -> replyMsgT st bot chan nick' "Usage: !showparams" >> return bot
                             else return bot
      | x == "!setparam" = if nick' == owner' then case (length xs) of
          2 -> evalStateT (changeParam bot chan nick' (xs!!0) (xs!!1)) st
          _ -> replyMsgT st bot chan nick' "Usage: !setparam <parameter> <value>" >> return bot
                           else return bot
      | x == "!word" || x == "!name" || x == "!acronym" = case (length xs) of
          1 -> replyMsgT st bot chan nick' (listWordFull dict' (xs!!0)) >> return bot
          _ -> replyMsgT st bot chan nick' ("Usage: " ++ x ++ " <" ++ (tail x) ++ ">") >> return bot
      | x == "!wordlist" || x == "!namelist" || x == "!acronymlist" =
          let num = if read (xs!!0) > (100 :: Int) then 100 :: Int else read (xs!!0) in
          case (length xs) of
            1 -> replyMsgT st bot chan nick' (unwords $ listWordsCountSort dict' num (x =~ "word|name|acronym")) >>
                   replyMsgT st bot chan nick' ("Total " ++ (x =~ "word|name|acronym") ++ " count: " ++
                                            (show $ numWords dict' (x =~ "word|name|acronym"))) >> return bot
            _ -> replyMsgT st bot chan nick' ("Usage: " ++ x ++ " <number>") >> return bot
      | x == "!insertword" = if nick' == owner' then case (length xs) of
          2 -> do ww <- insertWordRaw (snd st) f (xs!!1) [] [] (xs!!0)
                  if isJust $ Map.lookup (xs!!1) dict' then
                    replyMsgT st bot chan nick' ("Word " ++ (xs!!1) ++ " already in dict.") >> return bot
                    else
                    replyMsgT st bot chan nick' ("Inserted word " ++ (xs!!1) ++ ".") >> return bot{fugly=f{dict=ww}}
          _ -> replyMsgT st bot chan nick' "Usage: !insertword <pos> <word>" >> return bot
                             else return bot
      | x == "!insertname" = if nick' == owner' then case (length xs) of
          1 -> do ww <- insertNameRaw (snd st) f (xs!!0) [] [] True
                  if isJust $ Map.lookup (xs!!0) dict' then
                    replyMsgT st bot chan nick' ("Name " ++ (xs!!0) ++ " already in dict.") >> return bot
                    else
                    replyMsgT st bot chan nick' ("Inserted name " ++ (xs!!0) ++ ".") >> return bot{fugly=f{dict=ww}}
          _ -> replyMsgT st bot chan nick' "Usage: !insertname <name>" >> return bot
                             else return bot
      | x == "!insertacronym" = if nick' == owner' then
            if length xs > 1 then do
              ww <- insertAcroRaw (snd st) f (xs!!0) [] [] (unwords $ tail xs)
              if isJust $ Map.lookup (xs!!0) dict' then
                replyMsgT st bot chan nick' ("Acronym " ++ (xs!!0) ++ " already in dict.") >> return bot
                else
                replyMsgT st bot chan nick' ("Inserted acronym " ++ (xs!!0) ++ ".") >> return bot{fugly=f{dict=ww}}
              else
              replyMsgT st bot chan nick' "Usage: !insertacronym <acronym> <definition>" >> return bot
                                else return bot
      | x == "!dropword" = if nick' == owner' then case (length xs) of
          1 -> if isJust $ Map.lookup (xs!!0) dict' then
                 replyMsgT st bot chan nick' ("Dropped word " ++ (xs!!0) ++ ".") >>
                   return bot{fugly=f{dict=dropWord dict' (xs!!0)}}
               else {-- Drop the word anyway because it might be a before or after word. --}
                 replyMsgT st bot chan nick' ("Word " ++ (xs!!0) ++ " not in dict.") >>
                   return bot{fugly=f{dict=dropWord dict' (xs!!0)}}
          _ -> replyMsgT st bot chan nick' "Usage: !dropword <word>" >> return bot
                           else return bot
      | x == "!dropafter" = if nick' == owner' then case (length xs) of
          2 -> if (isJust $ Map.lookup (xs!!0) dict') && (isJust $ Map.lookup (xs!!1) dict') then
                 let nd = dropBefore dict' (xs!!1) (xs!!0) in
                 replyMsgT st bot chan nick' ("Dropped word " ++ (xs!!1) ++ " after word " ++ (xs!!0) ++ ".") >>
                   return bot{fugly=f{dict=dropAfter nd (xs!!0) (xs!!1)}}
               else {-- Drop the association anyway, but report errors. --}
                 replyMsgT st bot chan nick' ("Word " ++ (xs!!0) ++ " or word " ++ (xs!!1) ++ " not in dict.") >>
                   return bot{fugly=f{dict=dropAfter (dropBefore dict' (xs!!1) (xs!!0)) (xs!!0) (xs!!1)}}
          1 -> if isJust $ Map.lookup (xs!!0) dict' then
                 replyMsgT st bot chan nick' ("Dropped all words after word " ++ (xs!!0) ++ ".") >>
                   return bot{fugly=f{dict=dropAllAfter dict' (xs!!0)}}
               else
                 replyMsgT st bot chan nick' ("Word " ++ (xs!!0) ++ " not in dict.") >>
                   return bot{fugly=f{dict=dropAllAfter dict' (xs!!0)}}
          _ -> replyMsgT st bot chan nick' "Usage: !dropafter <word> [after-word]" >> return bot
                            else return bot
      | x == "!banafter" = if nick' == owner' then case (length xs) of
          3 -> if (xs!!0) == "add" then let w = Map.lookup (xs!!1) dict' in
                 if isJust w then
                   if elem (xs!!2) $ wordGetBanAfter $ fromJust w then
                     replyMsgT st bot chan nick' ("Word " ++ (xs!!2) ++ " after word " ++ (xs!!1) ++ " already banned.") >> return bot
                     else let nd1 = Map.insert (xs!!1) (addBanAfter (fromJust w) (xs!!2)) dict'
                              nd2 = dropBefore nd1 (xs!!2) (xs!!1) in
                       replyMsgT st bot chan nick' ("Banned word " ++ (xs!!2) ++ " after word " ++ (xs!!1) ++ ".") >>
                         return bot{fugly=f{dict=dropAfter nd2 (xs!!1) (xs!!2)}}
                   else replyMsgT st bot chan nick' ("Word " ++ (xs!!1) ++ " not in dict.") >> return bot
               else if (xs!!0) == "delete" then let w = Map.lookup (xs!!1) dict' in
                 if isJust w then
                   if not $ elem (xs!!2) $ wordGetBanAfter $ fromJust w then
                     replyMsgT st bot chan nick' ("Word " ++ (xs!!2) ++ " not in ban list.") >> return bot
                     else
                       replyMsgT st bot chan nick' ("Unbanned word " ++ (xs!!2) ++ " after word " ++ (xs!!1) ++ ".") >>
                         return bot{fugly=f{dict=Map.insert (xs!!1) (deleteBanAfter (fromJust w) (xs!!2)) dict'}}
                   else replyMsgT st bot chan nick' ("Word " ++ (xs!!1) ++ " not in dict.") >> return bot
               else replyMsgT st bot chan nick' "Usage: !banafter <list|add|delete> <word> <after-word>" >> return bot
          2 -> if (xs!!0) == "list" then let w = Map.lookup (xs!!1) dict' in
                 if isJust w then
                   replyMsgT st bot chan nick' ("Banned after word list: " ++ (unwords $ wordGetBanAfter $ fromJust w)) >> return bot
                   else replyMsgT st bot chan nick' ("Word " ++ (xs!!1) ++ " not in dict.") >> return bot
               else replyMsgT st bot chan nick' "Usage: !banafter <list|add|delete> <word> <after-word>" >> return bot
          _ -> replyMsgT st bot chan nick' "Usage: !banafter <list|add|delete> <word> <after-word>" >> return bot
                           else return bot
      | x == "!ageword" = if nick' == owner' then case (length xs) of
          2 -> do num <- catch (if (read (xs!!1) :: Int) > 50 then return 50 else return (read (xs!!1) :: Int))
                              {-- This has to be caught here? --}
                              (\e -> do let err = show (e :: SomeException)
                                        evalStateT (hPutStrLnLock stderr ("Exception in ageword command: read: " ++ err)) st
                                        return 0)
                  if isJust $ Map.lookup (xs!!0) dict' then
                    replyMsgT st bot chan nick' ("Aged word " ++ (xs!!0) ++ ".") >>
                      return bot{fugly=f{dict=ageWord dict' (xs!!0) num}}
                    else
                    replyMsgT st bot chan nick' ("Word " ++ (xs!!0) ++ " not in dict.") >> return bot
          _ -> replyMsgT st bot chan nick' "Usage: !ageword <word> <number>" >> return bot
                          else return bot
      | x == "!agewords" = if nick' == owner' then case (length xs) of
          1 -> do num <- catch (if (read (xs!!0) :: Int) > 50 then return 50 else return (read (xs!!0) :: Int))
                              {-- This has to be caught here? --}
                              (\e -> do let err = show (e :: SomeException)
                                        evalStateT (hPutStrLnLock stderr ("Exception in agewords command: read: " ++ err)) st
                                        return 0)
                  replyMsgT st bot chan nick' ("Aged all words...")
                  return bot{fugly=f{dict=ageWords dict' num}}
          _ -> replyMsgT st bot chan nick' "Usage: !agewords <number>" >> return bot
                           else return bot
      | x == "!banword" = if nick' == owner' then case (length xs) of
          2 -> if (xs!!0) == "add" then
                 if elem (xs!!1) ban' then
                   replyMsgT st bot chan nick' ("Word " ++ (xs!!1) ++ " already banned.") >> return bot
                 else
                   replyMsgT st bot chan nick' ("Banned word " ++ (xs!!1) ++ ".") >>
                     return bot{fugly=f{dict=dropWord dict' (xs!!1), ban=nub $ ban' ++ [(xs!!1)]}}
               else if (xs!!0) == "delete" then
                 if not $ elem (xs!!1) ban' then
                   replyMsgT st bot chan nick' ("Word " ++ (xs!!1) ++ " not in ban list.") >> return bot
                 else
                   replyMsgT st bot chan nick' ("Unbanned word " ++ (xs!!1) ++ ".") >>
                     return bot{fugly=f{ban=nub $ delete (xs!!1) ban'}}
                 else replyMsgT st bot chan nick' "Usage: !banword <list|add|delete> <word>" >> return bot
          1 -> if (xs!!0) == "list" then
                 replyMsgT st bot chan nick' ("Banned word list: " ++ unwords ban') >> return bot
               else replyMsgT st bot chan nick' "Usage: !banword <list|add|delete> <word>" >> return bot
          _ -> replyMsgT st bot chan nick' "Usage: !banword <list|add|delete> <word>" >> return bot
                          else return bot
      | x == "!matchword" = if nick' == owner' then case (length xs) of
          2 -> if (xs!!0) == "add" then
                 if elem (xs!!1) match' then
                   replyMsgT st bot chan nick' ("Word " ++ (xs!!1) ++ " already matched.") >> return bot
                 else
                   replyMsgT st bot chan nick' ("Matching word " ++ (xs!!1) ++ ".") >>
                     return bot{fugly=f{FuglyLib.match=nub $ match' ++ [(xs!!1)]}}
               else if (xs!!0) == "delete" then
                 if not $ elem (xs!!1) match' then
                   replyMsgT st bot chan nick' ("Word " ++ (xs!!1) ++ " not in match list.") >> return bot
                 else
                   replyMsgT st bot chan nick' ("No longer matching word " ++ (xs!!1) ++ ".") >>
                     return bot{fugly=f{FuglyLib.match=nub $ delete (xs!!1) match'}}
                 else replyMsgT st bot chan nick' "Usage: !matchword <list|add|delete> <word>" >> return bot
          1 -> if (xs!!0) == "list" then
                 replyMsgT st bot chan nick' ("Matched word list: " ++ unwords match') >> return bot
               else replyMsgT st bot chan nick' "Usage: !matchword <list|add|delete> <word>" >> return bot
          _ -> replyMsgT st bot chan nick' "Usage: !matchword <list|add|delete> <word>" >> return bot
                            else return bot
      | x == "!talk" = if nick' == owner' then
          if length xs > 2 then do
            tId <- sentenceReply st s f (xs!!0) (xs!!1) rwords' randoms' stries' slen plen (drop 2 xs)
            if ttime > 0 then
              forkIO (do threadDelay $ ttime * 1000000
                         evalStateT (hPutStrLnLock stderr ("> debug: killed thread: " ++ show tId)) st
                         killThread tId) >> return bot
              else return bot
          else replyMsgT st bot chan nick' "Usage: !talk <channel> <nick> <msg>" >> return bot
                       else return bot
      | x == "!raw" = if nick' == owner' then
          if (length xs) > 0 then evalStateT (write s (xs!!0)(unwords $ tail xs)) st >> return bot
          else replyMsgT st bot chan nick' "Usage: !raw <msg>" >> return bot
                      else return bot
      | x == "!dict" = case (length xs) of
          2 -> (dictLookup (snd st) f (xs!!0) (xs!!1)) >>= (\x' -> replyMsgT st bot chan nick' x') >> return bot
          1 -> (dictLookup (snd st) f (xs!!0) []) >>= (\x' -> replyMsgT st bot chan nick' x') >> return bot
          _ -> replyMsgT st bot chan nick' "Usage: !dict <word> [part-of-speech]" >> return bot
      | x == "!closure" = case (length xs) of
          3 -> (wnClosure wne' (xs!!0) (xs!!1) (xs!!2)) >>= (\x' -> replyMsgT st bot chan nick' x') >> return bot
          2 -> (wnClosure wne' (xs!!0) (xs!!1) []) >>= (\x' -> replyMsgT st bot chan nick' x') >> return bot
          1 -> (wnClosure wne' (xs!!0) [] []) >>= (\x' -> replyMsgT st bot chan nick' x') >> return bot
          _ -> replyMsgT st bot chan nick' "Usage: !closure <word> [form] [part-of-speech]" >> return bot
      | x == "!meet" = case (length xs) of
          3 -> (wnMeet wne' (xs!!0) (xs!!1) (xs!!2)) >>= (\x' -> replyMsgT st bot chan nick' x') >> return bot
          2 -> (wnMeet wne' (xs!!0) (xs!!1) []) >>= (\x' -> replyMsgT st bot chan nick' x') >> return bot
          _ -> replyMsgT st bot chan nick' "Usage: !meet <word> <word> [part-of-speech]" >> return bot
      | x == "!parse" = if nick' == owner' then case (length xs) of
          0 -> replyMsgT st bot chan nick' "Usage: !parse <msg>" >> return bot
          _ -> (mapM (\x' -> replyMsgT st bot chan nick' x') $ take 3 (gfParseC pgf' (unwords $ take 12 xs))) >> return bot
                        else return bot
      | x == "!related" = case (length xs) of
          3 -> (wnRelated wne' (xs!!0) (xs!!1) (xs!!2)) >>= (\x' -> replyMsgT st bot chan nick' x') >> return bot
          2 -> (wnRelated wne' (xs!!0) (xs!!1) []) >>= (\x' -> replyMsgT st bot chan nick' x') >> return bot
          1 -> (wnRelated wne' (xs!!0) [] []) >>= (\x' -> replyMsgT st bot chan nick' x') >> return bot
          _ -> replyMsgT st bot chan nick' "Usage: !related <word> [form] [part-of-speech]" >> return bot
      | x == "!forms"  = case (length xs) of
          0 -> replyMsgT st bot chan nick' (concat $ map (++ " ") $ map show allForm) >> return bot
          _ -> replyMsgT st bot chan nick' "Usage: !forms" >> return bot
      | x == "!parts"  = case (length xs) of
          0 -> replyMsgT st bot chan nick' (concat $ map (++ " ") $ map show allPOS) >> return bot
          _ -> replyMsgT st bot chan nick' "Usage: !parts" >> return bot
      | x == "!isname" = case (length xs) of
          1 -> do n <- asIsName (snd st) aspell' (xs!!0)
                  replyMsgT st bot chan nick' (show n)
                  return bot
          _ -> replyMsgT st bot chan nick' "Usage: !isname <word>" >> return bot
      | x == "!isacronym" = case (length xs) of
          1 -> do n <- asIsAcronym (snd st) aspell' (xs!!0)
                  replyMsgT st bot chan nick' (show n)
                  return bot
          _ -> replyMsgT st bot chan nick' "Usage: !isacronym <word>" >> return bot
      -- | x == "!commas" = if (length xs) > 0 then do
      --     m <- insertCommas wne' 0 $ return xs
      --     replyMsgT st bot chan nick' (unwords m) >> return bot
      --                    else replyMsgT st bot chan nick' "Usage: !commas <msg>" >> return bot
      -- | x == "!asreplace" = case (length xs) of
      --     0 -> replyMsgT st bot chan nick' "Usage: !asreplace <msg>" >> return bot
      --     _ -> do ww <- asReplaceWords f xs ; replyMsgT st bot chan nick' $ unwords ww >> return bot
      -- | x == "!wnreplace" = case (length xs) of
      --     0 -> replyMsgT st bot chan nick' "Usage: !wnreplace <msg>" >> return bot
      --     _ -> do ww <- wnReplaceWords f True randoms' xs ; replyMsgT st bot chan nick' $ unwords ww >> return bot
      -- | x == "!random" = case (length xs) of
      --     1 -> replyMsgT st bot chan nick' (gfRandom pgf' (read (xs!!0))) >> return bot
      --     _ -> replyMsgT st bot chan nick' "Usage: !random <number>" >> return bot
      -- | x == "!gfcats" = case (length xs) of
      --     0 -> return (unwords $ gfCategories pgf') >>= (\x' -> replyMsgT st bot chan nick' x') >> return bot
      --     _ -> replyMsgT st bot chan nick' "Usage: !gfcats" >> return bot
      -- | x == "!gflin" = case (length xs) of
      --     0 -> replyMsgT st bot chan nick' "Usage: !gflin <msg>" >> return bot
      --     _ -> replyMsgT st bot chan nick' (gfLin pgf' $ unwords xs) >> return bot
      -- | x == "!gfshowexpr" = case (length xs) of
      --     2 -> replyMsgT st bot chan nick' (gfShowExpr pgf' (xs!!0) (read(xs!!1))) >> return bot
      --     _ -> replyMsgT st bot chan nick' "Usage: !gfshowexpr <type> <num>" >> return bot
      -- | x == "!params" = if nick' == owner' then replyMsgT st bot chan nick' (init (concat $ map (++ " ")
      --                                 $ map show $ init allParams)) >> return bot
      --                    else return bot
      -- | x == "!test" = if nick' == owner' then
      --     replyMsgT st bot chan nick' (unwords $ map show $ take 750 $ iterate succ (0 :: Int)) >> return bot
      --     else return bot
      | otherwise  = if nick' == owner' then replyMsgT st bot chan nick'
          ("Commands: !word !wordlist !insertword !name !namelist !insertname !acronym !acronymlist !insertacronym "
          ++ "!dropword !dropafter !banafter !ageword(s) !internalize "
          ++ "!banword !matchword "
          ++ "!dict !closure !meet !parse !related !forms !parts !isname !isacronym "
          ++ "!setparam !showparams !nick !join !part !talk !raw !quit !readfile !load !save") >> return bot
                     else replyMsgT st bot chan nick'
          ("Commands: !word !wordlist !name !namelist "
          ++ "!dict !closure !meet !related !forms !parts !isname !isacronym") >> return bot
    execCmd' bot _ = return bot

sentenceReply :: (MVar Bot, MVar ()) -> Handle -> Fugly -> String -> String -> Bool -> Int -> Int -> Int -> Int -> [String] -> IO ThreadId
sentenceReply st h fugly'@(Fugly{pgf=pgf'}) chan nick' rwords' rand stries' slen plen m = forkIO (do
    num' <- Random.getStdRandom (Random.randomR (1, 7 :: Int)) :: IO Int
    let num = if num' - 4 < 1 || stries' < 4 then 1 else num' - 4
    bloop <- Random.getStdRandom (Random.randomR (0, 4 :: Int)) :: IO Int
    x <- f ((sentence (snd st) fugly' rwords' rand stries' slen plen m) ++ [gf []]) [] num 0
    let ww = unwords x
    evalStateT (do if null ww then return ()
                     else if null nick' then hPutStrLnLock h ("PRIVMSG " ++ (chan ++ " :" ++ ww) ++ "\r") >>
                                             hPutStrLnLock stdout ("> PRIVMSG " ++ (chan ++ " :" ++ ww))
                          else if nick' == chan || bloop == 0 then hPutStrLnLock h ("PRIVMSG " ++ (chan ++ " :" ++ ww) ++ "\r") >>
                                                                   hPutStrLnLock stdout ("> PRIVMSG " ++ (chan ++ " :" ++ ww))
                               else hPutStrLnLock h ("PRIVMSG " ++ (chan ++ " :" ++ nick' ++ ": " ++ ww) ++ "\r") >>
                                    hPutStrLnLock stdout ("> PRIVMSG " ++ (chan ++ " :" ++ nick' ++ ": " ++ ww))) st)
  where
    gf :: String -> IO String
    gf [] = do
      r <- gfRandom2 pgf'
      let rr = filter (\x -> x =~ "NP") $ words r
      gf (if rr == (\\) rr (words r) then r else [])
    gf msg = return msg
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

replyMsgT :: (MVar Bot, MVar ()) -> Bot -> String -> String -> String -> IO ()
replyMsgT st bot chan nick' msg = evalStateT (replyMsg bot chan nick' msg) st

insertFromFile :: (MVar ()) -> Bot -> FilePath -> IO Bot
insertFromFile _ b [] = return b
insertFromFile st bot@(Bot{params=p@(Parameter{autoname=a}), fugly=f}) file = do
    ff <- readFile file
    fmsg <- asReplaceWords st f $ map cleanString $ words ff
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
    internalize' st' bot@(Bot{params=p@(Parameter{autoname=aname, stries=tries, slength=slen,
                                                  plength=plen, rwords=rw, randoms=rands}), fugly=f}) num i imsg = do
      mm <- chooseWord $ words imsg
      sen <- getSentence $ sentence (snd st') f rw rands tries slen plen mm
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
