import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Lazy
import           Data.Char
import           Data.List
import qualified Data.Map.Lazy                  as Map
import           Data.Maybe
import           Network.Socket                 hiding (Debug)
import           Network.Socks5
import           Prelude
import           System.Environment
import           System.IO
import           System.IO.Error
import qualified System.Random                  as Random
import           Text.Regex.Posix

import           FuglyLib
import           NLP.WordNet.PrimTypes          (allForm, allPOS)

data Bot = Bot {
    sock   :: Handle,
    params :: Parameter,
    fugly  :: Fugly
    }

data Parameter = Nick | Owner | DictFile | UserCommands | RejoinKick | MaxChanMsg | Debug
               | SentenceTries | SentenceLength | ParseLength | Learning | StrictLearn | StrictTopic
               | Autoname | AllowPM | Topic | Randoms | ReplaceWords | Timer | Delay
               | Greetings | Actions | UnknownParam | Parameter {
                 nick        :: String,
                 owner       :: String,
                 fuglydir    :: FilePath,
                 dictfile    :: String,
                 usercmd     :: Bool,
                 rejoinkick  :: Int,
                 maxchanmsg  :: Int,
                 stries      :: Int,
                 slength     :: Int,
                 plength     :: Int,
                 learning    :: Bool,
                 strictlearn :: Bool,
                 stricttopic :: Bool,
                 autoname    :: Bool,
                 allowpm     :: Bool,
                 debug       :: Bool,
                 topic       :: String,
                 randoms     :: Int,
                 rwords      :: Bool,
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
    toEnum 7  = SentenceTries
    toEnum 8  = SentenceLength
    toEnum 9  = ParseLength
    toEnum 10 = Learning
    toEnum 11 = StrictLearn
    toEnum 12 = StrictTopic
    toEnum 13 = Autoname
    toEnum 14 = AllowPM
    toEnum 15 = Debug
    toEnum 16 = Topic
    toEnum 17 = Randoms
    toEnum 18 = ReplaceWords
    toEnum 19 = Timer
    toEnum 20 = Delay
    toEnum 21 = Greetings
    toEnum 22 = Actions
    toEnum 23 = UnknownParam
    toEnum _  = UnknownParam
    fromEnum Nick           = 1
    fromEnum Owner          = 2
    fromEnum DictFile       = 3
    fromEnum UserCommands   = 4
    fromEnum RejoinKick     = 5
    fromEnum MaxChanMsg     = 6
    fromEnum SentenceTries  = 7
    fromEnum SentenceLength = 8
    fromEnum ParseLength    = 9
    fromEnum Learning       = 10
    fromEnum StrictLearn    = 11
    fromEnum StrictTopic    = 12
    fromEnum Autoname       = 13
    fromEnum AllowPM        = 14
    fromEnum Debug          = 15
    fromEnum Topic          = 16
    fromEnum Randoms        = 17
    fromEnum ReplaceWords   = 18
    fromEnum Timer          = 19
    fromEnum Delay          = 20
    fromEnum Greetings      = 21
    fromEnum Actions        = 22
    fromEnum UnknownParam   = 23
    fromEnum _              = 23
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

type ChanNicks = Map.Map String [String]
type Fstate = (MVar Bot, MVar (), MVar [ThreadId], MVar ChanNicks)

main :: IO ()
main = do
    bracket start stop loop
  where
    loop :: Fstate -> IO ()
    loop st = do catch (evalStateT run st)
                   (\e -> do let err = show (e :: SomeException)
                             evalStateT (hPutStrLnLock stderr ("Exception in main: " ++ err)) st
                             return ())

start :: IO Fstate
start = do
    args <- cmdLine
    let servn    = args !! 0
    let port     = args !! 1
    let hints    = defaultHints {addrFlags = [AI_NUMERICSERV]}
    serv <- getAddrInfo (Just hints) (Just servn) (Just port)
    let nick'    = cleanStringWhite isAscii (args !! 2)
    let owner'   = args !! 3
    let topic'   = args !! 5
    let fdir     = args !! 6 :: FilePath
    let dfile    = args !! 7
    let wndir    = args !! 8 :: FilePath
    let gfdir    = args !! 9 :: FilePath
    let passwd   = args !! 10
    let socks5   = args !! 11
    let s5hostn  = takeWhile (\x -> x /= ':') socks5
    let s5port   = fTail [] $ dropWhile (\x -> x /= ':') socks5
    let channels = words $ args !! 4
    s <- socket AF_INET Stream defaultProtocol
    _ <- setSocketOption s KeepAlive 0
    _ <- if null socks5 then connect s (addrAddress $ head serv)
         else do
           s5serv <- getAddrInfo (Just hints) (Just s5hostn) (Just s5port)
           socksConnectAddr s (addrAddress $ head s5serv) (addrAddress $ head serv)
    sh <- socketToHandle s ReadWriteMode
    hSetBuffering sh NoBuffering
    (f, p) <- initFugly fdir wndir gfdir topic'
    let b = if null p then
              Bot sh (Parameter nick' owner' fdir dfile False 10 400 20 7 0 True False False True False False topic' 50 False 0 2 40 5) f
              else Bot sh ((readParamsFromList p){nick=nick', owner=owner', fuglydir=fdir, dictfile=dfile}) f
    bot  <- newMVar b
    lock <- newMVar ()
    tc   <- newMVar []
    cn   <- newMVar Map.empty
    let fstate = (bot, lock, tc, cn)
    evalStateT (write sh True "NICK" nick') fstate
    evalStateT (write sh True "USER" (nick' ++ " 0 * :user")) fstate
    _   <- forkIO (do threadDelay 20000000
                      if not $ null passwd then
                         evalStateT (replyMsg b "nickserv" [] ("IDENTIFY " ++ passwd)) fstate
                         else return ()
                      evalStateT (joinChannel sh "JOIN" channels) fstate)
    pId <- forkIO $ pingLoop lock sh
    _   <- incT tc pId
    return fstate
  where
    pingLoop l sh = forever $ do
      lock <- takeMVar l
      hPutStrLn sh "PING :foo\r"
      putMVar l lock
      threadDelay 60000000

stop :: Fstate -> IO ()
stop (bot, lock, tc, _) = do
    Bot{sock=s, params=p@Parameter{fuglydir=fd, dictfile=df}, fugly=f} <- readMVar bot
    hClose s
    tc' <- readMVar tc
    _   <- mapM killThread tc'
    stopFugly lock fd f df (paramsToList p)

run :: StateT Fstate IO b
run = do
    st <- get
    Bot{sock=s, params=p@Parameter{debug=d}} <- lift $ readMVar $ getBot st
    _   <- return p
    tId <- lift $ forkIO $ timerLoop st >> return ()
    _   <- lift $ incT (getTCount st) tId
    forever $ lift $ getl st s d
    where
      getl st s d = if d then do
        hGetLine s >>= (\l -> do evalStateT (hPutStrLnLock stdout ("> debug: IRC msg: " ++ l)) st >> return l)
          >>= (\ll -> listenIRC st s ll)
                    else do
                      hGetLine s >>= (\ll -> listenIRC st s ll)
      listenIRC st s l = do
        let b  = getBot st
        bot@Bot{params=p@Parameter{nick=bn, owner=o, debug=d, greetings=g}} <- readMVar b
        let cn = getChanNicks st
        r   <- Random.getStdRandom (Random.randomR (0, 99)) :: IO Int
        cn' <- readMVar cn
        let rr  = mod (r + length l + length (Map.toList cn') + 50) 100
        let ll  = words l
        let lll = take 2 $ drop 1 ll
        let lm  = unwords (drop 1 ll)
        if "PING :" `isPrefixOf` l then do
          evalStateT (write s d "PONG" (':' : drop 6 l)) st >> return ()
          else if "433 " `isPrefixOf` lm then do
            evalStateT (write s d "NICK" (bn ++ "_")) st >> swapMVar b bot{params=p{nick=(bn ++ "_")}}
              >> return ("Nickname is already in use.") >>= (\x -> evalStateT (replyMsg bot o [] x) st)
            else if "353 " `isPrefixOf` lm then let cnicks = words $ drop 1 $ dropWhile (\x -> x /= ':') $ drop 1 l
                                                    chan   = takeWhile (\x -> x /= ' ') $ dropWhile (\x -> x /= '#') l in do
              if null cnicks then swapMVar cn (Map.delete chan cn') >> return ()
                else swapMVar cn (Map.insert chan cnicks cn') >> return ()
              else if (length ll > 2) && (fHead [] lll) == "NICK" && getNick ll == bn then do
                (do nb <- evalStateT (changeNick lll) st ; swapMVar b nb) >> return ()
                else if (length ll > 2) && (fHead [] lll) == "JOIN" && rr < g then do
                  evalStateT (greeting $ words l) st >> return ()
                   else do
                     (catch (evalStateT (processLine rr $ words l) st >> return ())
                      (\e -> do let err = show (e :: SomeException)
                                evalStateT (hPutStrLnLock stderr ("Exception in processLine: " ++ err)) st
                                putMVar b bot
                                return ()))

getBot :: Fstate -> MVar Bot
getBot (b, _, _, _) = b

getLock :: Fstate -> MVar ()
getLock (_, l, _, _) = l

getTCount :: Fstate -> MVar [ThreadId]
getTCount (_, _, tc, _) = tc

getChanNicks :: Fstate -> MVar (Map.Map String [String])
getChanNicks (_, _, _, cn) = cn

incT :: MVar [ThreadId] -> ThreadId -> IO Int
incT c tId = do { v <- takeMVar c ; putMVar c (nub $ tId : v) ; return $ length v }

decT :: MVar [ThreadId] -> ThreadId -> IO ()
decT c tId = do { v <- takeMVar c ; putMVar c (delete tId v) }

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
    let dfilePos     = (maximum' $ elemIndices "-dictfile" args) + 1
    let dfile        = if l > dfilePos then args !! dfilePos else "default"
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
    return (server : port : nick' : owner' : channel : topic' : fuglydir' : dfile : wndir : gfdir : passwd : socks5 : [])
  where
    maximum' [] = 1000
    maximum' a  = maximum a

getLoad :: IO [String]
getLoad = do
    h <- openFile "/proc/loadavg" ReadMode
    hSetBuffering h LineBuffering
    l <- hGetLine h
    hClose h
    return $ words l

changeNick :: [String] -> StateT Fstate IO Bot
changeNick [] = do
    b   <- gets getBot
    bot <- lift $ readMVar b
    return bot
changeNick line = do
    st <- get
    bot@Bot{params=p@Parameter{nick=n}} <- lift $ readMVar $ getBot st
    _ <- return p
    lift $ testNick st bot n line
  where
    testNick :: Fstate -> Bot -> String -> [String] -> IO Bot
    testNick _ bot [] _ = return bot
    testNick _ bot _ [] = return bot
    testNick st' bot@Bot{params=p@Parameter{owner=o}} old line'
        | (x == "NICK") = return ("Nick change successful.") >>= (\x' -> evalStateT (replyMsg bot o [] x') st')
                          >> return bot{params=p{nick=drop 1 y}}
        | otherwise     = return ("Nick change failed!") >>= (\x' -> evalStateT (replyMsg bot o [] x') st')
                          >> return bot{params=p{nick=old}}
      where
        x = fHead [] line'
        y = fLast [] line'
    testNick _ bot _ _ = return bot

joinChannel :: Handle -> String -> [String] -> StateT Fstate IO ()
joinChannel _ _  []    = return () :: StateT Fstate IO ()
joinChannel h [] b     = joinChannel h "join" b
joinChannel h a (x:xs) = do
    if a == "JOIN" || a == "PART" then do
      write h False a x
      joinChannel h a xs
        else return ()

readParamsFromList :: [String] -> Parameter
readParamsFromList a = Parameter{nick="", owner="", fuglydir="", dictfile="", usercmd=read (a!!0),
                                 rejoinkick=read (a!!1), maxchanmsg=read (a!!2), stries=read (a!!3),
                                 slength=read (a!!4), plength=read (a!!5), learning=read (a!!6),
                                 strictlearn=read (a!!7), stricttopic=read (a!!8), autoname=read (a!!9),
                                 allowpm=read (a!!10), debug=read (a!!11), Main.topic=(a!!12),
                                 randoms=read (a!!13), rwords=read (a!!14), timer=read (a!!15),
                                 delay=read (a!!16), greetings=read (a!!17), actions=read (a!!18)}

paramsToList :: Parameter -> [String]
paramsToList (Parameter _ _ _ _ uc rk mcm st sl pl l stl stt an apm d t r rw ti dl g a) =
  [show uc, show rk, show mcm, show st, show sl, show pl, show l, show stl,
   show stt, show an, show apm, show d, t, show r, show rw, show ti, show dl,
   show g, show a]
paramsToList _ = []

changeParam :: Bot -> String -> String -> String -> String -> StateT Fstate IO Bot
changeParam bot@Bot{sock=s, params=p@Parameter{nick=botnick, owner=owner', fuglydir=fdir, dictfile=dfile, debug=debug'},
                    fugly=f@Fugly{dict=dict', defs=defs', ban=ban', FuglyLib.match=match'}} chan nick' param value = do
    lock <- gets getLock
    case readParam param of
      Nick           -> (write s debug' "NICK" $ cleanStringWhite isAscii value)     >> return bot
      Owner          -> replyMsg' value                 "Owner"               >>= (\x -> return bot{params=p{owner=x}})
      Topic          -> replyMsg' value                 "Topic"               >>= (\x -> return bot{params=p{Main.topic=x}})
      UserCommands   -> replyMsg' (readBool value)      "User commands"       >>= (\x -> return bot{params=p{usercmd=x}})
      RejoinKick     -> replyMsg' (readInt 1 4096 value) "Rejoin kick time"   >>= (\x -> return bot{params=p{rejoinkick=x}})
      MaxChanMsg     -> replyMsg' (readInt 9 450 value) "Max channel message" >>= (\x -> return bot{params=p{maxchanmsg=x}})
      SentenceTries  -> replyMsg' (readInt 1 4096 value) "Sentence tries"     >>= (\x -> return bot{params=p{stries=x}})
      SentenceLength -> replyMsg' (readInt 2 256 value) "Sentence length"     >>= (\x -> return bot{params=p{slength=x}})
      ParseLength    -> replyMsg' (readInt 0 256 value) "Parse length"        >>= (\x -> return bot{params=p{plength=x}})
      Learning       -> replyMsg' (readBool value)     "Learning"             >>= (\x -> return bot{params=p{learning=x}})
      StrictLearn    -> replyMsg' (readBool value)     "Strict learn"         >>= (\x -> return bot{params=p{strictlearn=x}})
      StrictTopic    -> replyMsg' (readBool value)     "Strict topic"         >>= (\x -> return bot{params=p{stricttopic=x}})
      Autoname       -> replyMsg' (readBool value)     "Autoname"             >>= (\x -> return bot{params=p{autoname=x}})
      AllowPM        -> replyMsg' (readBool value)     "Allow PM"             >>= (\x -> return bot{params=p{allowpm=x}})
      Debug          -> replyMsg' (readBool value)     "Debug"                >>= (\x -> return bot{params=p{debug=x}})
      DictFile       -> do (d, de, b, m, pl) <- lift $ catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then
                                                                          return (Map.empty, [], [], [], (paramsToList p))
                                                                        else
                                                                          return (dict', defs', ban', match', (paramsToList p)))
                                               (loadDict fdir value) (\_ -> return (Map.empty, [], [], [], (paramsToList p)))
                           _ <- lift $ saveDict lock f fdir dfile (paramsToList p)
                           let pp = (readParamsFromList pl){nick=botnick, owner=owner', fuglydir=fdir}
                           replyMsg' value "Dict file" >>= (\x -> return bot{params=pp{dictfile=x}, fugly=f{dict=d, defs=de, ban=b, FuglyLib.match=m}})
      Randoms        -> replyMsg' (readInt 0 100 value) "Randoms"             >>= (\x -> return bot{params=p{randoms=x}})
      ReplaceWords   -> replyMsg' (readBool value)      "Replace words"       >>= (\x -> return bot{params=p{rwords=x}})
      Timer          -> replyMsg' (readInt 0 36000 value)  "Timer"            >>= (\x -> return bot{params=p{timer=x}})
      Delay          -> replyMsg' (readInt 0 120 value)    "Delay"            >>= (\x -> return bot{params=p{delay=x}})
      Greetings      -> replyMsg' (readInt 0 100 value)   "Greetings"         >>= (\x -> return bot{params=p{greetings=x}})
      Actions        -> replyMsg' (readInt 0 100 value)   "Actions"           >>= (\x -> return bot{params=p{actions=x}})
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

getChannelNicks :: Handle -> String -> StateT Fstate IO ()
getChannelNicks h [] = write h False "NAMES" []
getChannelNicks h c  = write h False "NAMES" c

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

rejoinChannel :: Handle -> String -> Int -> StateT Fstate IO ()
rejoinChannel _ []   _  = return () :: StateT Fstate IO ()
rejoinChannel h chan rk = do
    if rk == 0 then return () else rejoin' rk chan h >> return ()
  where
    rejoin' rk' chan' h' = do
      st <- get
      lift $ forkIO (threadDelay (rk' * 1000000) >>
                     evalStateT (hPutStrLnLock h' ("JOIN " ++ chan' ++ "\r")) st)

timerLoop :: Fstate -> IO ThreadId
timerLoop st = do
    let tc = getTCount st
    tId <- myThreadId
    _   <- incT tc tId
    forever $ do
      r <- Random.getStdRandom (Random.randomR (0, 999))
      let b = getBot st
      Bot{params=p@Parameter{timer=t}} <- readMVar b
      _ <- return p
      let timer' = if (t < 5) then 30000000 else t * 1000000 + (t * r * 500)
      threadDelay timer'
      if t > 4 then do
        bot@Bot{sock=s, params=pp@Parameter{nick=botnick, Main.topic=topic', debug=d, actions=acts},
                fugly=f@Fugly{defs=defs'}} <- readMVar b
        _ <- return (pp, f)
        let norm     = [de | (type', de) <- defs', type' == Normal]
        let len_norm = length norm
        let cn       = getChanNicks st
        cn' <- readMVar cn
        let chans = concat [[c] | (c, _) <- Map.toList cn']
        let chan  = chans!!mod (r + 55) (length chans)
        evalStateT (getChannelNicks s chan) st
        threadDelay 1000000
        let nicks = Map.lookup chan cn'
        load <- getLoad
        if isJust nicks && (read $ fHead [] load :: Float) < 1.5 then do
          let n   = delete botnick $ fromJust nicks
          let m'  = cleanStringBlack (\x -> x == '@' || x == '&' || x == '~' || x == '+') $ n!!mod (r + 23) (length n)
          let n'  = cleanStringBlack (\x -> x == '@' || x == '&' || x == '~' || x == '+') $ n!!mod r (length n)
          let msg = case mod r $ 12 + len_norm of
                0 -> "It's quiet in here."
                1 -> "Does anybody here like " ++ topic' ++ "?"
                2 -> "Well, " ++ topic' ++ " is interesting, don't you think?"
                3 -> "I've heard that " ++ m' ++ " likes " ++ topic' ++ "."
                4 -> "I suspect that " ++ m' ++ " might know something about that."
                5 -> "It's all the same to me."
                6 -> "I wonder what " ++ m' ++ " thinks about that?"
                7 -> "It's getting late..."
                8 -> "Hmm..."
                9 -> "Yes indeed."
                _  -> if len_norm == 0 then "Hmm..." else fixAction m' topic' $ norm!!mod r len_norm
          action <- ircAction False n' m' topic' defs'
          let who = if mod (r + 11) 3 == 0 then n' else chan in if r < acts * 10 then
            forkIO (evalStateT (write s d "PRIVMSG" (chan ++ " :\SOHACTION " ++ action ++ "\SOH")) st)
            else if r < 850 then
              sentenceReply st bot r load chan who $ words msg
                 else
                   forkIO $ replyMsgT st bot chan who msg
          else forkIO $ return ()
        else forkIO $ return ()

ircAction :: Bool -> String -> String -> String -> [Default] -> IO String
ircAction _     []    _     _      _     = return "panics."
ircAction greet nick1 nick2 topic' defs' = do
    r <- Random.getStdRandom (Random.randomR (0, 999)) :: IO Int
    let action      = [de | (type', de) <- defs', type' == Action]
    let len_action  = length action
    let gaction     = [de | (type', de) <- defs', type' == GreetAction]
    let len_gaction = length gaction
    let altnick = if null nick2 then "" else " and " ++ nick2
    return (if greet then case mod r $ 6 + len_gaction of
                0 -> "waves to " ++ nick1 ++ "."
                1 -> "greets " ++ nick1 ++ "."
                2 -> "says hello to " ++ nick1 ++ "."
                3 -> "welcomes " ++ nick1 ++ " to the channel."
                _ -> if len_gaction == 0 then "waves." else fixAction nick1 topic' $ gaction!!mod r len_gaction
            else case mod r $ 7 + len_action of
                0 -> "is bored."
                1 -> "looks at " ++ nick1 ++ altnick ++ "."
                2 -> "waves to " ++ nick1 ++ altnick ++ "."
                3 -> "ponders."
                4 -> "thinks deeply."
                _  -> if len_action == 0 then "yawns." else fixAction nick1 topic' $ action!!mod r len_action)

fixAction :: String -> String -> String -> String
fixAction _  _ [] = []
fixAction [] t m  = dePlenk $ unwords $ replace "#topic" t $ replace "#nick" "somebody" $ words m
fixAction n [] m  = dePlenk $ unwords $ replace "#topic" "stuff" $ replace "#nick" n $ words m
fixAction n  t m  = dePlenk $ unwords $ replace "#topic" t $ replace "#nick" n $ words m

greeting :: [String] -> StateT Fstate IO ()
greeting []   = return ()
greeting line = do
    b <- gets getBot
    bot@Bot{sock=s, params=p@Parameter{nick=n, debug=d, Main.topic=top}, fugly=f@Fugly{defs=defs'}} <- lift $ takeMVar b
    _ <- return (p, f)
    r <- lift $ Random.getStdRandom (Random.randomR (0, 999)) :: StateT Fstate IO Int
    lift $ threadDelay (600000 * (mod r 8) + 2000000)
    action <- lift $ ircAction True who [] top defs'
    let enter     = [de | (t, de) <- defs', t == Enter]
    let len_enter = length enter
    let greet     = [de | (t, de) <- defs', t == Greeting]
    let len_greet = length greet
    if who == n then
      case mod r $ 6 + len_enter of
        0 -> replyMsg bot chan [] "Hello world."
        1 -> replyMsg bot chan [] "Good morning!"
        2 -> replyMsg bot chan [] "Hello friends."
        3 -> replyMsg bot chan [] "\SOHACTION wonders if this is the right channel.\SOH"
        _ -> replyMsg bot chan [] $ if len_enter == 0 then "Hello world." else
                                      fixAction [] top $ enter!!mod r len_enter
      else if r < 600 then case mod r $ 6 + len_greet of
          0 -> replyMsg bot chan [] ("Hello " ++ who ++ ".")
          1 -> replyMsg bot chan [] ("Welcome to the channel, " ++ who ++ ".")
          2 -> replyMsg bot chan who "Greetings."
          3 -> replyMsg bot chan who "Hello."
          _ -> replyMsg bot chan (if elem "#nick" line then [] else who) $
               if len_greet == 0 then "Hello." else fixAction who top $ greet!!mod r len_greet
           else
             write s d "PRIVMSG" (chan ++ " :\SOHACTION " ++ action ++ "\SOH")
    lift $ putMVar b bot >> return ()
  where
    who  = getNick line
    chan = dropWhile (\x -> x == ':') $ getChannel line

processLine :: Int -> [String] -> StateT Fstate IO ()
processLine _ []   = return ()
processLine r line = do
    b <- gets getBot
    bot@Bot{sock=s, params=p@Parameter{nick=n, rejoinkick=rk}} <- lift $ takeMVar b
    _ <- return p
    let bk = beenKicked n line
    if (not $ null bk) then do (rejoinChannel s bk rk >> lift (putMVar b bot) >> return ())
      else if null msg then lift $ putMVar b bot >> return ()
         else if chan == n then do nb <- prvcmd bot ; lift $ putMVar b nb >> return ()
              else if spokenTo n msg then if null (tail msg) then lift $ putMVar b bot >> return ()
                                          else if (head $ head $ tail msg) == '!'
                                               then do nb <- execCmd bot chan who (tail msg)
                                                       lift $ putMVar b nb >> return ()
                                               else do nb <- reply bot r False chan who (tail msg)
                                                       lift $ putMVar b nb >> return ()
                   else do nb <- reply bot r True chan who msg ; lift $ putMVar b nb >> return ()
  where
    msg  = getMsg line
    who  = getNick line
    chan = getChannel line
    prvcmd bot = if (length $ head msg) > 0 then
                   if (head $ head msg) == '!' then execCmd bot who who msg
                   else reply bot r False [] who msg
                 else reply bot r False [] who msg

reply :: Bot -> Int -> Bool -> String -> String -> [String] -> StateT Fstate IO Bot
reply bot _ _ _ _ [] = return bot
reply bot@Bot{sock=h, params=p@Parameter{nick=bn, owner=o, learning=l, plength=plen, strictlearn=sl,
                                         autoname=an, allowpm=apm, debug=d, Main.topic=top, actions=acts},
              fugly=f@Fugly{defs=defs', pgf=pgf', FuglyLib.match=match'}} r chanmsg chan nick' msg = do
    st@(_, lock, _, _) <- get :: StateT Fstate IO Fstate
    _   <- return p
    let mmsg = if null $ head msg then msg
                 else case fLast ' ' $ head msg of
                   ':' -> tail msg
                   ',' -> tail msg
                   _   -> msg
    load <- lift getLoad
    let fload    = read $ fHead [] load :: Float
    let rr       = mod (r + (floor $ fload * 5.1)) 100
    let fmsg     = map cleanString mmsg
    let parse    = if sl then
                     if fload < 1.5 then gfParseBool pgf' plen $ unwords fmsg
                     else False
                   else True
    let matchon  = map toLower (" " ++ intercalate " | " (bn : match') ++ " ")
    let isaction = head msg == "\SOHACTION"
    action <- lift $ ircAction False nick' [] top defs'
    lift $ (if null chan then
                   if apm && not isaction then
                     sentenceReply st bot rr load nick' [] fmsg >> return ()
                   else return ()
                 else if chanmsg then
                        if map toLower (unwords msg) =~ matchon then
                          if isaction && rr < acts  || rr * 5 + 15 < acts then
                             evalStateT (write h d "PRIVMSG" (chan ++ " :\SOHACTION " ++ action ++ "\SOH")) st
                            else sentenceReply st bot rr load chan chan fmsg >> return ()
                        else return ()
                      else sentenceReply st bot rr load chan nick' fmsg >> return ())
    if ((nick' == o && null chan) || parse) && l && not isaction then do
      nd <- lift $ insertWords lock f an top fmsg
      hPutStrLnLock stdout ("> parse: " ++ unwords fmsg)
      return bot{fugly=f{dict=nd}} else
      return bot
reply bot _ _ _ _ _ = return bot

sentenceReply :: Fstate -> Bot -> Int -> [String] -> String -> String -> [String] -> IO ThreadId
sentenceReply _ _ _ _ _ _ [] = forkIO $ return ()
sentenceReply st@(_, lock, tc, _) bot@Bot{sock=h,
                params=p@Parameter{stries=str, slength=slen, plength=plen,
                                   Main.topic=top, randoms=rand, rwords=rw,
                                   stricttopic=stopic, debug=d, delay=dl},
                fugly=fugly'@Fugly{defs=defs'}} r load chan nick' m = forkIO (do
    tId  <- myThreadId
    tc'  <- incT tc tId
    _    <- if d then evalStateT (hPutStrLnLock stdout ("> debug: thread count: " ++ show tc')) st else return ()
    _    <- return p
    let fload = read $ fHead [] load :: Float
    let r'    = 1 + mod r 7
    let rr    = mod (r + floor (fload * 2.3)) 5
    let n'    = if nick' == chan then "somebody" else nick'
    let nn    = if nick' == chan || null nick' then [] else nick' ++ ": "
    action <- ircAction False n' [] top defs'
    if tc' < 10 && fload < 2.3 then do
      let d1 = dl * 1000000
      threadDelay (d1 * (1 + if rr - 2 > 0 then rr - 2 else 0 * 3 + if r' - 3 > 0 then r' - 3 else 0 * 9))
      let num = if r' - 4 < 1 || str < 4 || length m < 7 then 1 else r' - 4
      x <- sentenceA lock fugly' d rw stopic rand str slen top m
      y <- sentenceB lock fugly' d rw stopic rand str slen plen top num m
      let ww = unwords $ if null x then y else x
      evalStateT (do if null ww then return ()
                       else if null nick' || nick' == chan || rr == 0 then
                               write h d "PRIVMSG" $ chan ++ " :" ++ ww
                            else write h d "PRIVMSG" $ chan ++ " :" ++ nick' ++ ": " ++ ww) st
      else replyMsgT st bot chan [] $ case mod r 13 of
        1 -> nn ++ "I don't know if you're making any sense."
        2 -> nn ++ "I can't chat now, sorry."
        3 -> nn ++ "Let's discuss this later."
        4 -> nn ++ "I'll get back to you on that."
        5 -> "\SOHACTION " ++ action ++ "\SOH"
        6 -> "\SOHACTION looks confused.\SOH"
        7 -> "\SOHACTION is AFK for a bit.\SOH"
        8 -> "\SOHACTION feels like ignoring " ++ n' ++ ".\SOH"
        9 -> "All this chat is making me thirsty."
        _ -> []
    decT tc tId)
sentenceReply _ _ _ _ _ _ _ = forkIO $ return ()

execCmd :: Bot -> String -> String -> [String] -> StateT Fstate IO Bot
execCmd b _ _ [] = lift $ return b
execCmd b _ [] _ = lift $ return b
execCmd b [] _ _ = lift $ return b
execCmd b chan nick' (x:xs) = do
    st <- get
    lift $ catch (execCmd' b st) (\e -> do let err = show (e :: SomeException)
                                           evalStateT (hPutStrLnLock stderr ("Exception in execCmd: " ++ err)) st
                                           return b)
  where
    execCmd' :: Bot -> Fstate -> IO Bot
    execCmd' bot@Bot{sock=s, params=p@(Parameter botnick owner' fdir
                                       dfile usercmd' rkick maxcmsg
                                       stries' slen plen learn slearn stopic
                                       autoname' allowpm' debug' topic' randoms'
                                       rwords' timer' delay' greets actions'),
                     fugly=f@(Fugly dict' defs' pgf' wne' aspell' ban' match')} st
      | usercmd' == False && nick' /= owner' = return bot
      | x == "!quit" = if nick' == owner' then case length xs of
          0 -> do evalStateT (write s debug' "QUIT" ":Bye") st >> return bot
          _ -> do evalStateT (write s debug' "QUIT" (":" ++ unwords xs)) st >> return bot
                       else return bot
      | x == "!save" = if nick' == owner' then
                         catch (saveDict (getLock st) f fdir dfile (paramsToList p))
                         (\e -> do let err = show (e :: SomeException)
                                   evalStateT (hPutStrLnLock stderr ("Exception in saveDict: " ++ err)) st
                                   return ())
                         >> replyMsgT st bot chan nick' "Saved dict file!" >> return bot
                       else return bot
      | x == "!load" = if nick' == owner' then do
           (nd, nde, nb, nm, np) <- catch (loadDict fdir dfile) (\e -> do let err = show (e :: SomeException)
                                                                          evalStateT (hPutStrLnLock stderr ("Exception in loadDict: " ++ err)) st
                                                                          return (dict', defs', ban', match', paramsToList p))
           replyMsgT st bot chan nick' "Loaded dict file!"
           return bot{params=(readParamsFromList np){nick=botnick, owner=owner', fuglydir=fdir, dictfile=dfile},
                      fugly=(Fugly nd nde pgf' wne' aspell' nb nm)}
                       else return bot
      | x == "!join" = if nick' == owner' then evalStateT (joinChannel s "JOIN" xs) st >> return bot else return bot
      | x == "!part" = if nick' == owner' then evalStateT (joinChannel s "PART" xs) st >> return bot else return bot
      | x == "!nick" = if nick' == owner' then case length xs of
          1 -> evalStateT ((\x' -> write s debug' "NICK" $ cleanStringWhite isAscii x') (xs!!0)) st >> return bot
          _ -> replyMsgT st bot chan nick' "Usage: !nick <nick>" >> return bot
                       else return bot
      -- | x == "!readfile" = if nick' == owner' then case length xs of
      --     1 -> catch (insertFromFile (getLock st) bot (xs!!0)) (\e -> do let err = show (e :: SomeException)
      --                                                                    evalStateT (hPutStrLnLock stderr ("Exception in insertFromFile: " ++ err)) st
      --                                                                    return bot)
      --     _ -> replyMsgT st bot chan nick' "Usage: !readfile <file>" >> return bot
      --                      else return bot
      | x == "!internalize" = if nick' == owner' then
          if length xs > 1 then do replyMsgT st bot chan nick' ("Internalizing...")
                                   internalize st bot (read (xs!!0)) $ unwords $ tail xs
          else
            replyMsgT st bot chan nick' "Usage: !internalize <tries> <msg>" >> return bot
                              else return bot
      | x == "!showparams" = if nick' == owner' then case length xs of
          0 -> replyMsgT st bot chan nick' ("nick: " ++ botnick ++ "  owner: " ++ owner' ++
                   "  usercommands: " ++ show usercmd' ++ "  rejoinkick: "
                   ++ show rkick ++ "  maxchanmsg: " ++ show maxcmsg
                   ++ "  sentencetries: " ++ show stries' ++ "  sentencelength: "
                   ++ show slen ++ "  parselength: " ++ show plen ++ "  dictfile: " ++ dfile
                   ++ "  learning: " ++ show learn ++ "  strictlearn: " ++ show slearn
                   ++ "  stricttopic: " ++ show stopic ++ "  debug: " ++ show debug'
                   ++ "  autoname: " ++ show autoname' ++ "  allowpm: " ++ show allowpm'
                   ++ "  topic: " ++ topic' ++ "  randoms: " ++ show randoms'
                   ++ "  replacewords: " ++ show rwords'
                   ++ "  timer: " ++ show timer' ++ "  delay: " ++ show delay'
                   ++ "  greetings: " ++ show greets ++ "  actions: " ++ show actions') >> return bot
          _ -> replyMsgT st bot chan nick' "Usage: !showparams" >> return bot
                             else return bot
      | x == "!setparam" = if nick' == owner' then case length xs of
          2 -> evalStateT (changeParam bot chan nick' (xs!!0) (xs!!1)) st
          _ -> replyMsgT st bot chan nick' "Usage: !setparam <parameter> <value>" >> return bot
                           else return bot
      | x == "!word" || x == "!name" || x == "!acronym" = case length xs of
          1 -> replyMsgT st bot chan nick' (listWordFull dict' (xs!!0)) >> return bot
          _ -> replyMsgT st bot chan nick' ("Usage: " ++ x ++ " <" ++ (tail x) ++ ">") >> return bot
      | x == "!wordlist" || x == "!namelist" || x == "!acronymlist" =
          let num = if read (xs!!0) > (100 :: Int) then 100 :: Int else read (xs!!0) in
          case length xs of
            2 -> replyMsgT st bot chan nick' (unwords $ listWordsCountSort dict' num (x =~ "word|name|acronym") (xs!!1)) >>
                   replyMsgT st bot chan nick' ("Total " ++ (x =~ "word|name|acronym") ++ " count with topic " ++ (xs!!1) ++ ": " ++
                                            (show $ numWords dict' (x =~ "word|name|acronym") (xs!!1))) >> return bot
            1 -> replyMsgT st bot chan nick' (unwords $ listWordsCountSort dict' num (x =~ "word|name|acronym") []) >>
                   replyMsgT st bot chan nick' ("Total " ++ (x =~ "word|name|acronym") ++ " count: " ++
                                            (show $ numWords dict' (x =~ "word|name|acronym") [])) >> return bot
            _ -> replyMsgT st bot chan nick' ("Usage: " ++ x ++ " <number> [topic]") >> return bot
      | x == "!insertword" = if nick' == owner' then case length xs of
          2 -> do ww <- insertWordRaw (getLock st) f True (xs!!0) [] [] topic' (xs!!1)
                  if isJust $ Map.lookup (xs!!0) dict' then
                    replyMsgT st bot chan nick' ("Word " ++ (xs!!0) ++ " already in dict.") >> return bot
                    else
                    replyMsgT st bot chan nick' ("Inserted word " ++ (xs!!0) ++ ".") >> return bot{fugly=f{dict=ww}}
          1 -> do ww <- insertWordRaw (getLock st) f True (xs!!0) [] [] topic' []
                  if isJust $ Map.lookup (xs!!0) dict' then
                    replyMsgT st bot chan nick' ("Word " ++ (xs!!0) ++ " already in dict.") >> return bot
                    else
                    replyMsgT st bot chan nick' ("Inserted word " ++ (xs!!0) ++ ".") >> return bot{fugly=f{dict=ww}}
          _ -> replyMsgT st bot chan nick' "Usage: !insertword <word> [pos]" >> return bot
                             else return bot
      | x == "!insertname" = if nick' == owner' then case length xs of
          1 -> do ww <- insertNameRaw (getLock st) f True (xs!!0) [] [] topic'
                  if isJust $ Map.lookup (xs!!0) dict' then
                    replyMsgT st bot chan nick' ("Name " ++ (xs!!0) ++ " already in dict.") >> return bot
                    else
                    replyMsgT st bot chan nick' ("Inserted name " ++ (xs!!0) ++ ".") >> return bot{fugly=f{dict=ww}}
          _ -> replyMsgT st bot chan nick' "Usage: !insertname <name>" >> return bot
                             else return bot
      | x == "!insertacronym" = if nick' == owner' then
            if length xs > 1 then do
              ww <- insertAcroRaw (getLock st) f (xs!!0) [] [] topic' (unwords $ tail xs)
              if isJust $ Map.lookup (xs!!0) dict' then
                replyMsgT st bot chan nick' ("Acronym " ++ (xs!!0) ++ " already in dict.") >> return bot
                else
                replyMsgT st bot chan nick' ("Inserted acronym " ++ (xs!!0) ++ ".") >> return bot{fugly=f{dict=ww}}
              else
              replyMsgT st bot chan nick' "Usage: !insertacronym <acronym> <definition>" >> return bot
                                else return bot
      | x == "!insertdefault" = if nick' == owner' then
                                  if length xs > 1 then replyMsgT st bot chan nick' ("Inserted default " ++
                                                          show ((read $ head xs) :: DType) ++ " " ++ (unwords $ tail xs) ++ ".") >>
                                                        return bot{fugly=f{defs=defs' ++ [(read $ head xs, unwords $ tail xs)]}}
                                  else
                                    replyMsgT st bot chan nick' "Usage: !insertdefault <Normal|Action|GreetAction|Greeting|Enter> <default>" >> return bot
                                else return bot
      | x == "!dropdefault" = if nick' == owner' then
                                if length xs > 1 then replyMsgT st bot chan nick' ("Dropped default " ++
                                                        show ((read $ head xs) :: DType) ++ " " ++ (unwords $ tail xs) ++ ".") >>
                                                      return bot{fugly=f{defs=filter (\(t, d) -> not (t == read (head xs) && d == unwords (tail xs))) defs'}}
                                else
                                  replyMsgT st bot chan nick' "Usage: !dropdefault <Normal|Action|GreetAction|Greeting|Enter> <default>" >> return bot
                              else return bot
      | x == "!dropword" = if nick' == owner' then case length xs of
          1 -> if isJust $ Map.lookup (xs!!0) dict' then
                 replyMsgT st bot chan nick' ("Dropped word " ++ (xs!!0) ++ ".") >>
                   return bot{fugly=f{dict=dropWord dict' (xs!!0)}}
               else {-- Drop the word anyway because it might be a before or after word. --}
                 replyMsgT st bot chan nick' ("Word " ++ (xs!!0) ++ " not in dict.") >>
                   return bot{fugly=f{dict=dropWord dict' (xs!!0)}}
          _ -> replyMsgT st bot chan nick' "Usage: !dropword <word>" >> return bot
                           else return bot
      | x == "!dropafter" = if nick' == owner' then case length xs of
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
      | x == "!listtopics" = case length xs of
          0 -> replyMsgT st bot chan nick' ("topics: " ++ (unwords $ listTopics dict')) >> return bot
          _ -> replyMsgT st bot chan nick' ("Usage: !listtopics") >> return bot
      | x == "!droptopic" = if nick' == owner' then case length xs of
          1 -> if elem (xs!!0) $ listTopics dict' then
                 replyMsgT st bot chan nick' ("Dropped topic " ++ (xs!!0) ++ ".") >>
                   return bot{fugly=f{dict=dropTopic dict' (xs!!0)}}
               else
                 replyMsgT st bot chan nick' ("Topic " ++ (xs!!0) ++ " not in dict.") >> return bot
          _ -> replyMsgT st bot chan nick' "Usage: !droptopic <topic>" >> return bot
                            else return bot
      | x == "!droptopicwords" = if nick' == owner' then case length xs of
          1 -> if elem (xs!!0) $ listTopics dict' then let nd = dropTopicWords dict' (xs!!0) in
                 replyMsgT st bot chan nick' ("Dropped all words in topic " ++ (xs!!0) ++ ".") >>
                   return bot{fugly=f{dict=dropTopic nd (xs!!0)}}
               else
                 replyMsgT st bot chan nick' ("Topic " ++ (xs!!0) ++ " not in dict.") >> return bot
          _ -> replyMsgT st bot chan nick' "Usage: !droptopicwords <topic>" >> return bot
                            else return bot
      | x == "!banafter" = if nick' == owner' then case length xs of
          3 -> if (xs!!0) == "add" then let w = Map.lookup (xs!!1) dict' in
                 if isJust w then let nd1 = Map.insert (xs!!1) (addBanAfter (fromJust w) (xs!!2)) dict'
                                      nd2 = dropBefore nd1 (xs!!2) (xs!!1) in
                   if elem (xs!!2) $ wordGetBanAfter $ fromJust w then
                     replyMsgT st bot chan nick' ("Word " ++ (xs!!2) ++ " after word " ++ (xs!!1) ++ " already banned.") >>
                       return bot{fugly=f{dict=dropAfter nd2 (xs!!1) (xs!!2)}}
                     else {-- Drop the association anyway, but report errors. --}
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
      | x == "!ageword" = if nick' == owner' then case length xs of
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
      | x == "!banword" = if nick' == owner' then case length xs of
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
      | x == "!matchword" = if nick' == owner' then case length xs of
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
      | x == "!talk" = do
          if nick' == owner' then
            if length xs > 2 then do
              load <- getLoad
              let fload = read $ fHead [] load :: Float
              sentenceReply st bot (floor $ fload * 35) load (xs!!0) (xs!!1) (drop 2 xs) >> return bot
            else replyMsgT st bot chan nick' "Usage: !talk <channel> <nick> <msg>" >> return bot
            else return bot
      | x == "!raw" = if nick' == owner' then
          if length xs > 0 then evalStateT (write s debug' (xs!!0) (unwords $ tail xs)) st >> return bot
          else replyMsgT st bot chan nick' "Usage: !raw <msg>" >> return bot
                      else return bot
      | x == "!dict" = case length xs of
          2 -> (dictLookup (getLock st) f (xs!!0) (xs!!1)) >>= (\x' -> replyMsgT st bot chan nick' x') >> return bot
          1 -> (dictLookup (getLock st) f (xs!!0) []) >>= (\x' -> replyMsgT st bot chan nick' x') >> return bot
          _ -> replyMsgT st bot chan nick' "Usage: !dict <word> [part-of-speech]" >> return bot
      | x == "!closure" = case length xs of
          3 -> (wnClosure wne' (xs!!0) (xs!!1) (xs!!2)) >>= (\x' -> replyMsgT st bot chan nick' x') >> return bot
          2 -> (wnClosure wne' (xs!!0) (xs!!1) []) >>= (\x' -> replyMsgT st bot chan nick' x') >> return bot
          1 -> (wnClosure wne' (xs!!0) [] []) >>= (\x' -> replyMsgT st bot chan nick' x') >> return bot
          _ -> replyMsgT st bot chan nick' "Usage: !closure <word> [form] [part-of-speech]" >> return bot
      | x == "!meet" = case length xs of
          3 -> (wnMeet wne' (xs!!0) (xs!!1) (xs!!2)) >>= (\x' -> replyMsgT st bot chan nick' x') >> return bot
          2 -> (wnMeet wne' (xs!!0) (xs!!1) []) >>= (\x' -> replyMsgT st bot chan nick' x') >> return bot
          _ -> replyMsgT st bot chan nick' "Usage: !meet <word> <word> [part-of-speech]" >> return bot
      | x == "!parse" = if nick' == owner' then case length xs of
          0 -> replyMsgT st bot chan nick' "Usage: !parse <msg>" >> return bot
          _ -> (mapM (\x' -> replyMsgT st bot chan nick' x') $ take 3 (gfParseShow pgf' (unwords $ take 12 xs))) >> return bot
                        else return bot
      | x == "!related" = case length xs of
          3 -> (wnRelated wne' (xs!!0) (xs!!1) (xs!!2)) >>= (\x' -> replyMsgT st bot chan nick' x') >> return bot
          2 -> (wnRelated wne' (xs!!0) (xs!!1) []) >>= (\x' -> replyMsgT st bot chan nick' x') >> return bot
          1 -> (wnRelated wne' (xs!!0) [] []) >>= (\x' -> replyMsgT st bot chan nick' x') >> return bot
          _ -> replyMsgT st bot chan nick' "Usage: !related <word> [form] [part-of-speech]" >> return bot
      | x == "!forms"  = case length xs of
          0 -> replyMsgT st bot chan nick' (concat $ map (++ " ") $ map show allForm) >> return bot
          _ -> replyMsgT st bot chan nick' "Usage: !forms" >> return bot
      | x == "!parts"  = case length xs of
          0 -> replyMsgT st bot chan nick' (concat $ map (++ " ") $ map show allPOS) >> return bot
          _ -> replyMsgT st bot chan nick' "Usage: !parts" >> return bot
      | x == "!isname" = case length xs of
          1 -> do n <- asIsName (getLock st) aspell' (xs!!0)
                  replyMsgT st bot chan nick' (show n)
                  return bot
          _ -> replyMsgT st bot chan nick' "Usage: !isname <word>" >> return bot
      | x == "!isacronym" = case length xs of
          1 -> do n <- asIsAcronym (getLock st) aspell' (xs!!0)
                  replyMsgT st bot chan nick' (show n)
                  return bot
          _ -> replyMsgT st bot chan nick' "Usage: !isacronym <word>" >> return bot
      -- | x == "!commas" = if length xs > 0 then do
      --     m <- insertCommas wne' 0 $ return xs
      --     replyMsgT st bot chan nick' (unwords m) >> return bot
      --                    else replyMsgT st bot chan nick' "Usage: !commas <msg>" >> return bot
      -- | x == "!asreplace" = case length xs of
      --     0 -> replyMsgT st bot chan nick' "Usage: !asreplace <msg>" >> return bot
      --     _ -> do ww <- asReplaceWords f xs ; replyMsgT st bot chan nick' (unwords ww) >> return bot
      -- | x == "!wnreplace" = case length xs of
      --     0 -> replyMsgT st bot chan nick' "Usage: !wnreplace <msg>" >> return bot
      --     _ -> do ww <- wnReplaceWords f True randoms' xs ; replyMsgT st bot chan nick' (unwords ww) >> return bot
      -- | x == "!gfcats" = case length xs of
      --     0 -> return (unwords $ gfCategories pgf') >>= (\x' -> replyMsgT st bot chan nick' x') >> return bot
      --     _ -> replyMsgT st bot chan nick' "Usage: !gfcats" >> return bot
      -- | x == "!gflin" = case length xs of
      --     0 -> replyMsgT st bot chan nick' "Usage: !gflin <msg>" >> return bot
      --     _ -> replyMsgT st bot chan nick' (gfLin pgf' $ unwords xs) >> return bot
      -- | x == "!gfshowexpr" = case length xs of
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
          ++ "!dropword !banword !matchword !insertdefault !dropdefault !dropafter !banafter "
          ++ "!ageword !listtopics !droptopic !droptopicwords !internalize "
          ++ "!dict !closure !meet !parse !related !forms !parts !isname !isacronym "
          ++ "!setparam !showparams !nick !join !part !talk !raw !quit !readfile !load !save") >> return bot
                     else replyMsgT st bot chan nick'
          ("Commands: !word !wordlist !name !namelist !acronym !acronymlist !listtopics "
          ++ "!dict !closure !meet !related !forms !parts !isname !isacronym") >> return bot
    execCmd' bot _ = return bot

replyMsg :: Bot -> String -> String -> String -> StateT Fstate IO ()
replyMsg _                                                   _    _     [] = return ()
replyMsg bot@Bot{sock=s, params=p@Parameter{maxchanmsg=mcm, debug=d}} chan nick' msg
    | null nick' = if length msg > mcm then do
      _ <- return p
      write s d "PRIVMSG" (chan ++ " :" ++ (take mcm msg))
      lift $ threadDelay 2000000
      replyMsg bot chan [] (drop mcm msg) else
        write s d "PRIVMSG" (chan ++ " :" ++ msg)
    | chan == nick' = if length msg > mcm then do
      write s d "PRIVMSG" (nick' ++ " :" ++ (take mcm msg))
      lift $ threadDelay 2000000
      replyMsg bot chan nick' (drop mcm msg) else
        write s d "PRIVMSG" (nick' ++ " :" ++ msg)
    | otherwise = if length msg > mcm then do
      write s d "PRIVMSG" (chan ++ " :" ++ nick' ++ ": " ++ (take mcm msg))
      lift $ threadDelay 2000000
      replyMsg bot chan nick' (drop mcm msg) else
        write s d "PRIVMSG" (chan ++ " :" ++ nick' ++ ": " ++ msg)
replyMsg _ _ _ _ = return ()

replyMsgT :: Fstate -> Bot -> String -> String -> String -> IO ()
replyMsgT _  _   _    _     []  = return ()
replyMsgT st bot chan nick' msg = evalStateT (replyMsg bot chan nick' msg) st

-- insertFromFile :: (MVar ()) -> Bot -> FilePath -> IO Bot
-- insertFromFile _ b [] = return b
-- insertFromFile st bot@Bot{params=p@Parameter{autoname=a, Main.topic=t}, fugly=f} file = do
--     _    <- return p
--     ff   <- readFile file
--     fmsg <- asReplaceWords st f $ map cleanString $ words ff
--     n    <- insertWords st f a t fmsg
--     return bot{fugly=f{dict=n}}
-- insertFromFile _ b _ = return b

internalize :: Fstate -> Bot -> Int -> String -> IO Bot
internalize _  b 0 _   = return b
internalize _  b _ []  = return b
internalize st b n msg = internalize' st b n 0 msg
  where
    internalize' :: Fstate -> Bot -> Int -> Int -> String -> IO Bot
    internalize' _ bot _ _ [] = return bot
    internalize' st' bot@Bot{params=p@Parameter{autoname=aname, Main.topic=topic', stries=tries,
                                                slength=slen, debug=d, plength=plen, rwords=rw,
                                                stricttopic=stopic, randoms=rands}, fugly=f}
      num i imsg = do
      _   <- return p
      sen <- getSentence $ sentenceB' (getLock st') f d False rw stopic rands tries slen plen topic' $ words imsg
      nd  <- insertWords (getLock st') f aname topic' $ words sen
      r   <- Random.getStdRandom (Random.randomR (0, 2)) :: IO Int
      if i >= num then return bot
        else if r == 0 then evalStateT (hPutStrLnLock stdout ("> internalize: " ++ msg)) st >> internalize' st bot{fugly=f{dict=nd}} num (i + 1) msg
             else evalStateT (hPutStrLnLock stdout ("> internalize: " ++ sen)) st >> internalize' st bot{fugly=f{dict=nd}} num (i + 1) sen
    internalize' _ bot _ _ _ = return bot
    getSentence []     = return []
    getSentence (x:xs) = do
      ww <- x
      if null ww then getSentence xs
        else return ww

write :: Handle -> Bool -> [Char] -> [Char] -> StateT Fstate IO ()
write _     _ [] _  = return ()
write sock' d s []  = do
    hPutStrLnLock sock'  (s ++ "\r")
    if d then hPutStrLnLock stdout ("> debug: " ++ s) else return ()
write sock' d s msg = do
    hPutStrLnLock sock'  (s ++ " " ++ msg ++ "\r")
    if d then hPutStrLnLock stdout ("> debug: " ++ s ++ " " ++ msg) else return ()

hPutStrLnLock :: Handle -> String -> StateT Fstate IO ()
hPutStrLnLock s m = do
    st <- get :: StateT Fstate IO Fstate
    let l = getLock st
    lock <- lift $ takeMVar l
    lift (do hPutStrLn s m ; putMVar l lock)
