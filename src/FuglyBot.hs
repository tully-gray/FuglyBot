import           Control.Concurrent
import           Control.Exception              hiding (handle)
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Lazy
import           Data.Char
import           Data.List
import qualified Data.Map.Lazy                  as Map
import           Data.Maybe
import           FuglyLib                       hiding (hPutStrLnLock)
import           Network.Socket                 hiding (Debug)
import           Network.Socks5
import           NLP.WordNet.PrimTypes          (allForm, allPOS)
import           Prelude
import           SentenceA
import           System.Environment
import           System.IO
import           System.IO.Error
import qualified System.Random                  as Random
import           Text.Regex.Posix

data Bot = Bot {
    handle :: Handle,
    params :: Parameter,
    fugly  :: Fugly,
    lastm  :: [String]
    }

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
                 userCmd     :: Bool, -- allow users to issue commands
                 rejoinKick  :: Int,  -- rejoin channels after being kicked
                 maxChanMsg  :: Int,  -- maximum words in a single message
                 numThreads  :: Int,  -- maximum number of threads
                 nSetSize    :: Int,  -- neural network set size
                 sTries      :: Int,  -- sentence tries
                 sLength     :: Int,  -- sentence length
                 pLength     :: Int,  -- parse length
                 learning    :: Bool, -- allow learning
                 strictLearn :: Bool, -- strict learning using GF
                 strictTopic :: Bool, -- stay on topic or not
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

type ChanNicks = Map.Map String [String]
type Fstate = (MVar Bot, MVar (), MVar [ThreadId], MVar ChanNicks)

main :: IO ()
main = do
    bracket start stop loop
  where
    loop :: Fstate -> IO ()
    loop st = do catch (evalStateT run st)
                   (\e -> do let err = show (e :: SomeException)
                             evalStateT (hPutStrLnLock stderr
                               ("Exception in main: " ++ err)) st
                             return ())

start :: IO Fstate
start = do
    args <- cmdLine
    let servname = args !! 0
        port     = args !! 1
        hints    = defaultHints {addrFlags = [AI_NUMERICSERV]}
        nick'    = cleanStringWhite isAscii (args !! 2)
        owner'   = args !! 3
        topic'   = args !! 5
        fDir     = args !! 6 :: FilePath
        dFile    = args !! 7
        wnDir    = args !! 8 :: FilePath
        gfDir    = args !! 9 :: FilePath
        passwd   = args !! 10
        socks5   = args !! 11
        s5hostn  = takeWhile (\x -> x /= ':') socks5
        s5port   = fTail [] $ dropWhile (\x -> x /= ':') socks5
        channels = words $ args !! 4
    serv <- getAddrInfo (Just hints) (Just servname) (Just port)
    s <- socket AF_INET Stream defaultProtocol
    _ <- setSocketOption s KeepAlive 0
    _ <- if null socks5 then connect s (addrAddress $ head serv)
         else do
           s5serv <- getAddrInfo (Just hints) (Just s5hostn) (Just s5port)
           socksConnectAddr s (addrAddress $ head s5serv)
             (addrAddress $ head serv)
    sh <- socketToHandle s ReadWriteMode
    hSetBuffering sh NoBuffering
    (f, p) <- initFugly fDir wnDir gfDir topic'
    let b = if null p then
              Bot sh (Parameter nick' owner' fDir dFile False 10 400 4 50 20 7
                      0 True False False True False False topic' 50 False 0 2
                      40 5) f []
              else Bot sh ((readParamsFromList p){nick=nick', owner=owner',
                      fuglyDir=fDir, dictFile=dFile}) f []
    bot  <- newMVar b
    lock <- newMVar ()
    tc   <- newMVar []
    cn   <- newMVar Map.empty
    let fState = (bot, lock, tc, cn)
    evalStateT (write sh True "NICK" nick') fState
    evalStateT (write sh True "USER" (nick' ++ " 0 * :user")) fState
    _   <- forkIO (do threadDelay 20000000
                      if not $ null passwd then
                         evalStateT (replyMsg b "nickserv" []
                                     ("IDENTIFY " ++ passwd)) fState
                         else return ()
                      evalStateT (joinChannel sh "JOIN" channels) fState)
    pId <- forkIO $ pingLoop lock sh
    _   <- incT tc pId
    return fState
  where
    pingLoop l sh = forever $ do
      lock <- takeMVar l
      hPutStrLn sh "PING :foo\r"
      putMVar l lock
      threadDelay 60000000

stop :: Fstate -> IO ()
stop (bot, lock, tc, _) = do
    Bot{handle=h, params=p@Parameter{fuglyDir=fd, dictFile=df},
        fugly=f} <- readMVar bot
    hClose h
    tc' <- readMVar tc
    _   <- mapM killThread tc'
    stopFugly lock fd f df (paramsToList p)

run :: StateT Fstate IO b
run = do
    st <- get
    Bot{handle=h, params=p@Parameter{debug=d}} <- lift $ readMVar $ getBot st
    _   <- return p
    tId <- lift $ forkIO $ timerLoop st >> return ()
    _   <- lift $ incT (getTCount st) tId
    forever $ lift $ getLine' st h d
    where
      getLine' st h d = if d then do
        hGetLine h >>=
          (\l -> do evalStateT
                      (hPutStrLnLock stdout ("> debug: IRC msg: " ++ l)) st >>
                      return l) >>= (\ll -> listenIRC st h ll)
                    else do
                      hGetLine h >>= (\ll -> listenIRC st h ll)

listenIRC :: Fstate -> Handle -> String -> IO ()
listenIRC st h l = do
    let b = getBot st
    bot@Bot{params=p@Parameter{nick=bn, owner=o, debug=d, greetings=g}} <- readMVar b
    let cn = getChanNicks st
    r   <- Random.getStdRandom (Random.randomR (0, 99)) :: IO Int
    cn' <- readMVar cn
    let rr  = mod (r + length l + length (Map.toList cn') + 50) 100
        ll  = words l
        lll = take 2 $ drop 1 ll
        lm  = unwords $ drop 1 ll
    if "PING :" `isPrefixOf` l then do
      evalStateT (write h d "PONG" (':' : drop 6 l)) st >> return ()
      else if "433 " `isPrefixOf` lm then do
        evalStateT (write h d "NICK" (bn ++ "_")) st >>
          swapMVar b bot{params=p{nick=(bn ++ "_")}} >>
          return ("Nickname is already in use.") >>=
          (\x -> evalStateT (replyMsg bot o [] x) st)
        else if "353 " `isPrefixOf` lm then
          let cnicks = words $ drop 1 $ dropWhile (\x -> x /= ':') $ drop 1 l
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
        serverPos    = (maximum' $ elemIndices "-server" args) + 1
        server       = if l > serverPos then
                          args !! serverPos else "irc.freenode.net"
        portPos      = (maximum' $ elemIndices "-port" args) + 1
        port         = if l > portPos then
                          args !! portPos else "6667"
        nickPos      = (maximum' $ elemIndices "-nick" args) + 1
        nick'        = if l > nickPos then
                          args !! nickPos else "fuglybot"
        ownerPos     = (maximum' $ elemIndices "-owner" args) + 1
        owner'       = if l > ownerPos then
                          args !! ownerPos else "shadowdaemon"
        channelPos   = (maximum' $ elemIndices "-channel" args) + 1
        channel      = if l > channelPos then
                          args !! channelPos else "#fuglybot"
        topicPos     = (maximum' $ elemIndices "-topic" args) + 1
        topic'       = if l > topicPos then
                          args !! topicPos else "default"
        fuglyDirPos  = (maximum' $ elemIndices "-fuglydir" args) + 1
        fuglyDir'    = if l > fuglyDirPos then
                          args !! fuglyDirPos else "fugly"
        dFilePos     = (maximum' $ elemIndices "-dictfile" args) + 1
        dFile        = if l > dFilePos then
                          args !! dFilePos else "default"
        wnDirPos     = (maximum' $ elemIndices "-wndir" args) + 1
        wnDir        = if l > wnDirPos then
                          args !! wnDirPos else "/usr/share/wordnet/dict/"
        gfDirPos     = (maximum' $ elemIndices "-gfdir" args) + 1
        gfDir        = if l > gfDirPos then
                          args !! gfDirPos else "gf"
        passwdPos    = (maximum' $ elemIndices "-passwd" args) + 1
        passwd       = if l > passwdPos then
                          args !! passwdPos else ""
        socks5Pos    = (maximum' $ elemIndices "-socks" args) + 1
        socks5       = if l > socks5Pos then
                          args !! socks5Pos else ""
    return (server : port : nick' : owner' : channel : topic' : fuglyDir'
            : dFile : wnDir : gfDir : passwd : socks5 : [])
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
        | (x == "NICK") = return ("Nick change successful.") >>=
                          (\x' -> evalStateT (replyMsg bot o [] x') st') >>
                          return bot{params=p{nick=drop 1 y}}
        | otherwise     = return ("Nick change failed!") >>=
                          (\x' -> evalStateT (replyMsg bot o [] x') st') >>
                          return bot{params=p{nick=old}}
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
readParamsFromList a = Parameter
    {nick="", owner="", fuglyDir="", dictFile="",
     userCmd=read (a!!0), rejoinKick=read (a!!1), maxChanMsg=read (a!!2),
     numThreads=read (a!!3), nSetSize=read (a!!4), sTries=read (a!!5),
     sLength=read (a!!6), pLength=read (a!!7), learning=read (a!!8),
     strictLearn=read (a!!9), strictTopic=read (a!!10), autoName=read (a!!11),
     allowPM=read (a!!12), debug=read (a!!13), Main.topic=(a!!14),
     randoms=read (a!!15), replaceWord=read (a!!16), timer=read (a!!17),
     delay=read (a!!18), greetings=read (a!!19), actions=read (a!!20)}

paramsToList :: Parameter -> [String]
paramsToList (Parameter _ _ _ _ uc rk mcm nt ss st sl
              pl l stl stt an apm d t r rw ti dl g a) =
  [show uc, show rk, show mcm, show nt, show ss, show st, show sl,
   show pl, show l, show stl, show stt, show an, show apm, show d,
   t, show r, show rw, show ti, show dl, show g, show a]
paramsToList _ = []

changeParam :: Bot -> String -> String -> String -> String
               -> StateT Fstate IO Bot
changeParam bot@Bot{handle=h, params=p@Parameter{nick=botNick, owner=owner',
                    fuglyDir=fDir, dictFile=dFile, debug=debug'},
                    fugly=f@Fugly{dict=dict', defs=defs', ban=ban',
                    FuglyLib.match=match'}} chan nick' param value = do
    lock <- gets getLock
    case readParam param of
      Nick           -> (write h debug' "NICK" $
                         cleanStringWhite isAscii value) >> return bot
      Owner          -> replyMsg' value "Owner"
                        >>= (\x -> return bot{params=p{owner=x}})
      Topic          -> replyMsg' value "Topic"
                        >>= (\x -> return bot{params=p{Main.topic=x}})
      UserCommands   -> replyMsg' (readBool value) "User commands"
                        >>= (\x -> return bot{params=p{userCmd=x}})
      RejoinKick     -> replyMsg' (readInt 1 4096 value) "Rejoin kick time"
                        >>= (\x -> return bot{params=p{rejoinKick=x}})
      MaxChanMsg     -> replyMsg' (readInt 9 450 value) "Max channel message"
                        >>= (\x -> return bot{params=p{maxChanMsg=x}})
      NumThreads     -> replyMsg' (readInt 1 50 value) "Number of threads"
                        >>= (\x -> return bot{params=p{numThreads=x}})
      NSetSize       -> replyMsg' (readInt 2 256 value) "NSet size"
                        >>= (\x -> return bot{params=p{nSetSize=x}})
      SentenceTries  -> replyMsg' (readInt 1 4096 value) "Sentence tries"
                        >>= (\x -> return bot{params=p{sTries=x}})
      SentenceLength -> replyMsg' (readInt 2 256 value) "Sentence length"
                        >>= (\x -> return bot{params=p{sLength=x}})
      ParseLength    -> replyMsg' (readInt 0 256 value) "Parse length"
                        >>= (\x -> return bot{params=p{pLength=x}})
      Learning       -> replyMsg' (readBool value) "Learning"
                        >>= (\x -> return bot{params=p{learning=x}})
      StrictLearn    -> replyMsg' (readBool value) "Strict learn"
                        >>= (\x -> return bot{params=p{strictLearn=x}})
      StrictTopic    -> replyMsg' (readBool value) "Strict topic"
                        >>= (\x -> return bot{params=p{strictTopic=x}})
      Autoname       -> replyMsg' (readBool value) "Auto name"
                        >>= (\x -> return bot{params=p{autoName=x}})
      AllowPM        -> replyMsg' (readBool value) "Allow PM"
                        >>= (\x -> return bot{params=p{allowPM=x}})
      Debug          -> replyMsg' (readBool value) "Debug"
                        >>= (\x -> return bot{params=p{debug=x}})
      Randoms        -> replyMsg' (readInt 0 100 value) "Randoms"
                        >>= (\x -> return bot{params=p{randoms=x}})
      ReplaceWords   -> replyMsg' (readBool value) "Replace words"
                        >>= (\x -> return bot{params=p{replaceWord=x}})
      Timer          -> replyMsg' (readInt 0 36000 value) "Timer"
                        >>= (\x -> return bot{params=p{timer=x}})
      Delay          -> replyMsg' (readInt 0 120 value) "Delay"
                        >>= (\x -> return bot{params=p{delay=x}})
      Greetings      -> replyMsg' (readInt 0 100 value) "Greetings"
                        >>= (\x -> return bot{params=p{greetings=x}})
      Actions        -> replyMsg' (readInt 0 100 value) "Actions"
                        >>= (\x -> return bot{params=p{actions=x}})
      DictFile -> do (d, de, b, m, pl) <- lift $
                       catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then
                                          return (Map.empty, [], [], [], (paramsToList p))
                                        else
                                          return (dict', defs', ban', match', (paramsToList p)))
                       (loadDict fDir value) (\_ -> return (Map.empty, [], [], [], (paramsToList p)))
                     _ <- lift $ saveDict lock f fDir dFile (paramsToList p)
                     let pp = (readParamsFromList pl){nick=botNick, owner=owner', fuglyDir=fDir}
                     replyMsg' value "Dict file" >>= (\x -> return bot{params=pp{dictFile=x},
                       fugly=f{dict=d, defs=de, ban=b, FuglyLib.match=m}})
      _ -> return bot
  where
    replyMsg' v msg = do replyMsg bot chan nick' (msg ++ " set to " ++ show v ++ ".")
                         return v
    readInt min' max' a
      | aa < min' = min'
      | aa > max' = max'
      | otherwise = aa
      where
        aa = read a
changeParam bot _ _ _ _ = return bot

readBool :: String -> Bool
readBool a
    | (map toLower a) == "true"    = True
    | (map toLower a) == "yes"     = True
    | (map toLower a) == "on"      = True
    | (map toLower a) == "false"   = False
    | (map toLower a) == "no"      = False
    | (map toLower a) == "off"     = False
    | otherwise                    = False

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
    | (head $ drop 1 a) == "KICK" = if (head $ drop 3 a) == n then
                                       getChannel a else []
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
        bot@Bot{handle=h, params=pp@Parameter{nick=botnick,
                Main.topic=topic', debug=d, actions=acts},
                fugly=f@Fugly{defs=defs'}} <- readMVar b
        _ <- return (pp, f)
        let norm    = [de | (type', de) <- defs', type' == Normal]
            lenNorm = length norm
            cn      = getChanNicks st
        cn' <- readMVar cn
        let chans = concat [[c] | (c, _) <- Map.toList cn']
            chan  = chans!!mod (r + 55) (length chans)
        evalStateT (getChannelNicks h chan) st
        threadDelay 1000000
        let nicks = Map.lookup chan cn'
        load <- getLoad
        if isJust nicks && (read $ fHead [] load :: Float) < 2.5 then do
          let n  = delete botnick $ fromJust nicks
              m' = cleanStringBlack (\x -> x == '@' || x == '&' ||
                     x == '~' || x == '+') $ n!!mod (r + 23) (length n)
              n' = cleanStringBlack (\x -> x == '@' || x == '&' ||
                     x == '~' || x == '+') $ n!!mod r (length n)
              msg = case mod r $ 12 + lenNorm of
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
                _  -> if lenNorm == 0 then "Hmm..."
                      else fixAction m' topic' $ norm!!mod r lenNorm
          action <- ircAction False n' m' topic' defs'
          let who = if mod (r + 11) 3 == 0 then n'
                    else chan in if r < acts * 10 then
            forkIO (evalStateT (write h d "PRIVMSG"
                   (chan ++ " :\SOHACTION " ++ action ++ "\SOH")) st)
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
    let action     = [de | (type', de) <- defs', type' == Action]
    let lenAction  = length action
    let gaction    = [de | (type', de) <- defs', type' == GreetAction]
    let lenGaction = length gaction
    let altnick    = if null nick2 then "" else " and " ++ nick2
    return (if greet then case mod r $ 6 + lenGaction of
                0 -> "waves to " ++ nick1 ++ "."
                1 -> "greets " ++ nick1 ++ "."
                2 -> "says hello to " ++ nick1 ++ "."
                3 -> "welcomes " ++ nick1 ++ " to the channel."
                _ -> if lenGaction == 0 then "waves."
                     else fixAction nick1 topic' $ gaction!!mod r lenGaction
            else case mod r $ 7 + lenAction of
                0 -> "is bored."
                1 -> "looks at " ++ nick1 ++ altnick ++ "."
                2 -> "waves to " ++ nick1 ++ altnick ++ "."
                3 -> "ponders."
                4 -> "thinks deeply."
                _  -> if lenAction == 0 then "yawns."
                      else fixAction nick1 topic' $ action!!mod r lenAction)

fixAction :: String -> String -> String -> String
fixAction _  _ [] = []
fixAction [] t m  = dePlenk $ unwords $ replace "#topic" t $
                    replace "#nick" "somebody" $ words m
fixAction n [] m  = dePlenk $ unwords $ replace "#topic" "stuff" $
                    replace "#nick" n $ words m
fixAction n  t m  = dePlenk $ unwords $ replace "#topic" t $
                    replace "#nick" n $ words m

greeting :: [String] -> StateT Fstate IO ()
greeting []   = return ()
greeting line = do
    b <- gets getBot
    bot@Bot{handle=h, params=p@Parameter{nick=n, debug=d, Main.topic=top},
            fugly=f@Fugly{defs=defs'}} <- lift $ takeMVar b
    _ <- return (p, f)
    r <- lift $ Random.getStdRandom (Random.randomR (0, 999)) :: StateT Fstate IO Int
    lift $ threadDelay (600000 * (mod r 8) + 2000000)
    action <- lift $ ircAction True who [] top defs'
    let enter     = [de | (t, de) <- defs', t == Enter]
        lenEnter  = length enter
        greet     = [de | (t, de) <- defs', t == Greeting]
        lenGreet  = length greet
    if who == n then
      case mod r $ 6 + lenEnter of
        0 -> replyMsg bot chan [] "Hello world."
        1 -> replyMsg bot chan [] "Good morning!"
        2 -> replyMsg bot chan [] "Hello friends."
        3 -> replyMsg bot chan [] "\SOHACTION wonders if this is the right channel.\SOH"
        _ -> replyMsg bot chan [] $ if lenEnter == 0 then "Hello world." else
                                      fixAction [] top $ enter!!mod r lenEnter
      else if r < 600 then case mod r $ 6 + lenGreet of
          0 -> replyMsg bot chan [] ("Hello " ++ who ++ ".")
          1 -> replyMsg bot chan [] ("Welcome to the channel, " ++ who ++ ".")
          2 -> replyMsg bot chan who "Greetings."
          3 -> replyMsg bot chan who "Hello."
          _ -> let l = if lenGreet == 0 then "Hello." else greet!!mod r lenGreet in
            replyMsg bot chan (if elem "#nick" $ words l then [] else who) $
              fixAction who top l
           else
             write h d "PRIVMSG" (chan ++ " :\SOHACTION " ++ action ++ "\SOH")
    lift $ putMVar b bot >> return ()
  where
    who  = getNick line
    chan = dropWhile (\x -> x == ':') $ getChannel line

processLine :: Int -> [String] -> StateT Fstate IO ()
processLine _ []   = return ()
processLine r line = do
    b <- gets getBot
    bot@Bot{handle=h, params=p@Parameter{nick=n, rejoinKick=rk}} <- lift $ takeMVar b
    _ <- return p
    let bk = beenKicked n line
    if (not $ null bk) then do
      (rejoinChannel h bk rk >>lift (putMVar b bot) >> return ())
      else if null msg then lift $ putMVar b bot >> return ()
         else if chan == n then do
             nb <- prvcmd bot ; lift $ putMVar b nb >> return ()
              else if spokenTo n msg then
                if null (tail msg) then lift $ putMVar b bot >> return ()
                else if (head $ head $ tail msg) == '!' then do
                  nb <- execCmd bot chan who (tail msg)
                  lift $ putMVar b nb >> return ()
                    else do nb <- reply bot r False chan who (tail msg)
                            lift $ putMVar b nb >> return ()
                  else do nb <- reply bot r True chan who msg
                          lift $ putMVar b nb >> return ()
  where
    msg  = getMsg line
    who  = getNick line
    chan = getChannel line
    prvcmd bot = if (length $ head msg) > 0 then
                   if (head $ head msg) == '!' then execCmd bot who who msg
                   else reply bot r False [] who msg
                 else reply bot r False [] who msg

reply :: Bot -> Int -> Bool -> String -> String -> [String]
         -> StateT Fstate IO Bot
reply bot _ _ _ _ [] = return bot
reply bot@Bot{handle=h, params=p@Parameter{nick=bn, owner=o,
              learning=l, pLength=plen, strictLearn=sl,
              autoName=an, allowPM=apm, debug=d, Main.topic=top,
              nSetSize=nsets, actions=acts},
              fugly=f@Fugly{defs=defs', pgf=pgf', aspell=aspell',
                            dict=dict', FuglyLib.match=match',
                            ban=ban'}, lastm=lastm'}
  r chanmsg chan nick' msg = do
    st@(_, lock, _, _) <- get :: StateT Fstate IO Fstate
    _ <- return p
    let mmsg = if null $ head msg then msg
                 else case fLast ' ' $ head msg of
                   ':' -> tail msg
                   ',' -> tail msg
                   _   -> msg
    load <- lift getLoad
    n    <- lift $ isName    lock aspell' dict' $ head mmsg
    a    <- lift $ isAcronym lock aspell' dict' $ head mmsg
    r'   <- lift $ Random.getStdRandom (Random.randomR (0, 99)) :: StateT Fstate IO Int
    let fload    = read $ fHead [] load :: Float
        rr       = mod (r + r') 100
        fmsg     = nmsg n a mmsg
        parse    = if sl then
                     if fload < 2.5 then gfParseBool pgf' plen $ unwords fmsg
                     else False
                   else True
        matchon  = map toLower (" " ++ intercalate " | " (bn : match') ++ " ")
        isaction = head msg == "\SOHACTION"
    action <- lift $ ircAction False nick' [] top defs'
    if null fmsg then return () else lift $
      (if null chan then
         if apm && not isaction then
           sentenceReply st bot rr load nick' [] fmsg >> return ()
         else return ()
       else if chanmsg then
         if (" " ++ map toLower (unwords fmsg) ++ " ") =~ matchon then
           if isaction && rr - 60 < acts  || rr * 5 + 15 < acts then
             evalStateT (write h d "PRIVMSG"
               (chan ++ " :\SOHACTION "++ action ++ "\SOH")) st
           else sentenceReply st bot rr load chan chan fmsg >> return ()
         else return ()
           else sentenceReply st bot rr load chan nick' fmsg >> return ())
    (nn, ns, nm) <- lift $ nnInsert f nsets lastm' fmsg
    if ((nick' == o && null chan) || parse) && l &&
       not isaction && noban fmsg ban' then do
      nd <- lift $ insertWords lock f an top fmsg
      hPutStrLnLock stdout ("> parse: " ++ unwords fmsg)
      return bot{fugly=f{dict=nd, nnet=nn, nset=ns, nmap=nm}, lastm=fmsg} else
      return bot{fugly=f{nnet=nn, nset=ns, nmap=nm}, lastm=fmsg}
  where
    nmsg _ _ []     = []
    nmsg n a (x:xs) = let x' = if n || a then x else map toLower x in
      map cleanString (x':xs)
    noban []     _ = True
    noban (x:xs) b = if elem x b then False
                     else noban xs b
reply bot _ _ _ _ _ = return bot

sentenceReply :: Fstate -> Bot -> Int -> [String] -> String
                 -> String -> [String] -> IO ThreadId
sentenceReply _ _ _ _ _ _ [] = forkIO $ return ()
sentenceReply st@(_, lock, tc, _) bot@Bot{handle=h,
                params=p@Parameter{numThreads=nt, sTries=str,
                                   sLength=slen, pLength=plen,
                                   Main.topic=top, randoms=rand, replaceWord=rw,
                                   strictTopic=stopic, debug=d, delay=dl},
                fugly=fugly'@Fugly{defs=defs'}} r load chan nick' m = forkIO (do
    tId <- myThreadId
    tc' <- incT tc tId
    _   <- if d then evalStateT (hPutStrLnLock stdout
                       ("> debug: thread count: " ++ show tc')) st
           else return ()
    _   <- return p
    let fload = read $ fHead [] load :: Float
        r'    = 1 + mod r 7
        rr    = mod r 5
        n'    = if nick' == chan then "somebody" else nick'
        nn    = if nick' == chan || null nick' then [] else nick' ++ ": "
    action <- ircAction False n' [] top defs'
    if tc' < (nt + 2) && fload < 4.3 then do
      let d1     = dl * 1000000
          bdelay = (if dl < 4 then 0 else if r' - 3 > 0 then r' - 3 else 0) * 9
          sdelay = (if rr - 2 > 0 then rr - 2 else 0) * 3 in
            threadDelay $ d1 * (1 + sdelay + if bdelay > 90 then 90 else bdelay)
      let num    = if r' - 4 < 1 || str < 4 || length m < 7 then 1 else r' - 4
      x <- sentenceA lock fugly' r d rw stopic rand str slen top m
      y <- if null x then sentenceB lock fugly' r d rw stopic rand str
                          slen plen top num m
           else return []
      let ww = if null x then unwords y else x
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
    lift $ catch (execCmd' b st)
      (\e -> do let err = show (e :: SomeException)
                evalStateT (hPutStrLnLock stderr
                  ("Exception in execCmd: " ++ err)) st
                return b)
  where
    execCmd' :: Bot -> Fstate -> IO Bot
    execCmd' bot@Bot{handle=h, params=p@(Parameter botnick owner' fdir
                                       dfile userCmd' rkick maxcmsg numt nsets
                                       sTries' slen plen learn slearn stopic
                                       autoName' allowPM' debug' topic' randoms'
                                       replaceWord' timer' delay' greets actions'),
                     fugly=f@(Fugly dict' defs' pgf' wne' aspell' ban' match' _ _ _)} st
      | userCmd' == False && nick' /= owner' = return bot
      | x == "!quit" = if nick' == owner' then case length xs of
          0 -> do evalStateT (write h debug' "QUIT" ":Bye") st >> return bot
          _ -> do evalStateT (write h debug' "QUIT" (":" ++ unwords xs)) st >> return bot
                       else return bot
      | x == "!save" = if nick' == owner' then let l' = getLock st in
                         catch (do {saveDict l' f fdir dfile (paramsToList p) ; saveNeural l' f fdir})
                         (\e -> do let err = show (e :: SomeException)
                                   evalStateT (hPutStrLnLock stderr ("Exception saving state: " ++ err)) st
                                   return ())
                         >> replyMsgT st bot chan nick' "Saved bot state!" >> return bot
                       else return bot
      | x == "!load" = if nick' == owner' then do
           (nd, nde, nb, nm, np) <- catch (loadDict fdir dfile)
                                    (\e -> do let err = show (e :: SomeException)
                                              evalStateT (hPutStrLnLock stderr ("Exception in loadDict: " ++ err)) st
                                              return (dict', defs', ban', match', paramsToList p))
           (ns, nm') <- catch (loadNeural fdir nsets)
                        (\e -> do let err = show (e :: SomeException)
                                  hPutStrLn stderr ("Exception in loadNeural: " ++ err)
                                  return ([], Map.empty))
           replyMsgT st bot chan nick' "Loaded bot state!"
           return bot{params=(readParamsFromList np){nick=botnick, owner=owner', fuglyDir=fdir, dictFile=dfile},
                      fugly=f{dict=nd, defs=nde, pgf=pgf', wne=wne', aspell=aspell', ban=nb, FuglyLib.match=nm, nset=ns, nmap=nm'}}
                       else return bot
      | x == "!join" = if nick' == owner' then evalStateT (joinChannel h "JOIN" xs) st >> return bot else return bot
      | x == "!part" = if nick' == owner' then evalStateT (joinChannel h "PART" xs) st >> return bot else return bot
      | x == "!nick" = if nick' == owner' then case length xs of
          1 -> evalStateT ((\x' -> write h debug' "NICK" $ cleanStringWhite isAscii x') (xs!!0)) st >> return bot
          _ -> replyMsgT st bot chan nick' "Usage: !nick <nick>" >> return bot
                       else return bot
      | x == "!internalize" = if nick' == owner' then
          if length xs > 1 then do replyMsgT st bot chan nick' ("Internalizing...")
                                   internalize st bot (read (xs!!0)) $ unwords $ tail xs
          else
            replyMsgT st bot chan nick' "Usage: !internalize <tries> <msg>" >> return bot
                              else return bot
      | x == "!showparams" = if nick' == owner' then case length xs of
          0 -> replyMsgT st bot chan nick' ("nick: " ++ botnick ++ "  owner: " ++ owner' ++
                   "  usercommands: " ++ show userCmd' ++ "  rejoinkick: "
                   ++ show rkick ++ "  maxchanmsg: " ++ show maxcmsg
                   ++ "  numthreads: " ++ show numt ++ "  nsetsize: " ++ show nsets
                   ++ "  sentencetries: " ++ show sTries' ++ "  sentencelength: "
                   ++ show slen ++ "  parselength: " ++ show plen ++ "  dictfile: " ++ dfile
                   ++ "  learning: " ++ show learn ++ "  strictlearn: " ++ show slearn
                   ++ "  stricttopic: " ++ show stopic ++ "  debug: " ++ show debug'
                   ++ "  autoname: " ++ show autoName' ++ "  allowpm: " ++ show allowPM'
                   ++ "  topic: " ++ topic' ++ "  randoms: " ++ show randoms'
                   ++ "  replacewords: " ++ show replaceWord'
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
                   if notElem (xs!!2) $ wordGetBanAfter $ fromJust w then
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
                   replyMsgT st bot chan nick' ("Learning on word " ++ (xs!!1) ++ " already banned.") >> return bot
                 else
                   replyMsgT st bot chan nick' ("Banned learning on word " ++ (xs!!1) ++ ".") >>
                     return bot{fugly=f{ban=nub $ ban' ++ [(xs!!1)]}}
               else if (xs!!0) == "delete" then
                 if notElem (xs!!1) ban' then
                   replyMsgT st bot chan nick' ("Word " ++ (xs!!1) ++ " not in ban list.") >> return bot
                 else
                   replyMsgT st bot chan nick' ("Unbanned learning on word " ++ (xs!!1) ++ ".") >>
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
                 if notElem (xs!!1) match' then
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
              r'   <- Random.getStdRandom (Random.randomR (0, 99))
              load <- getLoad
              sentenceReply st bot r' load (xs!!0) (xs!!1) (drop 2 xs) >> return bot
            else replyMsgT st bot chan nick' "Usage: !talk <channel> <nick> <msg>" >> return bot
            else return bot
      | x == "!raw" = if nick' == owner' then
          if length xs > 0 then evalStateT (write h debug' (xs!!0) (unwords $ tail xs)) st >> return bot
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
      | x == "!params" = if nick' == owner' then replyMsgT st bot chan nick' (init (concat $ map (++ " ")
                                      $ map show $ init allParams)) >> return bot
                         else return bot
      | otherwise  = if nick' == owner' then replyMsgT st bot chan nick'
          ("Commands: !word !wordlist !insertword !name !namelist !insertname !acronym !acronymlist !insertacronym "
          ++ "!dropword !banword !matchword !insertdefault !dropdefault !dropafter !banafter "
          ++ "!ageword !listtopics !droptopic !droptopicwords !internalize "
          ++ "!dict !closure !meet !parse !related !forms !parts !isname !isacronym "
          ++ "!setparam !showparams !nick !join !part !talk !raw !quit !load !save") >> return bot
                     else replyMsgT st bot chan nick'
          ("Commands: !word !wordlist !name !namelist !acronym !acronymlist !listtopics "
          ++ "!dict !closure !meet !related !forms !parts !isname !isacronym") >> return bot
    execCmd' bot _ = return bot

replyMsg :: Bot -> String -> String -> String -> StateT Fstate IO ()
replyMsg _ _ _ [] = return ()
replyMsg bot@Bot{handle=h, params=p@Parameter{maxChanMsg=mcm, debug=d}}
           chan nick' msg
    | null nick' = if length msg > mcm then do
      _ <- return p
      write h d "PRIVMSG" (chan ++ " :" ++ (take mcm msg))
      lift $ threadDelay 2000000
      replyMsg bot chan [] (drop mcm msg) else
        write h d "PRIVMSG" (chan ++ " :" ++ msg)
    | chan == nick' = if length msg > mcm then do
      write h d "PRIVMSG" (nick' ++ " :" ++ (take mcm msg))
      lift $ threadDelay 2000000
      replyMsg bot chan nick' (drop mcm msg) else
        write h d "PRIVMSG" (nick' ++ " :" ++ msg)
    | otherwise = if length msg > mcm then do
      write h d "PRIVMSG" (chan ++ " :" ++ nick' ++ ": " ++ (take mcm msg))
      lift $ threadDelay 2000000
      replyMsg bot chan nick' (drop mcm msg) else
        write h d "PRIVMSG" (chan ++ " :" ++ nick' ++ ": " ++ msg)
replyMsg _ _ _ _ = return ()

replyMsgT :: Fstate -> Bot -> String -> String -> String -> IO ()
replyMsgT _  _   _    _     []  = return ()
replyMsgT st bot chan nick' msg = evalStateT (replyMsg bot chan nick' msg) st

internalize :: Fstate -> Bot -> Int -> String -> IO Bot
internalize _  b 0 _   = return b
internalize _  b _ []  = return b
internalize st b n msg = internalize' st b n 0 msg
  where
    internalize' :: Fstate -> Bot -> Int -> Int -> String -> IO Bot
    internalize' _ bot _ _ [] = return bot
    internalize' st' bot@Bot{params=p@Parameter{autoName=aname,
                             Main.topic=topic', sTries=tries,
                             sLength=slen, debug=d, pLength=plen,
                             replaceWord=rw, strictTopic=stopic,
                             randoms=rands}, fugly=f}
      num i imsg = do
      _   <- return p
      r   <- Random.getStdRandom (Random.randomR (0, 2)) :: IO Int
      sen <- getSentence $ sentenceB' (getLock st') f r d False rw stopic
             rands tries slen plen topic' $ words imsg
      nd  <- insertWords (getLock st') f aname topic' $ words sen
      if i >= num then return bot
        else if r == 0 then evalStateT (hPutStrLnLock stdout
          ("> internalize: " ++ msg)) st >>
          internalize' st bot{fugly=f{dict=nd}} num (i + 1) msg
          else evalStateT (hPutStrLnLock stdout ("> internalize: " ++ sen)) st >>
               internalize' st bot{fugly=f{dict=nd}} num (i + 1) sen
    internalize' _ bot _ _ _ = return bot
    getSentence []     = return []
    getSentence (x:xs) = do
      ww <- x
      if null ww then getSentence xs
        else return ww

write :: Handle -> Bool -> [Char] -> [Char] -> StateT Fstate IO ()
write _     _ [] _  = return ()
write h d s []  = do
    hPutStrLnLock h (s ++ "\r")
    if d then hPutStrLnLock stdout ("> debug: " ++ s) else return ()
write h d s msg = do
    hPutStrLnLock h (s ++ " " ++ msg ++ "\r")
    if d then hPutStrLnLock stdout ("> debug: " ++ s ++ " " ++ msg)
      else return ()

hPutStrLnLock :: Handle -> String -> StateT Fstate IO ()
hPutStrLnLock h m = do
    st <- get :: StateT Fstate IO Fstate
    let l = getLock st
    lock <- lift $ takeMVar l
    lift (do hPutStrLn h m ; putMVar l lock)
