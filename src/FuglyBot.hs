import           Control.Concurrent
import           Control.Exception              hiding (handle)
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Lazy
import           Data.Char                      (isAscii, toLower)
import           Data.List
import qualified Data.Map.Lazy                  as Map
import           Data.Maybe
import qualified Fugly.Command                  as Cmd
import           Fugly.LoadSave
import           Fugly.Parameter
import           Fugly.Reply
import           Fugly.Types                    hiding (topic)
import           FuglyLib                       hiding (hPutStrLnLock)
import           Network.Socket                 hiding (Debug)
import           Network.Socks5
import           Prelude
import           System.Environment
import           System.IO
import           System.IO.Error
import qualified System.Random                  as Random
import           Text.Regex.Posix               hiding (match)

main :: IO ()
main = do
    bracket start stop loop
  where
    loop :: FState -> IO ()
    loop st = do catch (evalStateT run st)
                   (\e -> do let err = show (e :: SomeException)
                             evalStateT (hPutStrLnLock stderr
                               ("Exception in main: " ++ err)) st
                             return ())

start :: IO FState
start = do
    args <- cmdLine
    let servname = args !! 0
        port     = args !! 1
        hints    = defaultHints {addrFlags = [AI_NUMERICSERV]}
        nick'    = cleanStringWhite isAscii (args !! 2)
        owners'  = words $ args !! 3
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
    (f, p) <- initFugly fDir wnDir gfDir dFile
    let b = if null p then
              Bot sh (Parameter nick' owners' fDir dFile False 10 400 8 64 10 7
                      0 [] False False True False True topic' 10 False 0 2
                      0 0) f
              else Bot sh ((readParamsFromList p){nick=nick', owners=owners',
                      fuglyDir=fDir, dictFile=dFile}) f
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
    return fState

stop :: FState -> IO ()
stop (bot, lock, tc, _) = do
    Bot{handle=h, params=p@Parameter{fuglyDir=fd, dictFile=df},
        fugly=f} <- readMVar bot
    hClose h
    tc' <- readMVar tc
    _   <- mapM killThread tc'
    stopFugly lock fd f df (paramsToList p)

run :: StateT FState IO b
run = do
    st <- get
    Bot{handle=h, params=p@Parameter{debug=d}} <- lift $ readMVar $ getBot st
    _   <- return p
    tId <- lift $ forkIO $ timerLoop st >> return ()
    _   <- lift $ incT (getTCount st) tId
    forever $ lift $ getLine' st h d
  where
    getLine' st h d = do
      l <- hGetLine h
      if d then
        evalStateT (hPutStrLnLock stdout ("> debug: IRC msg: " ++ l)) st >>
          listenIRC st h l
        else listenIRC st h l

listenIRC :: FState -> Handle -> String -> IO ()
listenIRC st h l = do
    let b    = getBot st
        lock = getLock st
    bot@Bot{params=p@Parameter{nick=bn, owners=o, debug=d, greetings=g}} <- readMVar b
    let cn = getChanNicks st
    r   <- Random.getStdRandom (Random.randomR (0, 99)) :: IO Int
    cn' <- readMVar cn
    let rr  = mod (r + length l + length (Map.toList cn') + 50) 100
    listenIRC' lock bot p b d bn o g cn cn' rr
  where
    ll  = words l
    lll = take 2 $ drop 1 ll
    lm  = unwords $ drop 1 ll
    getNicks' cn cn' = let
      cnicks = words $ drop 1 $ dropWhile (\x -> x /= ':') $ drop 1 l
      chan   = takeWhile (\x -> x /= ' ') $ dropWhile (\x -> x /= '#') l in
      if null cnicks then return ()
        else swapMVar cn (Map.insert chan cnicks cn') >> return ()
    listenIRC' lock bot p b d bn o g cn cn' rr
      | "PING :" `isPrefixOf` l = evalStateT (write h d "PONG" (':' : drop 6 l)) st >> return ()
      | "433 " `isPrefixOf` lm = evalStateT (write h d "NICK" (bn ++ "_")) st >>
          swapMVar b bot{params=p{nick=(bn ++ "_")}} >>
          return ("Nickname is already in use.") >>=
          (\x -> evalStateT (mapM (\y' -> replyMsg bot y' [] x) o) st) >> return ()
      | "353 " `isPrefixOf` lm = getNicks' cn cn'
      | (length ll > 2) && (fHead [] lll) == "NICK" && getNick ll == bn =
          (do nb <- evalStateT (changeNick lll) st ; swapMVar b nb) >> return ()
      | (length ll > 2) && (fHead [] lll) == "JOIN" && rr < g =
          evalStateT (greeting lock $ words l) st >> return ()
      | otherwise = (catch (evalStateT (processLine rr $ words l) st >> return ())
          (\e -> do let err = show (e :: SomeException)
                    evalStateT (hPutStrLnLock stderr ("Exception in processLine: " ++ err)) st
                    putMVar b bot
                    return ()))

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
        owners'      = if l > ownerPos then
                          args !! ownerPos else "shadowdaemon"
        channelPos   = (maximum' $ elemIndices "-channel" args) + 1
        channel      = if l > channelPos then
                          args !! channelPos else "#fuglybot"
        topicPos     = (maximum' $ elemIndices "-topic" args) + 1
        topic'       = if l > topicPos then
                          args !! topicPos else "stuff"
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
    return (server : port : nick' : owners' : channel : topic' : fuglyDir'
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

changeNick :: [String] -> StateT FState IO Bot
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
    testNick :: FState -> Bot -> String -> [String] -> IO Bot
    testNick _ bot [] _ = return bot
    testNick _ bot _ [] = return bot
    testNick st' bot@Bot{params=p@Parameter{owners=o}} old line'
        | (x == "NICK") = return ("Nick change successful.") >>=
                          (\x' -> evalStateT (mapM (\y' -> replyMsg bot y' [] x') o) st') >>
                          return bot{params=p{nick=drop 1 y}}
        | otherwise     = return ("Nick change failed!") >>=
                          (\x' -> evalStateT (mapM (\y' -> replyMsg bot y' [] x') o) st') >>
                          return bot{params=p{nick=old}}
      where
        x = fHead [] line'
        y = fLast [] line'
    testNick _ bot _ _ = return bot

joinChannel :: Handle -> String -> [String] -> StateT FState IO ()
joinChannel _ _  []    = return () :: StateT FState IO ()
joinChannel h [] b     = joinChannel h "join" b
joinChannel h a (x:xs) = do
    if a == "JOIN" || a == "PART" then do
      write h False a x
      joinChannel h a xs
        else return ()

changeParam :: Bot -> String -> String -> String -> String
               -> StateT FState IO Bot
changeParam bot@Bot{handle=h, params=p@Parameter{nick=botNick, owners=owners',
                    fuglyDir=fDir, dictFile=dFile, debug=debug'},
                    fugly=f@Fugly{dict=dict', defs=defs', ban=ban',
                    match=match'}} chan nick' param value = do
    lock <- gets getLock
    case readParam param of
      Nick           -> (write h debug' "NICK" $
                         cleanStringWhite isAscii value) >> return bot
      Owners         -> replyMsg' value "Owners"
                        >>= \x -> return bot{params=p{owners=words x}}
      Topic          -> replyMsg' value "Topic"
                        >>= \x -> return bot{params=p{topic=x}}
      UserCommands   -> replyMsg' (readBool value) "User commands"
                        >>= \x -> return bot{params=p{userCmd=x}}
      RejoinKick     -> replyMsg' (readInt 1 4096 value) "Rejoin kick time"
                        >>= \x -> return bot{params=p{rejoinKick=x}}
      MaxChanMsg     -> replyMsg' (readInt 9 450 value) "Max channel message"
                        >>= \x -> return bot{params=p{maxChanMsg=x}}
      NumThreads     -> replyMsg' (readInt 1 50 value) "Number of threads"
                        >>= \x -> return bot{params=p{numThreads=x}}
      NSetSize       -> replyMsg' (readInt 2 256 value) "NSet size"
                        >>= \x -> return bot{params=p{nSetSize=x}}
      SentenceTries  -> replyMsg' (readInt 1 4096 value) "Sentence tries"
                        >>= \x -> return bot{params=p{sTries=x}}
      SentenceLength -> replyMsg' (readInt 2 256 value) "Sentence length"
                        >>= \x -> return bot{params=p{sLength=x}}
      ParseLength    -> replyMsg' (readInt 0 256 value) "Parse length"
                        >>= \x -> return bot{params=p{pLength=x}}
      Learning       -> replyMsg' value "Learning"
                        >>= \x -> return bot{params=p{learning=x}}
      StrictLearn    -> replyMsg' (readBool value) "Strict learn"
                        >>= \x -> return bot{params=p{strictLearn=x}}
      StrictTopic    -> replyMsg' (readBool value) "Strict topic"
                        >>= \x -> return bot{params=p{strictTopic=x}}
      Autoname       -> replyMsg' (readBool value) "Auto name"
                        >>= \x -> return bot{params=p{autoName=x}}
      AllowPM        -> replyMsg' (readBool value) "Allow PM"
                        >>= \x -> return bot{params=p{allowPM=x}}
      Debug          -> replyMsg' (readBool value) "Debug"
                        >>= \x -> return bot{params=p{debug=x}}
      Randoms        -> replyMsg' (readInt 0 100 value) "Randoms"
                        >>= \x -> return bot{params=p{randoms=x}}
      ReplaceWords   -> replyMsg' (readBool value) "Replace words"
                        >>= \x -> return bot{params=p{replaceWord=x}}
      Timer          -> replyMsg' (readInt 0 36000 value) "Timer"
                        >>= \x -> return bot{params=p{timer=x}}
      Delay          -> replyMsg' (readInt 0 120 value) "Delay"
                        >>= \x -> return bot{params=p{delay=x}}
      Greetings      -> replyMsg' (readInt 0 100 value) "Greetings"
                        >>= \x -> return bot{params=p{greetings=x}}
      Actions        -> replyMsg' (readInt 0 100 value) "Actions"
                        >>= \x -> return bot{params=p{actions=x}}
      DictFile -> do
        (d, de, b, m, pl) <- lift $
          catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then
                             return (Map.empty, [], [], [], (paramsToList p))
                           else
                             return (dict', defs', ban', match', (paramsToList p)))
          (loadDict fDir value) (\_ -> return (Map.empty, [], [], [], (paramsToList p)))
        _ <- lift $ saveDict lock f fDir dFile (paramsToList p)
        let pp = (readParamsFromList pl){nick=botNick, owners=owners', fuglyDir=fDir}
        replyMsg' value "Dict file" >>= \x -> return bot{params=pp{dictFile=x},
          fugly=f{dict=d, defs=de, ban=b, match=m}}
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
    p = if (length $ drop 1 msg) > 0 then head $ drop 1 msg else ""

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

getChannelNicks :: Handle -> String -> StateT FState IO ()
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
    | (head $ drop 1 a) == "KICK" =
      if (head $ drop 3 a) == n then getChannel a else []
    | otherwise = []

rejoinChannel :: Handle -> String -> Int -> StateT FState IO ()
rejoinChannel _ []   _  = return () :: StateT FState IO ()
rejoinChannel h chan rk = do
    if rk == 0 then return () else rejoin' rk chan h >> return ()
  where
    rejoin' rk' chan' h' = do
      st <- get
      lift $ forkIO (threadDelay (rk' * 1000000) >>
        evalStateT (hPutStrLnLock h' ("JOIN " ++ chan' ++ "\r")) st)

timerLoop :: FState -> IO ThreadId
timerLoop st = do
    let tc = getTCount st
    tId <- myThreadId
    _   <- incT tc tId
    forever $ do
      r <- Random.getStdRandom (Random.randomR (0, 999))
      let b    = getBot st
          lock = getLock st
      Bot{params=p@Parameter{timer=t}} <- readMVar b
      _ <- return p
      threadDelay $ if (t < 5) then 30000000 else t * 1000000 + (t * r * 500)
      if t > 4 then do
        bot@Bot{handle=h, params=pp@Parameter{nick=botnick,
                randoms=rand, replaceWord=rw, debug=d,
                strictTopic=st', topic=topic', actions=acts},
                fugly=f@Fugly{defs=defs'}} <- readMVar b
        _ <- return (pp, f)
        let norm = [de | (type', de) <- defs', type' == Normal]
            cn   = getChanNicks st
        cn' <- readMVar cn
        let chans = concat [[c] | (c, _) <- Map.toList cn']
            chan  = chans!!mod (r + 55) (length chans)
        evalStateT (getChannelNicks h chan) st
        threadDelay 1000000
        let nicks = Map.lookup chan cn'
        load <- getLoad
        if isJust nicks && (read $ fHead [] load :: Float) < 2.5 then do
          let n   = delete botnick $ fromJust nicks
              n'  = cleanStringBlack (\x -> x == '@' || x == '&' ||
                      x == '~' || x == '+') $ n!!mod r (length n)
              who = if mod (r + 11) 3 == 0 then n' else chan
          msg <- if length norm == 0 then return "Well this is interesting..." else
                   defsReplace lock f r d rw st' rand topic' n' $ norm!!(mod r $ length norm)
          action <- ircAction lock f d rw st' rand False n' topic' defs'
          if (not $ null action) && r < acts * 10 then
            forkIO (evalStateT (write h d "PRIVMSG"
                   (chan ++ " :\SOHACTION " ++ action ++ "\SOH")) st)
            else if r < 850 then
              forkReply st bot r load chan who $ words msg
                 else
                   forkIO $ replyMsgT st bot chan who msg
          else forkIO $ return ()
        else forkIO $ return ()

ircAction :: MVar () -> Fugly -> Bool -> Bool -> Bool -> Int
             -> Bool -> String -> String -> [Default] -> IO String
ircAction _ _ _ _  _   _    _ [] _  _     = return []
ircAction _ _ _ _  _   _    _ _  _  []    = return []
ircAction l f d rw st' rand g n top defs' = do
    r <- Random.getStdRandom (Random.randomR (0, 999)) :: IO Int
    let action  = [de | (t, de) <- defs', t == Action]
        gaction = [de | (t, de) <- defs', t == GreetAction]
        rep     = defsReplace l f r d rw st' rand top n
    if g then
      if (length gaction) == 0 then return []
      else rep $ gaction!!mod r (length gaction)
      else
        if (length action) == 0 then return []
        else rep $ action!!mod r (length action)

greeting :: MVar () -> [String] -> StateT FState IO ()
greeting _    []   = return ()
greeting lock line = do
    b <- gets getBot
    bot@Bot{handle=h, params=p@Parameter{nick=n, debug=d, topic=top,
            replaceWord=rw, strictTopic=st, randoms=rand},
            fugly=f@Fugly{defs=defs'}} <- lift $ takeMVar b
    _ <- return (p, f)
    r <- lift $ Random.getStdRandom (Random.randomR (0, 999)) :: StateT FState IO Int
    lift $ threadDelay (600000 * (mod r 8) + 2000000)
    action <- lift $ ircAction lock f d rw st rand True who top defs'
    let enter = [de | (t, de) <- defs', t == Enter]
        greet = [de | (t, de) <- defs', t == Greeting]
    if who == n then do
      rep <- lift $ defsReplace lock f r d rw st rand top [] $ enter!!mod r (length enter)
      replyMsg bot chan [] $ if (length enter) == 0 then [] else rep
      else
      if null action || r < 600 then do
        let l = if (length greet) == 0 then [] else greet!!mod r (length greet)
        rep <- lift $ defsReplace lock f r d rw st rand top who l
        replyMsg bot chan (if elem "#nick" $ words l then [] else who) rep
      else
        write h d "PRIVMSG" (chan ++ " :\SOHACTION " ++ action ++ "\SOH")
    lift $ putMVar b bot >> return ()
  where
    who  = getNick line
    chan = dropWhile (\x -> x == ':') $ getChannel line

processLine :: Int -> [String] -> StateT FState IO ()
processLine _ []   = return ()
processLine r line = do
    b <- gets getBot
    bot@Bot{handle=h, params=p@Parameter{nick=n, rejoinKick=rk}} <- lift $ takeMVar b
    _ <- return p
    let bk = beenKicked n line
    if (not $ null bk) then do
      (rejoinChannel h bk rk >> lift (putMVar b bot) >> return ())
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
         -> StateT FState IO Bot
reply bot _ _ _ _ [] = return bot
reply bot@Bot{handle=h, params=p@Parameter{nick=bn, learning=learn', pLength=plen,
              autoName=an, allowPM=apm, debug=d, topic=top, replaceWord=rw, randoms=rand,
              actions=acts, strictLearn=sl, strictTopic=st'},
              fugly=f@Fugly{defs=defs', pgf=pgf', aspell=aspell',
                            dict=dict', match=match', ban=ban'}}
  r chanmsg chan nick' msg = do
    st@(_, lock, _, _) <- get :: StateT FState IO FState
    _ <- return p
    let mmsg = if null $ head msg then msg
                 else case fLast ' ' $ head msg of
                   ':' -> tail msg
                   ',' -> tail msg
                   _   -> msg
    load <- lift getLoad
    n    <- lift $ isName    lock aspell' dict' $ head mmsg
    a    <- lift $ isAcronym lock aspell' dict' $ head mmsg
    r'   <- lift $ Random.getStdRandom (Random.randomR (0, 99)) :: StateT FState IO Int
    let fload    = read $ fHead [] load :: Float
        rr       = mod (r + r') 100
        fmsg     = nmsg n a mmsg
        parse    = if sl then
                     if fload < 2.5 then gfParseBool pgf' plen $ unwords fmsg
                     else False
                   else True
        matchOn  = map toLower (" " ++ intercalate " | " (bn : match') ++ " ")
        isAction = head msg == "\SOHACTION"
        l        = nick' =~ learn' || chan =~ learn'
    if null fmsg then return () else lift $ (
      if null chan then
        if apm && not isAction then
          forkReply st bot rr load nick' [] fmsg >> return ()
        else return ()
      else
        if chanmsg then
          if rr < 35 && (" " ++ map toLower (unwords fmsg) ++ " ") =~ matchOn then
            if isAction && (rr - 60 < acts  || rr * 5 + 15 < acts) then do
              action <- ircAction lock f d rw st' rand False nick' top defs'
              if null action then return ()
                else evalStateT (write h d "PRIVMSG" (chan ++ " :\SOHACTION "++ action ++ "\SOH")) st
            else forkReply st bot rr load chan (if r' < 55 then chan else nick') fmsg >> return ()
          else return ()
        else forkReply st bot rr load chan nick' fmsg >> return ())
    if l && parse && not isAction && noban fmsg ban' then do
      nd           <- lift $ insertWords lock f an top fmsg
      hPutStrLnLock stdout ("> parse: " ++ unwords fmsg)
      return bot{fugly=f{dict=nd}} else
      return bot
  where
    nmsg _ _ []     = []
    nmsg n a (x:xs) = let x' = if n || a then x else map toLower x in
      map cleanString (x':xs)
    noban []     _ = True
    noban (x:xs) b = if elem x b then False
                     else noban xs b
reply bot _ _ _ _ _ = return bot

forkReply :: FState -> Bot -> Int -> [String] -> String
             -> String -> [String] -> IO ThreadId
forkReply _ _ _ _ _ _ [] = forkIO $ return ()
forkReply st@(_, lock, tc, _) Bot{handle=h,
                params=p@Parameter{numThreads=nt, sTries=str,
                                   sLength=slen, pLength=plen,
                                   topic=top, randoms=rand, replaceWord=rw,
                                   strictTopic=stopic, debug=d, delay=dl},
                fugly=fugly'} r load chan nick' msg = forkIO (do
    tId <- myThreadId
    tc' <- incT tc tId
    _   <- return p
    if d then
      evalStateT (hPutStrLnLock stdout ("> debug: thread count: " ++ show tc')) st
      else return ()
    let fload = read $ fHead [] load :: Float
        r'    = 1 + mod r 7
        rr    = mod r 6
    if tc' < (nt + 2) && fload < 4.3 then do
      let num = if r' - 4 < 1 || str < 4 || length msg < 7 then 1 else r' - 4
          dl' = dl * 1000000
          bd  = (if dl < 4 then 0 else if r' - 3 > 0 then r' - 3 else 0) * 9
          sd  = (if rr - 2 > 0 then rr - 2 else 0) * 3
          v = replyResponse lock fugly' r d rw stopic rand 1 nick' top $ unwords msg
          w = replyRegex lock fugly' r d rw stopic rand nick' top $ unwords msg
          x = replyMixed lock fugly' r d rw stopic rand str slen top msg
          y = replyRandom lock fugly' r d rw stopic rand str slen plen top num msg
          z = replyDefault lock fugly' r d nick' top
      threadDelay $ dl' * (1 + sd + if bd > 90 then 90 else bd)
      out <- getResponse [v, w, x, y, z]
      evalStateT (do
        if null out then return () else
          if null nick' || nick' == chan || rr == 0 || rr == 2 then
            write h d "PRIVMSG" $ chan ++ " :" ++ out
          else write h d "PRIVMSG" $ chan ++ " :" ++ nick' ++ ": " ++ out) st
      else return ()
    decT tc tId)
  where
    getResponse []     = return []
    getResponse (x:xs) = do
      x' <- x
      if null x' then getResponse xs else return x'
forkReply _ _ _ _ _ _ _ = forkIO $ return ()

execCmd :: Bot -> String -> String -> [String] -> StateT FState IO Bot
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
    execCmd' :: Bot -> FState -> IO Bot
    execCmd' bot@Bot{handle=h, params=p@Parameter{owners=owners', userCmd=userCmd',
               debug=debug', topic=topic'},
               fugly=f@Fugly{pgf=pgf', wne=wne', aspell=aspell'}} st
      | userCmd' == False && not (elem nick' owners') = return bot
      | x == "!quit" = Cmd.quit bot st isOwner write' xs
      | x == "!save" = Cmd.save bot st isOwner showRep showErr
      | x == "!load" = Cmd.load bot st isOwner showRep showErr
      | x == "!join" = Cmd.join bot st isOwner (joinChannel h) xs
      | x == "!part" = Cmd.part bot st isOwner cn (joinChannel h) xs
      | x == "!nick" = Cmd.nick bot st isOwner showRep write' xs
      | x == "!showparams" = Cmd.showParams bot p isOwner showRep xs
      | x == "!setparam" = Cmd.setParam bot st isOwner showRep (changeParam bot chan nick') $ map (strip '"') $ joinWords '"' xs
      | x == "!word" || x == "!name" || x == "!acronym" = Cmd.word bot x showRep xs
      | x == "!wordlist" || x == "!namelist" || x == "!acronymlist" = Cmd.wordList bot x showRep xs
      | x == "!insertword" = Cmd.insertWord bot st isOwner showRep topic' xs
      | x == "!insertacronym" = Cmd.insertAcronym bot st isOwner showRep topic' xs
      | x == "!insertdefault" = Cmd.insertDefault bot isOwner showRep xs
      | x == "!dropdefault" = Cmd.dropDefault bot isOwner showRep xs
      | x == "!defaultlist" = Cmd.defaultList bot isOwner showRep xs
      | x == "!dropword" = Cmd.dropWord bot isOwner showRep xs
      | x == "!dropafter" = Cmd.dropAfter bot isOwner showRep xs
      | x == "!topiclist" = Cmd.topicList bot showRep xs
      | x == "!droptopic" = Cmd.dropTopic bot isOwner showRep xs
      | x == "!droptopicwords" = Cmd.dropTopicWords bot isOwner showRep xs
      | x == "!banafter" = Cmd.banAfter bot isOwner showRep xs
      | x == "!ageword" = Cmd.ageWord bot st isOwner showRep showErr xs
      | x == "!forcelearn" = Cmd.forceLearn bot st isOwner showRep showErr xs
      | x == "!banword" = Cmd.banWord bot isOwner showRep xs
      | x == "!matchword" = Cmd.matchWord bot isOwner showRep xs
      | x == "!talk" = Cmd.talk bot isOwner showRep (forkReply st) getLoad xs
      | x == "!raw" = Cmd.raw bot st isOwner showRep write' xs
      | x == "!dict" = Cmd.dictL bot st showRep xs
      | x == "!closure" = Cmd.closure bot showRep (wnClosure wne') xs
      | x == "!meet" = Cmd.meet bot showRep (wnMeet wne') xs
      -- | x == "!parse" = Cmd.parse bot isOwner showRep (gfParseShow pgf') xs
      | x == "!related" = Cmd.related bot showRep (wnRelated wne') xs
      | x == "!forms" = Cmd.showForms bot showRep xs
      | x == "!parts" = Cmd.showParts bot showRep xs
      | x == "!isname" = Cmd.isName bot showRep (asIsName (getLock st) aspell') xs
      | x == "!isacronym" = Cmd.isName bot showRep (asIsAcronym (getLock st) aspell') xs
      | x == "!params" = Cmd.listParams bot isOwner showRep
      | otherwise = if isOwner then showRep
          ("Commands: !word !wordlist !insertword !name !namelist !acronym !acronymlist !insertacronym "
          ++ "!dropword !banword !matchword !insertdefault !dropdefault !defaultlist !dropafter !banafter "
          ++ "!ageword !topiclist !droptopic !droptopicwords !forcelearn "
          ++ "!dict !closure !meet !parse !related !forms !parts !isname !isacronym "
          ++ "!setparam !showparams !nick !join !part !talk !raw !quit !load !save") >> return bot
                     else showRep
          ("Commands: !word !wordlist !name !namelist !acronym !acronymlist !topiclist "
          ++ "!dict !closure !meet !related !forms !parts !isname !isacronym") >> return bot
      where
        showRep = replyMsgT st bot chan nick'
        showErr = hPutStrLnLock stderr
        write'  = write h debug'
        cn      = getChanNicks st
        isOwner = elem nick' owners'
    execCmd' bot _ = return bot

replyMsg :: Bot -> String -> String -> String -> StateT FState IO ()
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

replyMsgT :: FState -> Bot -> String -> String -> String -> IO ()
replyMsgT _  _   _    _     []  = return ()
replyMsgT st bot chan nick' msg = evalStateT (replyMsg bot chan nick' msg) st

write :: Handle -> Bool -> String -> String -> StateT FState IO ()
write _     _ [] _  = return ()
write h d s []  = do
    hPutStrLnLock h (s ++ "\r")
    if d then hPutStrLnLock stdout ("> debug: " ++ s) else return ()
write h d s msg = do
    hPutStrLnLock h (s ++ " " ++ msg ++ "\r")
    if d then hPutStrLnLock stdout ("> debug: " ++ s ++ " " ++ msg)
      else return ()

hPutStrLnLock :: Handle -> String -> StateT FState IO ()
hPutStrLnLock h m = do
    st <- get :: StateT FState IO FState
    let l = getLock st
    lock <- lift $ takeMVar l
    lift (do hPutStrLn h m ; putMVar l lock)
