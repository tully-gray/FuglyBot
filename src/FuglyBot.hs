import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.Char
import Data.List
import qualified Data.Map.Strict as Map
import Network
import System.Environment (getArgs)
import System.IO
import System.IO.Error
import qualified System.Random as Random
import Text.Regex.Posix
import Prelude

import FuglyLib

data Bot = Bot {
    socket :: Handle,
    params :: Parameter,
    fugly :: Fugly
    }

data Parameter = Nick | Owner | UserCommands | RejoinKick | ThreadTime | MaxChanMsg
               | SentenceTries | SentenceLength | ParseLength | Learning | AllowPM
               | Topic | Randoms | UnknownParam
               | Parameter {
                 nick        :: String,
                 owner       :: String,
                 fuglydir    :: FilePath,
                 wndir       :: FilePath,
                 gfdir       :: FilePath,
                 usercmd     :: Bool,
                 rejoinkick  :: Int,
                 threadtime  :: Int,
                 maxchanmsg  :: Int,
                 stries      :: Int,
                 slength     :: Int,
                 plength     :: Int,
                 learning    :: Bool,
                 allowpm     :: Bool,
                 topic       :: String,
                 randoms     :: Int
                 }
               deriving (Eq, Ord, Show)

allParams :: [Parameter]
allParams = [Nick ..]

instance Enum Parameter where
    toEnum 1 = Nick
    toEnum 2 = Owner
    toEnum 3 = UserCommands
    toEnum 4 = RejoinKick
    toEnum 5 = ThreadTime
    toEnum 6 = MaxChanMsg
    toEnum 7 = SentenceTries
    toEnum 8 = SentenceLength
    toEnum 9 = ParseLength
    toEnum 10 = Learning
    toEnum 11 = AllowPM
    toEnum 12 = Topic
    toEnum 13 = Randoms
    toEnum 14 = UnknownParam
    toEnum _  = UnknownParam
    fromEnum Nick           = 1
    fromEnum Owner          = 2
    fromEnum UserCommands   = 3
    fromEnum RejoinKick     = 4
    fromEnum ThreadTime     = 5
    fromEnum MaxChanMsg     = 6
    fromEnum SentenceTries  = 7
    fromEnum SentenceLength = 8
    fromEnum ParseLength    = 9
    fromEnum Learning       = 10
    fromEnum AllowPM        = 11
    fromEnum Topic          = 12
    fromEnum Randoms        = 13
    fromEnum UnknownParam   = 14
    fromEnum _              = 14
    enumFrom i = enumFromTo i UnknownParam
    enumFromThen i j = enumFromThenTo i j UnknownParam

readParam :: String -> Parameter
readParam a | (map toLower a) == "nick"            = Nick
readParam a | (map toLower a) == "owner"           = Owner
readParam a | (map toLower a) == "usercmd"         = UserCommands
readParam a | (map toLower a) == "usercmds"        = UserCommands
readParam a | (map toLower a) == "usercommands"    = UserCommands
readParam a | (map toLower a) == "rejoinkick"      = RejoinKick
readParam a | (map toLower a) == "threadtime"      = ThreadTime
readParam a | (map toLower a) == "maxchanmsg"      = MaxChanMsg
readParam a | (map toLower a) == "stries"          = SentenceTries
readParam a | (map toLower a) == "sentencetries"   = SentenceTries
readParam a | (map toLower a) == "slen"            = SentenceLength
readParam a | (map toLower a) == "slength"         = SentenceLength
readParam a | (map toLower a) == "sentencelength"  = SentenceLength
readParam a | (map toLower a) == "plength"         = ParseLength
readParam a | (map toLower a) == "parselength"     = ParseLength
readParam a | (map toLower a) == "learning"        = Learning
readParam a | (map toLower a) == "allowpm"         = AllowPM
readParam a | (map toLower a) == "topic"           = Topic
readParam a | (map toLower a) == "randoms"         = Randoms
readParam _                                        = UnknownParam

main :: IO ()
main = do
    args <- cmdLine
    bracket (start args) stop (loop args)
  where
    stop :: MVar Bot -> IO ()
    stop bot = do
      b <- readMVar bot
      hClose $ (\(Bot s _ _) -> s) b
    loop :: [String] -> MVar Bot -> IO ()
    loop args bot = do catchIOError (evalStateT (run args) bot) (const $ return ())

start :: [String] -> IO (MVar Bot)
start args = do
    let server   = args !! 0
    let port     = read $ args !! 1 :: Integer
    let nick     = cleanStringWhite isAscii (args !! 2)
    let owner    = args !! 3
    let topic    = args !! 5
    let fuglydir = args !! 6 :: FilePath
    let wndir    = args !! 7 :: FilePath
    let gfdir    = args !! 8 :: FilePath
    socket <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering socket NoBuffering
    fugly <- initFugly fuglydir wndir gfdir topic
    let b = (Bot socket (Parameter nick owner fuglydir wndir gfdir False 10 90 400 100 10 7 False False topic 50) fugly)
    bot <- newMVar b
    write socket "NICK" nick
    write socket "USER" (nick ++ " 0 * :user")
    return bot

run :: [String] -> StateT (MVar Bot) IO b
run args = do
    b <- get
    bot <- lift $ readMVar b
    let s = (\(Bot s _ _) -> s) bot
    let channel = args !! 4
    let passwd  = args !! 9
    lift (forkIO (do
                     threadDelay 20000000
                     if not $ null passwd then replyMsg bot "nickserv" [] ("IDENTIFY " ++ passwd) else return ()
                     joinChannel s "JOIN" [channel]
                     forever (do write s "PING" ":foo" ; threadDelay 20000000))) >> return ()
    forever $ do
      l <- lift $ hGetLine s
      lift $ putStrLn l
      listenIRC b s l
    where
      listenIRC b s l = do
        bot <- lift $ readMVar b
        let ll = words l
        let lll = take 2 $ drop 1 ll
        let botnick = (\(Bot _ (Parameter {nick=n}) _) -> n) bot
        if "PING :" `isPrefixOf` l then do
          lift (write s "PONG" (':' : drop 6 l)) >> return ()
          else if (length ll > 2) && (head lll) == "NICK" && getNick ll == botnick then do
            lift (do nb <- evalStateT (changeNick [] lll) b ; swapMVar b nb) >> return ()
              else do
                lift (forkIO (runInBoundThread $ evalStateT (processLine $ words l) b)) >> return ()

cmdLine :: IO [String]
cmdLine = do
    args <- getArgs
    let l            = length args
    let serverPos    = (maximum' $ elemIndices "-server" args) + 1
    let server       = if l > serverPos then args !! serverPos else "irc.freenode.net"
    let portPos      = (maximum' $ elemIndices "-port" args) + 1
    let port         = if l > portPos then args !! portPos else "6667"
    let nickPos      = (maximum' $ elemIndices "-nick" args) + 1
    let nick         = if l > nickPos then args !! nickPos else "fuglybot"
    let ownerPos     = (maximum' $ elemIndices "-owner" args) + 1
    let owner        = if l > ownerPos then args !! ownerPos else "shadowdaemon"
    let channelPos   = (maximum' $ elemIndices "-channel" args) + 1
    let channel      = if l > channelPos then args !! channelPos else "#fuglybot"
    let topicPos     = (maximum' $ elemIndices "-topic" args) + 1
    let topic        = if l > topicPos then args !! topicPos else "default"
    let fuglydirPos  = (maximum' $ elemIndices "-fuglydir" args) + 1
    let fuglydir     = if l > fuglydirPos then args !! fuglydirPos
                                            else "/var/lib/fuglybot"
    let wndirPos     = (maximum' $ elemIndices "-wndir" args) + 1
    let wndir        = if l > wndirPos then args !! wndirPos
                                            else "/usr/share/wordnet/dict/"
    let gfdirPos     = (maximum' $ elemIndices "-gfdir" args) + 1
    let gfdir        = if l > gfdirPos then args !! gfdirPos
                                            else "/var/lib/fuglybot"
    let passwdPos    = (maximum' $ elemIndices "-passwd" args) + 1
    let passwd       = if l > passwdPos then args !! passwdPos else ""
    return (server : port : nick : owner : channel : topic : fuglydir : wndir : gfdir : passwd : [])
  where
    maximum' [] = 1000
    maximum' a  = maximum a

changeNick :: [String] -> [String] -> StateT (MVar Bot) IO Bot
changeNick (_ : _) (_ : _) = do
    b <- get
    bot <- lift $ readMVar b
    return bot
changeNick (x:_) [] = do
    b <- get
    bot <- lift $ readMVar b
    let socket = (\(Bot s _ _) -> s) bot
    let new = cleanStringWhite isAscii x
    lift $ write socket "NICK" new
    return bot
changeNick [] line = do
    b <- get
    bot <- lift $ readMVar b
    let nick = (\(Bot _ (Parameter {nick = n}) _) -> n) bot
    lift $ testNick bot nick line
  where
    testNick :: Bot -> String -> [String] -> IO Bot
    testNick bot [] _ = return bot
    testNick bot _ [] = return bot
    testNick bot@(Bot socket params@(Parameter {owner = o}) fugly)
      old line
        | (x == "NICK") = return ("Nick change successful.") >>= replyMsg bot o []
                          >> return (Bot socket params{nick = drop 1 y} fugly)
        | otherwise     = return ("Nick change failed!") >>= replyMsg bot o []
                          >> return (Bot socket params{nick = old} fugly)
      where
        x = head line
        y = last line
    testNick bot@(Bot _ _ _) _ _ = return bot

joinChannel :: Handle -> String -> [String] -> IO ()
joinChannel _ _  []    = return () :: IO ()
joinChannel h [] b     = joinChannel h "join" b
joinChannel h a (x:xs) = do
    if a == "JOIN" || a == "PART" then do
      write h a x
      joinChannel h a xs
        else return ()

changeParam :: Bot -> String -> String -> String -> String -> IO Bot
changeParam bot@(Bot _ p@(Parameter {fuglydir=fd, topic=t}) f) chan nick param value = do
    case (readParam param) of
      Nick           -> do nb <- newMVar bot ; evalStateT (changeNick (value : "" : []) []) nb
      Owner          -> replyMsg' value                 "Owner"               >>= (\x -> return bot{params=p{owner=x}})
      UserCommands   -> replyMsg' (readBool value)      "User commands"       >>= (\x -> return bot{params=p{usercmd=x}})
      RejoinKick     -> replyMsg' (readInt 1 4096 value) "Rejoin kick time"   >>= (\x -> return bot{params=p{rejoinkick=x}})
      ThreadTime     -> replyMsg' (readInt 1 4096 value) "Thread time"        >>= (\x -> return bot{params=p{threadtime=x}})
      MaxChanMsg     -> replyMsg' (readInt 0 450 value) "Max channel message" >>= (\x -> return bot{params=p{maxchanmsg=x}})
      SentenceTries  -> replyMsg' (readInt 0 4096 value) "Sentence tries"     >>= (\x -> return bot{params=p{stries=x}})
      SentenceLength -> replyMsg' (readInt 0 256 value) "Sentence length"     >>= (\x -> return bot{params=p{slength=x}})
      ParseLength    -> replyMsg' (readInt 2 256 value) "Parse length"        >>= (\x -> return bot{params=p{plength=x}})
      Learning       -> replyMsg' (readBool value)     "Learning"             >>= (\x -> return bot{params=p{learning=x}})
      AllowPM        -> replyMsg' (readBool value)     "Allow PM"             >>= (\x -> return bot{params=p{allowpm=x}})
      Topic          -> do (d, a, b) <- catchIOError (loadDict fd value) (const $ return (Map.empty, [], []))
                           _ <- saveDict f fd t
                           replyMsg' value "Topic" >>= (\x -> return bot{params=p{topic=x},fugly=f{dict=d,allow=a,ban=b}})
      Randoms        -> replyMsg' (readInt 0 100 value) "Randoms"             >>= (\x -> return bot{params=p{randoms=x}})
      _              -> return bot
  where
    replyMsg' value msg = do replyMsg bot chan nick (msg ++ " set to " ++ show value ++ ".") >> return value
    readInt min max a
      | aa < min = min
      | aa > max = max
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

rejoinChannel :: Handle -> String -> Int -> IO ()
rejoinChannel _ []   _  = return () :: IO ()
rejoinChannel h chan rk = do
    if rk == 0 then return () else rejoin' rk chan h >> return ()
  where
    rejoin' rk chan h = forkIO (threadDelay (rk * 1000000) >>
                                hPutStr h ("JOIN " ++ chan ++ "\r\n"))

processLine :: [String] -> StateT (MVar Bot) IO ()
processLine [] = return ()
processLine line = do
    b <- get
    bot <- lift $ takeMVar b
    t <- lift $ myThreadId
    let socket = (\(Bot s _ _) -> s) bot
    let nick = (\(Bot _ (Parameter {nick = n}) _) -> n) bot
    let rejoinkick = (\(Bot _ (Parameter {rejoinkick = r}) _) -> r) bot
    let ttime = (\(Bot _ (Parameter {threadtime = t}) _) -> t * 1000000) bot
    let bk = beenKicked nick line
    {-- Kill long lived threads --}
    -- lift $ forkIO (do threadDelay ttime ; putMVar b bot ; killThread t) >> return ()
    if (not $ null bk) then do lift (rejoinChannel socket bk rejoinkick >> putMVar b bot)
      else if null msg then lift $ putMVar b bot
         else if chan == nick then do nb <- prvcmd bot ; lift $ putMVar b nb
           else if spokenTo nick msg then if null (tail msg) then lift $ putMVar b bot
                                          else if (head $ head $ tail msg) == '!'
                                            then do nb <- execCmd bot chan who (tail msg)
                                                    lift $ putMVar b nb
                                               else do nb <- reply bot chan who (tail msg)
                                                       lift $ putMVar b nb
             else do nb <- reply bot chan [] msg ; lift $ putMVar b nb
  where
    msg  = getMsg line
    who  = getNick line
    chan = getChannel line
    prvcmd bot = if (length $ head msg) > 0 then
                   if (head $ head msg) == '!' then execCmd bot who who msg
                   else reply bot [] who msg
                 else reply bot [] who msg

reply :: (Monad (t IO), MonadTrans t) =>
          Bot -> String -> String -> [String] -> t IO Bot
reply bot _ _ [] = return bot
reply bot@(Bot socket params@(Parameter botnick owner _ _ _ _ _ _ _ stries slen plen learning allowpm _ randoms)
           fugly@(Fugly _ pgf wne _ _ _)) chan nick msg = do
    let mmsg = if null $ head msg then msg
                 else case fLast "reply" ' ' $ head msg of
                   ':' -> tail msg
                   ',' -> tail msg
                   _   -> msg
    fmsg <- lift $ asReplaceWords fugly $ map cleanString mmsg
    let parse = gfParseBool pgf plen $ unwords fmsg
    mm <- lift $ chooseWord wne fmsg
    _ <- if null chan then if allowpm then lift $ sentenceReply socket fugly nick [] randoms stries slen plen 2 mm
                           else return ()
         else if null nick then if length fmsg > 2 && (unwords fmsg) =~ botnick then
                                  lift $ sentenceReply socket fugly chan chan randoms stries slen plen 1 mm
                                else return ()
           else lift $ sentenceReply socket fugly chan nick randoms stries slen plen 2 mm
    if ((nick == owner && null chan) || parse) && learning then do
      nd <- lift $ insertWords fugly fmsg
      lift $ putStrLn ">parse<"
      return (Bot socket params fugly{dict=nd}) else
      return bot

execCmd :: MonadTrans t => Bot -> String -> String -> [String] -> t IO Bot
execCmd bot _ _ [] = lift $ return bot
execCmd bot _ [] _ = lift $ return bot
execCmd bot [] _ _ = lift $ return bot
execCmd bot chan nick (x:xs) = do
    lift $ execCmd' bot
  where
    execCmd' :: Bot -> IO Bot
    execCmd' bot@(Bot socket params@(Parameter botnick owner fuglydir _ _
                                     usercmd rejoinkick threadtime maxchanmsg
                                     stries slen plen learning allowpm topic randoms)
                  fugly@(Fugly dict pgf wne aspell allow ban))
      | usercmd == False && nick /= owner = return bot
      | x == "!quit" =
        if nick == owner then case (length xs) of
          0 -> do stopFugly fuglydir fugly topic >>
                    write socket "QUIT" ":Bye" >> return bot
          _ -> do stopFugly fuglydir fugly topic >>
                    write socket "QUIT" (":" ++ unwords xs) >> return bot
        else return bot
      | x == "!save" = if nick == owner then catchIOError (saveDict fugly fuglydir topic)
                                       (const $ return ()) >> replyMsg bot chan nick "Saved dict file!"
                                             >> return bot else return bot
      | x == "!load" = if nick == owner then do
           (nd, na, nb) <- catchIOError (loadDict fuglydir topic) (const $ return (dict, [], []))
           replyMsg bot chan nick "Loaded dict file!"
           return (Bot socket params (Fugly nd pgf wne aspell na nb))
                       else return bot
      | x == "!join" = if nick == owner then joinChannel socket "JOIN" xs >>
                                             return bot else return bot
      | x == "!part" = if nick == owner then joinChannel socket "PART" xs >>
                                             return bot else return bot
      | x == "!nick" = if nick == owner then do nb <- newMVar bot ; evalStateT (changeNick xs []) nb else return bot
      | x == "!readfile" = if nick == owner then case (length xs) of
          1 -> catchIOError (insertFromFile bot (xs!!0)) (const $ return bot)
          _ -> replyMsg bot chan nick "Usage: !readfile <file>" >>
               return bot else return bot
      | x == "!showparams" =
          if nick == owner then case (length xs) of
            0 -> replyMsg bot chan nick ("nick: " ++ botnick ++ "  owner: " ++ owner ++
                   "  usercommands: " ++ show usercmd ++ "  rejoinkick: "
                   ++ show rejoinkick ++ "  threadtime: " ++ show threadtime ++ "  maxchanmsg: " ++ show maxchanmsg
                   ++ "  sentencetries: " ++ show stries ++ "  sentencelength: " ++ show slen ++ "  parselength: " ++ show plen
                   ++ "  learning: " ++ show learning ++ "  allowpm: " ++ show allowpm
                   ++ "  topic: " ++ topic ++ "  randoms: " ++ show randoms) >> return bot
            _ -> replyMsg bot chan nick "Usage: !showparams" >> return bot
          else return bot
      | x == "!setparam" =
            if nick == owner then case (length xs) of
              2 -> changeParam bot chan nick (xs!!0) (xs!!1)
              _ -> replyMsg bot chan nick "Usage: !setparam <parameter> <value>" >> return bot
            else return bot
      | x == "!params" =
              if nick == owner then replyMsg bot chan nick (init (concat $ map (++ " ")
                                      $ map show $ init allParams)) >> return bot
              else return bot
      | x == "!dict" =
          case (length xs) of
            2 -> (dictLookup fugly (xs!!0) (xs!!1)) >>= replyMsg bot chan nick >> return bot
            1 -> (dictLookup fugly (xs!!0) []) >>= replyMsg bot chan nick >> return bot
            _ -> replyMsg bot chan nick "Usage: !dict <word> [part-of-speech]" >> return bot
      | x == "!wordlist" =
          let num = if read (xs!!0) > (100 :: Integer) then 100 :: Int else read (xs!!0) in
          case (length xs) of
            1 -> replyMsg bot chan nick (unwords $ listWordsCountSort2 dict num)
                 >> replyMsg bot chan nick ("Total word count: " ++ (show $ Map.size dict))
                 >> return bot
            _ -> replyMsg bot chan nick "Usage: !wordlist <number>" >> return bot
      | x == "!word" = case (length xs) of
            1 -> replyMsg bot chan nick (listWordFull dict (xs!!0)) >> return bot
            _ -> replyMsg bot chan nick "Usage: !word <word>" >> return bot
      | x == "!insertword" = if nick == owner then
          case (length xs) of
            2 -> do ww <- insertWordRaw fugly (xs!!1) [] [] (xs!!0)
                    replyMsg bot chan nick ("Inserted word " ++ (xs!!1))
                    return (Bot socket params fugly{dict=ww})
            _ -> replyMsg bot chan nick "Usage: !insertword <pos> <word>" >> return bot
                         else return bot
      | x == "!dropword" = if nick == owner then
          case (length xs) of
            1 -> replyMsg bot chan nick ("Dropped word " ++ (xs!!0)) >>
                 return (Bot socket params fugly{dict=dropWord dict (xs!!0)})
            _ -> replyMsg bot chan nick "Usage: !dropword <word>"
                 >> return bot
                         else return bot
      | x == "!ageword" = if nick == owner then
          case (length xs) of
            1 -> replyMsg bot chan nick ("Aged word " ++ (xs!!0)) >>
                 return (Bot socket params fugly{dict=ageWord dict (xs!!0)})
            _ -> replyMsg bot chan nick "Usage: !ageword <word>"
                 >> return bot
                         else return bot
      | x == "!agewords" = if nick == owner then
          case (length xs) of
            0 -> replyMsg bot chan nick ("Aged all words...") >>
                 return (Bot socket params fugly{dict=ageWords dict})
            _ -> replyMsg bot chan nick "Usage: !agewords"
                 >> return bot
                         else return bot
      | x == "!banword" = if nick == owner then
          case (length xs) of
            2 -> if (xs!!0) == "add" then
                    replyMsg bot chan nick ("Banned word " ++ (xs!!1)) >>
                    return (Bot socket params (Fugly (dropWord dict (xs!!1)) pgf wne aspell
                                               allow (nub $ ban ++ [(xs!!1)])))
                 else if (xs!!0) == "delete" then
                    replyMsg bot chan nick ("Unbanned word " ++ (xs!!1)) >>
                    return (Bot socket params (Fugly dict pgf wne aspell allow
                                               (nub $ delete (xs!!1) ban)))
                 else replyMsg bot chan nick "Usage: !banword <list|add|delete> <word>"
                      >> return bot
            1 -> if (xs!!0) == "list" then
                    replyMsg bot chan nick ("Banned word list: " ++ unwords ban)
                    >> return bot
                 else replyMsg bot chan nick "Usage: !banword <list|add|delete> <word>"
                      >> return bot
            _ -> replyMsg bot chan nick "Usage: !banword <list|add|delete> <word>"
                 >> return bot
                         else return bot
      | x == "!allowword" = if nick == owner then
          case (length xs) of
            2 -> if (xs!!0) == "add" then
                    replyMsg bot chan nick ("Allowed word " ++ (xs!!1)) >>
                    return (Bot socket params (Fugly dict pgf wne aspell
                                               (nub $ allow ++ [(xs!!1)]) ban))
                 else if (xs!!0) == "delete" then
                    replyMsg bot chan nick ("Unallowed word " ++ (xs!!1)) >>
                    return (Bot socket params (Fugly dict pgf wne aspell
                                               (nub $ delete (xs!!1) allow) ban))
                 else replyMsg bot chan nick "Usage: !allowword <list|add|delete> <word>"
                      >> return bot
            1 -> if (xs!!0) == "list" then
                    replyMsg bot chan nick ("Allowed word list: " ++ unwords allow)
                    >> return bot
                 else replyMsg bot chan nick "Usage: !allowword <list|add|delete> <word>"
                      >> return bot
            _ -> replyMsg bot chan nick "Usage: !allowword <list|add|delete> <word>"
                 >> return bot
                         else return bot
      | x == "!namelist" =
          let num = if read (xs!!0) > (100 :: Integer) then 100 :: Int else read (xs!!0) in
          case (length xs) of
            1 -> replyMsg bot chan nick (unwords $ listNamesCountSort2 dict num)
                 >> replyMsg bot chan nick ("Total name count: " ++ (show $ length $
                                             filter (\x -> wordIs x == "name") $ Map.elems dict))
                 >> return bot
            _ -> replyMsg bot chan nick "Usage: !namelist <number>" >> return bot
      | x == "!name" = case (length xs) of
            1 -> replyMsg bot chan nick (listWordFull dict (xs!!0)) >> return bot
            _ -> replyMsg bot chan nick "Usage: !name <name>" >> return bot
      | x == "!insertname" = if nick == owner then
          case (length xs) of
            1 -> do ww <- insertName fugly (xs!!0) [] []
                    replyMsg bot chan nick ("Inserted name " ++ (xs!!0))
                    return (Bot socket params fugly{dict=ww})
            _ -> replyMsg bot chan nick "Usage: !insertname <name>" >> return bot
                           else return bot
      | x == "!internalize" = if nick == owner then
                                if length xs > 1 then do replyMsg bot chan nick ("Internalizing...")
                                                         internalize bot (read (xs!!0)) $ unwords $ tail xs
                                else
                                  replyMsg bot chan nick "Usage: !internalize <tries> <msg>" >> return bot
                           else return bot
      | x == "!talk" = if nick == owner then
          if length xs > 2 then sentenceReply socket fugly (xs!!0) (xs!!1) randoms stries slen plen 2 (drop 2 xs)
                                  >> return bot
          else replyMsg bot chan nick "Usage: !talk <channel> <nick> <msg>" >> return bot
                     else return bot
      | x == "!raw" = if nick == owner then
          if (length xs) > 0 then write socket (xs!!0)(unwords $ tail xs)
                                  >> return bot
          else replyMsg bot chan nick "Usage: !raw <msg>" >> return bot
                     else return bot
      | x == "!related" = case (length xs) of
            3 -> (wnRelated wne (xs!!0) (xs!!1) (xs!!2)) >>= replyMsg bot chan nick >> return bot
            2 -> (wnRelated wne (xs!!0) (xs!!1) []) >>= replyMsg bot chan nick >> return bot
            1 -> (wnRelated wne (xs!!0) [] []) >>= replyMsg bot chan nick >> return bot
            _ -> replyMsg bot chan nick "Usage: !related <word> [form] [part-of-speech]"
                 >> return bot
      | x == "!closure" = case (length xs) of
            3 -> (wnClosure wne (xs!!0) (xs!!1) (xs!!2)) >>= replyMsg bot chan nick
                 >> return bot
            2 -> (wnClosure wne (xs!!0) (xs!!1) []) >>= replyMsg bot chan nick >> return bot
            1 -> (wnClosure wne (xs!!0) [] []) >>= replyMsg bot chan nick >> return bot
            _ -> replyMsg bot chan nick "Usage: !closure <word> [part-of-speech]"
                 >> return bot
      | x == "!meet" = case (length xs) of
            3 -> (wnMeet wne (xs!!0) (xs!!1) (xs!!2)) >>= replyMsg bot chan nick
                 >> return bot
            2 -> (wnMeet wne (xs!!0) (xs!!1) []) >>= replyMsg bot chan nick >> return bot
            _ -> replyMsg bot chan nick "Usage: !meet <word> <word> [part-of-speech]"
                 >> return bot
      | x == "!parse" = case (length xs) of
            0 -> replyMsg bot chan nick "Usage: !parse <sentence>" >> return bot
            _ -> (sequence $ map (replyMsg bot chan nick) $ take 3
                  (gfParseC pgf (unwords $ take 12 xs))) >> return bot
      | x == "!gfcats" = case (length xs) of
            0 -> return (unwords $ gfCategories pgf) >>= replyMsg bot chan nick >> return bot
            _ -> replyMsg bot chan nick "Usage: !gfcats" >> return bot
      -- | x == "!random" = case (length xs) of
      --       1 -> replyMsg bot chan nick (gfAll pgf (read (xs!!0))) >> return bot
      --       _ -> replyMsg bot chan nick "Usage: !random <number>" >> return bot
      | x == "!test" = if nick == owner then
            replyMsg bot chan nick (unwords $ map show $ take 750 $ iterate succ (0 :: Int)) >> return bot
            else return bot
      | otherwise  = if nick == owner then replyMsg bot chan nick
                       ("Commands: !dict !word !wordlist !insertword !dropword "
                       ++ "!banword !allowword !namelist !name !insertname !closure !meet !parse "
                       ++ "!related !gfcats !ageword(s) !internalize "
                       ++ "!params !setparam !showparams !nick !join !part !talk !raw "
                       ++ "!quit !readfile !load !save") >> return bot
                     else replyMsg bot chan nick ("Commands: !dict !word !wordlist !name "
                       ++ "!closure !meet !parse !related !gfcats") >> return bot
    execCmd' bot = return bot

sentenceReply :: Handle -> Fugly -> String -> String -> Int -> Int -> Int -> Int -> Int -> [String] -> IO ()
sentenceReply h fugly chan nick randoms stries slen plen num m = do
    x <- f (sentence fugly randoms stries slen plen m) [] num 0
    let ww = unwords x
    if null nick then hPutStr h ("PRIVMSG " ++ (chan ++ " :" ++ ww) ++ "\r\n") >>
                      hPutStr stdout ("> PRIVMSG " ++ (chan ++ " :" ++ ww) ++ "\n")
      else if nick == chan then hPutStr h ("PRIVMSG " ++ (chan ++ " :" ++ ww) ++ "\r\n") >>
                                hPutStr stdout ("> PRIVMSG " ++ (chan ++ " :" ++ ww) ++ "\n")
           else hPutStr h ("PRIVMSG " ++ (chan ++ " :" ++ nick ++ ": " ++ ww) ++ "\r\n") >>
                hPutStr stdout ("> PRIVMSG " ++ (chan ++ " :" ++ nick ++ ": " ++ ww) ++ "\n")
  where
    f :: [IO String] -> [String] -> Int -> Int -> IO [String]
    f []     a n i = return a
    f (x:xs) a n i = do
      xx <- x
      if i >= n then return a
        else if null xx then f xs a n i
        else f xs ([xx ++ " "] ++ a) n (i + 1)

replyMsg :: Bot -> String -> String -> String -> IO ()
replyMsg bot@(Bot socket (Parameter {maxchanmsg=mcm}) _) chan nick msg
    | null nick      = if length msg > mcm then do
      write socket "PRIVMSG" (chan ++ " :" ++ (take mcm msg))
      threadDelay 2000000
      replyMsg bot chan [] (drop mcm msg) else
        write socket "PRIVMSG" (chan ++ " :" ++ msg)
    | chan == nick   = if length msg > mcm then do
      write socket "PRIVMSG" (nick ++ " :" ++ (take mcm msg))
      threadDelay 2000000
      replyMsg bot chan nick (drop mcm msg) else
        write socket "PRIVMSG" (nick ++ " :" ++ msg)
    | otherwise      = if length msg > mcm then do
      write socket "PRIVMSG" (chan ++ " :" ++ nick ++ ": " ++ (take mcm msg))
      threadDelay 2000000
      replyMsg bot chan nick (drop mcm msg) else
        write socket "PRIVMSG" (chan ++ " :" ++ nick ++ ": " ++ msg)
replyMsg _ _ _ _ = return ()

write :: Handle -> String -> String -> IO ()
write socket s msg = do
    hPutStr socket (s ++ " " ++ msg ++ "\r\n")
    hPutStr stdout ("> " ++ s ++ " " ++ msg ++ "\n")

insertFromFile :: Bot -> FilePath -> IO Bot
insertFromFile (Bot s p fugly) file = do
    f <- readFile file
    fmsg <- asReplaceWords fugly $ map cleanString $ words f
    n <- insertWords fugly fmsg
    return (Bot s p fugly{dict=n})

internalize :: Bot -> Int -> String -> IO Bot
internalize bot _ [] = return bot
internalize bot num msg = internalize' bot num 0 msg
  where
    internalize' :: Bot -> Int -> Int -> String -> IO Bot
    internalize' bot _ _ [] = return bot
    internalize' bot@(Bot socket params@(Parameter {stries=stries,slength=slen,plength=plen,randoms=randoms})
                      fugly@(Fugly {wne=wne})) num i imsg = do
      mm <- chooseWord wne $ words imsg
      s <- getSentence $ sentence fugly randoms stries slen plen mm
      nd <- insertWords fugly $ words s
      r <- Random.getStdRandom (Random.randomR (0, 2)) :: IO Int
      if i >= num then return bot
        else if r == 0 then hPutStrLn stdout ("> internalize: " ++ msg) >> internalize' (Bot socket params fugly{dict=nd}) num (i + 1) msg
          else hPutStrLn stdout ("> internalize: " ++ s) >> internalize' (Bot socket params fugly{dict=nd}) num (i + 1) s
    getSentence []     = return []
    getSentence (x:xs) = do
      ww <- x
      if null ww then getSentence xs
        else return ww
