import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.Char
import Data.List
import qualified Data.Map.Lazy as Map
import Network
import Network.Socks5
import System.Environment
import System.IO
import System.IO.Error
import qualified System.Random as Random
import Text.Regex.Posix
import Prelude

import FuglyLib

data Bot = Bot {
    sock :: Handle,
    params :: Parameter,
    fugly :: Fugly
    }

data Parameter = Nick | Owner | UserCommands | RejoinKick | ThreadTime | MaxChanMsg
               | SentenceTries | SentenceLength | ParseLength | Learning | Autoname
               | AllowPM | Topic | Randoms | UnknownParam
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
                 autoname    :: Bool,
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
    toEnum 5 = MaxChanMsg
    toEnum 6 = SentenceTries
    toEnum 7 = SentenceLength
    toEnum 8 = ParseLength
    toEnum 9 = Learning
    toEnum 10 = Autoname
    toEnum 11 = AllowPM
    toEnum 12 = Topic
    toEnum 13 = Randoms
    toEnum 14 = UnknownParam
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
    fromEnum Autoname       = 10
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
readParam a | (map toLower a) == "autoname"        = Autoname
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
      hClose $ (\(Bot {sock=s}) -> s) b
    loop :: [String] -> MVar Bot -> IO ()
    loop args bot = do catchIOError (evalStateT (run args) bot) (const $ return ())

start :: [String] -> IO (MVar Bot)
start args = do
    let server   = args !! 0
    let port     = read $ args !! 1 :: Integer
    let nick'    = cleanStringWhite isAscii (args !! 2)
    let owner'   = args !! 3
    let topic'   = args !! 5
    let fdir     = args !! 6 :: FilePath
    let wndir    = args !! 7 :: FilePath
    let gfdir    = args !! 8 :: FilePath
    let socks5   = args !! 10
    let socks5add  = takeWhile (\x -> x /= ':') socks5
    let socks5port = read (tail $ dropWhile (\x -> x /= ':') socks5) :: Integer
    s <- if null socks5 then connectTo server (PortNumber (fromIntegral port)) else
           socksConnectTo socks5add (PortNumber (fromIntegral socks5port)) server (PortNumber (fromIntegral port))
    hSetBuffering s NoBuffering
    f <- initFugly fdir wndir gfdir topic'
    let b = (Bot s (Parameter nick' owner' fdir False
             10 400 100 5 5  True True False topic' 0) f)
    bot <- newMVar b
    write s "NICK" nick'
    write s "USER" (nick' ++ " 0 * :user")
    return bot

run :: [String] -> StateT (MVar Bot) IO b
run args = do
    b <- get
    bot <- lift $ readMVar b
    let s = (\(Bot s' _ _) -> s') bot
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
                lift (catch (evalStateT (processLine $ words l) b >> return ())
                      (\e -> do let err = show (e :: SomeException)
                                hPutStrLn stderr ("process line: " ++ err)
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

changeNick :: [String] -> [String] -> StateT (MVar Bot) IO Bot
changeNick (_:_) (_:_) = do
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
    let nick' = (\(Bot _ (Parameter {nick = n}) _) -> n) bot
    lift $ testNick bot nick' line
  where
    testNick :: Bot -> String -> [String] -> IO Bot
    testNick bot [] _ = return bot
    testNick bot _ [] = return bot
    testNick bot@(Bot socket p@(Parameter {owner = o}) f) old line'
        | (x == "NICK") = return ("Nick change successful.") >>= replyMsg bot o []
                          >> return (Bot socket p{nick = drop 1 y} f)
        | otherwise     = return ("Nick change failed!") >>= replyMsg bot o []
                          >> return (Bot socket p{nick = old} f)
      where
        x = head line'
        y = last line'
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
changeParam bot@(Bot _ p@(Parameter {fuglydir=fd, topic=t}) f) chan nick' param value = do
    case (readParam param) of
      Nick           -> do nb <- newMVar bot ; evalStateT (changeNick (value : "" : []) []) nb
      Owner          -> replyMsg' value                 "Owner"               >>= (\x -> return bot{params=p{owner=x}})
      UserCommands   -> replyMsg' (readBool value)      "User commands"       >>= (\x -> return bot{params=p{usercmd=x}})
      RejoinKick     -> replyMsg' (readInt 1 4096 value) "Rejoin kick time"   >>= (\x -> return bot{params=p{rejoinkick=x}})
      MaxChanMsg     -> replyMsg' (readInt 0 450 value) "Max channel message" >>= (\x -> return bot{params=p{maxchanmsg=x}})
      SentenceTries  -> replyMsg' (readInt 0 4096 value) "Sentence tries"     >>= (\x -> return bot{params=p{stries=x}})
      SentenceLength -> replyMsg' (readInt 0 256 value) "Sentence length"     >>= (\x -> return bot{params=p{slength=x}})
      ParseLength    -> replyMsg' (readInt 2 256 value) "Parse length"        >>= (\x -> return bot{params=p{plength=x}})
      Learning       -> replyMsg' (readBool value)     "Learning"             >>= (\x -> return bot{params=p{learning=x}})
      Autoname       -> replyMsg' (readBool value)     "Autoname"             >>= (\x -> return bot{params=p{autoname=x}})
      AllowPM        -> replyMsg' (readBool value)     "Allow PM"             >>= (\x -> return bot{params=p{allowpm=x}})
      Topic          -> do (d, a, b) <- catchIOError (loadDict fd value) (const $ return (Map.empty, [], []))
                           _ <- saveDict f fd t
                           replyMsg' value "Topic" >>= (\x -> return bot{params=p{topic=x},fugly=f{dict=d,allow=a,ban=b}})
      Randoms        -> replyMsg' (readInt 0 100 value) "Randoms"             >>= (\x -> return bot{params=p{randoms=x}})
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

rejoinChannel :: Handle -> String -> Int -> IO ()
rejoinChannel _ []   _  = return () :: IO ()
rejoinChannel h chan rk = do
    if rk == 0 then return () else rejoin' rk chan h >> return ()
  where
    rejoin' rk' chan' h' = forkIO (threadDelay (rk' * 1000000) >>
                                   hPutStr h' ("JOIN " ++ chan' ++ "\r\n"))

processLine :: [String] -> StateT (MVar Bot) IO ()
processLine [] = return ()
processLine line = do
    b <- get
    bot <- lift $ readMVar b
    let socket = (\(Bot s _ _) -> s) bot
    let nick' = (\(Bot _ (Parameter {nick = n}) _) -> n) bot
    let rk = (\(Bot _ (Parameter {rejoinkick = r}) _) -> r) bot
    let bk = beenKicked nick' line
    if (not $ null bk) then do lift (rejoinChannel socket bk rk >> swapMVar b bot >> return ())
      else if null msg then lift $ swapMVar b bot >> return ()
         else if chan == nick' then do nb <- prvcmd bot ; lift $ swapMVar b nb >> return ()
           else if spokenTo nick' msg then if null (tail msg) then lift $ swapMVar b bot >> return ()
                                          else if (head $ head $ tail msg) == '!'
                                            then do nb <- execCmd bot chan who (tail msg)
                                                    lift $ swapMVar b nb >> return ()
                                               else do nb <- reply bot chan who (tail msg)
                                                       lift $ swapMVar b nb >> return ()
             else do nb <- reply bot chan [] msg ; lift $ swapMVar b nb >> return ()
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
reply bot@(Bot socket p@(Parameter botnick owner' _ _ _ _ stries'
                         slen plen learning' autoname' allowpm' _ randoms')
           f@(Fugly _ pgf' wne' _ _ _)) chan nick' msg = do
    let mmsg = if null $ head msg then msg
                 else case fLast ' ' $ head msg of
                   ':' -> tail msg
                   ',' -> tail msg
                   _   -> msg
    fmsg <- lift $ asReplaceWords f $ map cleanString mmsg
    let parse = gfParseBool pgf' plen $ unwords fmsg
    mm <- lift $ chooseWord wne' fmsg
    r <- lift $ Random.getStdRandom (Random.randomR (1, 3 :: Int))
    _ <- lift $ forkIO (if null chan then
                          if allowpm' then
                            sentenceReply socket f nick' [] randoms' stries' slen plen r mm
                          else return ()
                        else if null nick' then
                               if length msg > 2 && (unwords msg) =~ botnick then
                                 sentenceReply socket f chan chan randoms' stries' slen plen r mm
                               else return ()
                             else sentenceReply socket f chan nick' randoms' stries' slen plen r mm)
    if ((nick' == owner' && null chan) || parse) && learning' then do
      nd <- lift $ insertWords f autoname' fmsg
      lift $ hPutStrLn stdout ">parse<"
      return (Bot socket p f{dict=nd}) else
      return bot
reply bot _ _ _ = return bot

execCmd :: MonadTrans t => Bot -> String -> String -> [String] -> t IO Bot
execCmd b _ _ [] = lift $ return b
execCmd b _ [] _ = lift $ return b
execCmd b [] _ _ = lift $ return b
execCmd b chan nick' (x:xs) = do
    lift $ execCmd' b
  where
    execCmd' :: Bot -> IO Bot
    execCmd' bot@(Bot socket p@(Parameter botnick owner' fdir
                                     usercmd' rkick maxcmsg
                                     stries' slen plen learn autoname'
                                     allowpm' topic' randoms')
                  f@(Fugly dict' pgf' wne' aspell' allow' ban'))
      | usercmd' == False && nick' /= owner' = return bot
      | x == "!quit" =
        if nick' == owner' then case (length xs) of
          0 -> do stopFugly fdir f topic' >>
                    write socket "QUIT" ":Bye" >> return bot
          _ -> do stopFugly fdir f topic' >>
                    write socket "QUIT" (":" ++ unwords xs) >> return bot
        else return bot
      | x == "!save" = if nick' == owner' then catchIOError (saveDict f fdir topic')
                                       (const $ return ()) >> replyMsg bot chan nick' "Saved dict file!"
                                             >> return bot else return bot
      | x == "!load" = if nick' == owner' then do
           (nd, na, nb) <- catchIOError (loadDict fdir topic') (const $ return (dict', [], []))
           replyMsg bot chan nick' "Loaded dict file!"
           return (Bot socket p (Fugly nd pgf' wne' aspell' na nb))
                       else return bot
      | x == "!join" = if nick' == owner' then joinChannel socket "JOIN" xs >>
                                             return bot else return bot
      | x == "!part" = if nick' == owner' then joinChannel socket "PART" xs >>
                                             return bot else return bot
      | x == "!nick" = if nick' == owner' then do nb <- newMVar bot ; evalStateT (changeNick xs []) nb else return bot
      | x == "!readfile" = if nick' == owner' then case (length xs) of
          1 -> catchIOError (insertFromFile bot (xs!!0)) (const $ return bot)
          _ -> replyMsg bot chan nick' "Usage: !readfile <file>" >>
               return bot else return bot
      | x == "!showparams" =
          if nick' == owner' then case (length xs) of
            0 -> replyMsg bot chan nick' ("nick: " ++ botnick ++ "  owner: " ++ owner' ++
                   "  usercommands: " ++ show usercmd' ++ "  rejoinkick: "
                   ++ show rkick ++ "  maxchanmsg: " ++ show maxcmsg
                   ++ "  sentencetries: " ++ show stries' ++ "  sentencelength: " ++ show slen ++ "  parselength: " ++ show plen
                   ++ "  learning: " ++ show learn ++ "  autoname: " ++ show autoname' ++ "  allowpm: " ++ show allowpm'
                   ++ "  topic: " ++ topic' ++ "  randoms: " ++ show randoms') >> return bot
            _ -> replyMsg bot chan nick' "Usage: !showparams" >> return bot
          else return bot
      | x == "!setparam" =
            if nick' == owner' then case (length xs) of
              2 -> changeParam bot chan nick' (xs!!0) (xs!!1)
              _ -> replyMsg bot chan nick' "Usage: !setparam <parameter> <value>" >> return bot
            else return bot
      | x == "!params" =
              if nick' == owner' then replyMsg bot chan nick' (init (concat $ map (++ " ")
                                      $ map show $ init allParams)) >> return bot
              else return bot
      | x == "!dict" =
          case (length xs) of
            2 -> (dictLookup f (xs!!0) (xs!!1)) >>= replyMsg bot chan nick' >> return bot
            1 -> (dictLookup f (xs!!0) []) >>= replyMsg bot chan nick' >> return bot
            _ -> replyMsg bot chan nick' "Usage: !dict <word> [part-of-speech]" >> return bot
      | x == "!wordlist" =
          let num = if read (xs!!0) > (100 :: Integer) then 100 :: Int else read (xs!!0) in
          case (length xs) of
            1 -> replyMsg bot chan nick' (unwords $ listWordsCountSort2 dict' num)
                 >> replyMsg bot chan nick' ("Total word count: " ++ (show $ Map.size dict'))
                 >> return bot
            _ -> replyMsg bot chan nick' "Usage: !wordlist <number>" >> return bot
      | x == "!word" = case (length xs) of
            1 -> replyMsg bot chan nick' (listWordFull dict' (xs!!0)) >> return bot
            _ -> replyMsg bot chan nick' "Usage: !word <word>" >> return bot
      | x == "!insertword" = if nick' == owner' then
          case (length xs) of
            2 -> do ww <- insertWordRaw f (xs!!1) [] [] (xs!!0)
                    replyMsg bot chan nick' ("Inserted word " ++ (xs!!1))
                    return (Bot socket p f{dict=ww})
            _ -> replyMsg bot chan nick' "Usage: !insertword <pos> <word>" >> return bot
                         else return bot
      | x == "!dropword" = if nick' == owner' then
          case (length xs) of
            1 -> replyMsg bot chan nick' ("Dropped word " ++ (xs!!0)) >>
                 return (Bot socket p f{dict=dropWord dict' (xs!!0)})
            _ -> replyMsg bot chan nick' "Usage: !dropword <word>"
                 >> return bot
                         else return bot
      | x == "!ageword" = if nick' == owner' then
          case (length xs) of
            2 -> replyMsg bot chan nick' ("Aged word " ++ (xs!!0)) >>
                 return (Bot socket p f{dict=ageWord dict' (xs!!0) (read (xs!!1))})
            _ -> replyMsg bot chan nick' "Usage: !ageword <word> <number>"
                 >> return bot
                         else return bot
      | x == "!agewords" = if nick' == owner' then
          case (length xs) of
            1 -> replyMsg bot chan nick' ("Aged all words...") >>
                 return (Bot socket p f{dict=ageWords dict' (read (xs!!0))})
            _ -> replyMsg bot chan nick' "Usage: !agewords <number>"
                 >> return bot
                         else return bot
      | x == "!cleanwords" = if nick' == owner' then
          case (length xs) of
            0 -> do nd <- fixWords aspell' dict'
                    replyMsg bot chan nick' ("Cleaned some words...")
                    return (Bot socket p f{dict=nd})
            _ -> replyMsg bot chan nick' "Usage: !cleanwords"
                 >> return bot
                         else return bot
      | x == "!banword" = if nick' == owner' then
          case (length xs) of
            2 -> if (xs!!0) == "add" then
                    replyMsg bot chan nick' ("Banned word " ++ (xs!!1)) >>
                    return (Bot socket p (Fugly (dropWord dict' (xs!!1)) pgf' wne' aspell'
                                               allow' (nub $ ban' ++ [(xs!!1)])))
                 else if (xs!!0) == "delete" then
                    replyMsg bot chan nick' ("Unbanned word " ++ (xs!!1)) >>
                    return (Bot socket p (Fugly dict' pgf' wne' aspell' allow'
                                               (nub $ delete (xs!!1) ban')))
                 else replyMsg bot chan nick' "Usage: !banword <list|add|delete> <word>"
                      >> return bot
            1 -> if (xs!!0) == "list" then
                    replyMsg bot chan nick' ("Banned word list: " ++ unwords ban')
                    >> return bot
                 else replyMsg bot chan nick' "Usage: !banword <list|add|delete> <word>"
                      >> return bot
            _ -> replyMsg bot chan nick' "Usage: !banword <list|add|delete> <word>"
                 >> return bot
                         else return bot
      | x == "!allowword" = if nick' == owner' then
          case (length xs) of
            2 -> if (xs!!0) == "add" then
                    replyMsg bot chan nick' ("Allowed word " ++ (xs!!1)) >>
                    return (Bot socket p (Fugly dict' pgf' wne' aspell'
                                               (nub $ allow' ++ [(xs!!1)]) ban'))
                 else if (xs!!0) == "delete" then
                    replyMsg bot chan nick' ("Unallowed word " ++ (xs!!1)) >>
                    return (Bot socket p (Fugly dict' pgf' wne' aspell'
                                               (nub $ delete (xs!!1) allow') ban'))
                 else replyMsg bot chan nick' "Usage: !allowword <list|add|delete> <word>"
                      >> return bot
            1 -> if (xs!!0) == "list" then
                    replyMsg bot chan nick' ("Allowed word list: " ++ unwords allow')
                    >> return bot
                 else replyMsg bot chan nick' "Usage: !allowword <list|add|delete> <word>"
                      >> return bot
            _ -> replyMsg bot chan nick' "Usage: !allowword <list|add|delete> <word>"
                 >> return bot
                         else return bot
      | x == "!namelist" =
          let num = if read (xs!!0) > (100 :: Integer) then 100 :: Int else read (xs!!0) in
          case (length xs) of
            1 -> replyMsg bot chan nick' (unwords $ listNamesCountSort2 dict' num)
                 >> replyMsg bot chan nick' ("Total name count: " ++ (show $ length $
                                             filter (\x' -> wordIs x' == "name") $ Map.elems dict'))
                 >> return bot
            _ -> replyMsg bot chan nick' "Usage: !namelist <number>" >> return bot
      | x == "!name" = case (length xs) of
            1 -> replyMsg bot chan nick' (listWordFull dict' (xs!!0)) >> return bot
            _ -> replyMsg bot chan nick' "Usage: !name <name>" >> return bot
      | x == "!insertname" = if nick' == owner' then
          case (length xs) of
            1 -> do ww <- insertName f (xs!!0) [] []
                    replyMsg bot chan nick' ("Inserted name " ++ (xs!!0))
                    return (Bot socket p f{dict=ww})
            _ -> replyMsg bot chan nick' "Usage: !insertname <name>" >> return bot
                           else return bot
      | x == "!internalize" = if nick' == owner' then
                                if length xs > 1 then do replyMsg bot chan nick' ("Internalizing...")
                                                         internalize bot (read (xs!!0)) $ unwords $ tail xs
                                else
                                  replyMsg bot chan nick' "Usage: !internalize <tries> <msg>" >> return bot
                           else return bot
      | x == "!talk" = if nick' == owner' then
          if length xs > 2 then sentenceReply socket f (xs!!0) (xs!!1) randoms' stries' slen plen 2 (drop 2 xs)
                                  >> return bot
          else replyMsg bot chan nick' "Usage: !talk <channel> <nick> <msg>" >> return bot
                     else return bot
      | x == "!raw" = if nick' == owner' then
          if (length xs) > 0 then write socket (xs!!0)(unwords $ tail xs)
                                  >> return bot
          else replyMsg bot chan nick' "Usage: !raw <msg>" >> return bot
                     else return bot
      | x == "!related" = case (length xs) of
            3 -> (wnRelated wne' (xs!!0) (xs!!1) (xs!!2)) >>= replyMsg bot chan nick' >> return bot
            2 -> (wnRelated wne' (xs!!0) (xs!!1) []) >>= replyMsg bot chan nick' >> return bot
            1 -> (wnRelated wne' (xs!!0) [] []) >>= replyMsg bot chan nick' >> return bot
            _ -> replyMsg bot chan nick' "Usage: !related <word> [form] [part-of-speech]"
                 >> return bot
      | x == "!closure" = case (length xs) of
            3 -> (wnClosure wne' (xs!!0) (xs!!1) (xs!!2)) >>= replyMsg bot chan nick'
                 >> return bot
            2 -> (wnClosure wne' (xs!!0) (xs!!1) []) >>= replyMsg bot chan nick' >> return bot
            1 -> (wnClosure wne' (xs!!0) [] []) >>= replyMsg bot chan nick' >> return bot
            _ -> replyMsg bot chan nick' "Usage: !closure <word> [part-of-speech]"
                 >> return bot
      | x == "!meet" = case (length xs) of
            3 -> (wnMeet wne' (xs!!0) (xs!!1) (xs!!2)) >>= replyMsg bot chan nick'
                 >> return bot
            2 -> (wnMeet wne' (xs!!0) (xs!!1) []) >>= replyMsg bot chan nick' >> return bot
            _ -> replyMsg bot chan nick' "Usage: !meet <word> <word> [part-of-speech]"
                 >> return bot
      | x == "!parse" = case (length xs) of
            0 -> replyMsg bot chan nick' "Usage: !parse <sentence>" >> return bot
            _ -> (mapM (replyMsg bot chan nick') $ take 3
                  (gfParseC pgf' (unwords $ take 12 xs))) >> return bot
      | x == "!gfcats" = case (length xs) of
            0 -> return (unwords $ gfCategories pgf') >>= replyMsg bot chan nick' >> return bot
            _ -> replyMsg bot chan nick' "Usage: !gfcats" >> return bot
      -- | x == "!random" = case (length xs) of
      --       1 -> replyMsg bot chan nick' (gfAll pgf' (read (xs!!0))) >> return bot
      --       _ -> replyMsg bot chan nick' "Usage: !random <number>" >> return bot
      | x == "!test" = if nick' == owner' then
            replyMsg bot chan nick' (unwords $ map show $ take 750 $ iterate succ (0 :: Int)) >> return bot
            else return bot
      | otherwise  = if nick' == owner' then replyMsg bot chan nick'
                       ("Commands: !dict !word !wordlist !insertword !dropword "
                       ++ "!banword !allowword !namelist !name !insertname !closure !meet !parse "
                       ++ "!related !gfcats !ageword(s) !cleanwords !internalize "
                       ++ "!params !setparam !showparams !nick !join !part !talk !raw "
                       ++ "!quit !readfile !load !save") >> return bot
                     else replyMsg bot chan nick' ("Commands: !dict !word !wordlist !name "
                       ++ "!closure !meet !parse !related !gfcats") >> return bot
    execCmd' bot = return bot

sentenceReply :: Handle -> Fugly -> String -> String -> Int -> Int -> Int -> Int -> Int -> [String] -> IO ()
sentenceReply h fugly' chan nick' randoms' stries' slen plen num m = do
    x <- f (sentence fugly' randoms' stries' slen plen m) [] num 0
    let ww = unwords x
    if null ww then return ()
      else if null nick' then hPutStr h ("PRIVMSG " ++ (chan ++ " :" ++ ww) ++ "\r\n") >>
                              hPutStr stdout ("> PRIVMSG " ++ (chan ++ " :" ++ ww) ++ "\n")
        else if nick' == chan then hPutStr h ("PRIVMSG " ++ (chan ++ " :" ++ ww) ++ "\r\n") >>
                                   hPutStr stdout ("> PRIVMSG " ++ (chan ++ " :" ++ ww) ++ "\n")
           else hPutStr h ("PRIVMSG " ++ (chan ++ " :" ++ nick' ++ ": " ++ ww) ++ "\r\n") >>
                hPutStr stdout ("> PRIVMSG " ++ (chan ++ " :" ++ nick' ++ ": " ++ ww) ++ "\n")
  where
    f :: [IO String] -> [String] -> Int -> Int -> IO [String]
    f []     a _ _ = return a
    f (x:xs) a n i = do
      xx <- x
      if i >= n then return a
        else if null xx then f xs a n i
        else f xs ([xx ++ " "] ++ a) n (i + 1)

replyMsg :: Bot -> String -> String -> String -> IO ()
replyMsg bot@(Bot socket (Parameter {maxchanmsg=mcm}) _) chan nick' msg
    | null nick'      = if length msg > mcm then do
      write socket "PRIVMSG" (chan ++ " :" ++ (take mcm msg))
      threadDelay 2000000
      replyMsg bot chan [] (drop mcm msg) else
        write socket "PRIVMSG" (chan ++ " :" ++ msg)
    | chan == nick'   = if length msg > mcm then do
      write socket "PRIVMSG" (nick' ++ " :" ++ (take mcm msg))
      threadDelay 2000000
      replyMsg bot chan nick' (drop mcm msg) else
        write socket "PRIVMSG" (nick' ++ " :" ++ msg)
    | otherwise      = if length msg > mcm then do
      write socket "PRIVMSG" (chan ++ " :" ++ nick' ++ ": " ++ (take mcm msg))
      threadDelay 2000000
      replyMsg bot chan nick' (drop mcm msg) else
        write socket "PRIVMSG" (chan ++ " :" ++ nick' ++ ": " ++ msg)
replyMsg _ _ _ _ = return ()

write :: Handle -> String -> String -> IO ()
write _ [] _  = return ()
write _ _ []  = return ()
write sock' s msg = do
    hPutStr sock'  (s ++ " " ++ msg ++ "\r\n")
    hPutStr stdout ("> " ++ s ++ " " ++ msg ++ "\n")

insertFromFile :: Bot -> FilePath -> IO Bot
insertFromFile b@(Bot _ _ _) [] = return b
insertFromFile (Bot s p@(Parameter {autoname=a}) fugly') file = do
    f <- readFile file
    fmsg <- asReplaceWords fugly' $ map cleanString $ words f
    n <- insertWords fugly' a fmsg
    return (Bot s p fugly'{dict=n})
insertFromFile b _ = return b

internalize :: Bot -> Int -> String -> IO Bot
internalize b 0 _   = return b
internalize b _ []  = return b
internalize b n msg = internalize' b n 0 msg
  where
    internalize' :: Bot -> Int -> Int -> String -> IO Bot
    internalize' bot _ _ [] = return bot
    internalize' bot@(Bot socket p@(Parameter {autoname=aname,stries=st,slength=slen,plength=plen,randoms=rands})
                      f@(Fugly {wne=wne'})) num i imsg = do
      mm <- chooseWord wne' $ words imsg
      s <- getSentence $ sentence f rands st slen plen mm
      nd <- insertWords f aname $ words s
      r <- Random.getStdRandom (Random.randomR (0, 2)) :: IO Int
      if i >= num then return bot
        else if r == 0 then hPutStrLn stdout ("> internalize: " ++ msg) >> internalize' (Bot socket p f{dict=nd}) num (i + 1) msg
          else hPutStrLn stdout ("> internalize: " ++ s) >> internalize' (Bot socket p f{dict=nd}) num (i + 1) s
    internalize' bot _ _ _ = return bot
    getSentence []     = return []
    getSentence (x:xs) = do
      ww <- x
      if null ww then getSentence xs
        else return ww
