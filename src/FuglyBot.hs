import Control.Concurrent.MVar
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Char
import Data.List
import qualified Data.Map as Map
import Network
import System.Environment (getArgs)
import System.IO
import System.IO.Error
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception
import Prelude

import FuglyLib

data Bot = Bot {
    socket :: Handle,
    params :: Parameter,
    fugly :: Fugly
    }

type Net = StateT Bot IO

data Parameter = Nick | Owner | UserCommands | RejoinKick | MaxChanMsg
               | ChatChannel | Topic | UnknownParam
               | Parameter {
                 nick        :: String,
                 owner       :: String,
                 fuglydir    :: FilePath,
                 wndir       :: FilePath,
                 usercmd     :: Bool,
                 rejoinkick  :: Int,
                 maxchanmsg  :: Int,
                 chatchannel :: String,
                 topic       :: String
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
    toEnum 6 = ChatChannel
    toEnum 7 = Topic
    toEnum 8 = UnknownParam
    fromEnum Nick           = 1
    fromEnum Owner          = 2
    fromEnum UserCommands   = 3
    fromEnum RejoinKick     = 4
    fromEnum MaxChanMsg     = 5
    fromEnum ChatChannel    = 6
    fromEnum Topic          = 7
    fromEnum UnknownParam   = 8
    enumFrom i = enumFromTo i UnknownParam
    enumFromThen i j = enumFromThenTo i j UnknownParam

readParam :: String -> Parameter
readParam a | (map toLower a) == "nick"            = Nick
readParam a | (map toLower a) == "owner"           = Owner
readParam a | (map toLower a) == "usercmd"         = UserCommands
readParam a | (map toLower a) == "usercommands"    = UserCommands
readParam a | (map toLower a) == "rejoinkick"      = RejoinKick
readParam a | (map toLower a) == "maxchanmsg"      = MaxChanMsg
readParam a | (map toLower a) == "chatchannel"     = ChatChannel
readParam a | (map toLower a) == "topic"           = Topic
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
    let channel  = args !! 4
    let nick     = cleanString isAscii (args !! 2)
    let owner    = args !! 3
    let fuglydir = args !! 5 :: FilePath
    let wndir    = args !! 6 :: FilePath
    socket <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering socket NoBuffering
    fugly <- initFugly fuglydir wndir
    bot <- newMVar (Bot socket (Parameter nick owner fuglydir wndir
                                False 10 460 channel []) fugly)
    return bot

run :: [String] -> StateT (MVar Bot) IO b
run args = do
    let channel = args !! 4
    let passwd  = args !! 7
    b <- get
    bot <- lift $ readMVar b
    let nick = (\(Bot _ (Parameter {nick=n}) _) -> n) bot
    let socket = (\(Bot s _ _) -> s) bot
    lift $ write socket "NICK" nick
    lift $ write socket "USER" (nick ++ " 0 * :user")
    lift $ privMsg bot "nickserv" ("IDENTIFY " ++ passwd)
    lift $ joinChannel socket "JOIN" [channel]
    listenIRC

listenIRC :: StateT (MVar Bot) IO b
listenIRC = do
    b <- get
    bot <- lift $ readMVar b
    let s = (\(Bot s _ _) -> s) bot
    l <- lift $ hGetLine s
    let ll = words l
    let lll = take 2 $ drop 1 ll
    lift $ putStrLn l
    if "PING :" `isPrefixOf` l then do
      lift (write s "PONG" (':' : drop 6 l)) >> listenIRC
      else if (length ll > 2) && (head lll) == "NICK" then do
      lift (do nb <- evalStateT (changeNick [] lll) b ; swapMVar b nb) >> listenIRC
        else do
             lift (forkIO (evalStateT (processLine $ words l) b)) >> listenIRC

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
    let channel      = if l > channelPos then args !! channelPos else "#lolbots"
    let fuglydirPos  = (maximum' $ elemIndices "-fuglydir" args) + 1
    let fuglydir     = if l > fuglydirPos then args !! fuglydirPos
                                            else "/var/lib/fuglybot"
    let wndirPos     = (maximum' $ elemIndices "-wndir" args) + 1
    let wndir        = if l > wndirPos then args !! wndirPos
                                            else "/usr/share/wordnet/dict/"
    let passwdPos    = (maximum' $ elemIndices "-passwd" args) + 1
    let passwd       = if l > passwdPos then args !! passwdPos else ""
    return (server : port : nick : owner : channel : fuglydir : wndir : passwd : [])
  where
    maximum' [] = 1000
    maximum' a  = maximum a

changeNick :: [String] -> [String] -> StateT (MVar Bot) IO Bot
changeNick [] [] = do
    b <- get
    bot <- lift $ readMVar b
    return bot
changeNick (x:_) [] = do
    b <- get
    bot <- lift $ readMVar b
    let socket = (\(Bot s _ _) -> s) bot
    let new = cleanString isAscii x
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
        | (x == "NICK") = return ("Nick change successful.") >>= privMsg bot o
                          >> return (Bot socket params{nick = drop 1 y} fugly)
        | otherwise     = return ("Nick change failed!") >>= privMsg bot o
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

changeParam :: Bot -> String -> String -> IO Bot
changeParam bot@(Bot _ p _) param value = do
    case (readParam param) of
      Nick         -> do nb <- newMVar bot ; evalStateT (changeNick (value : "" : []) []) nb
      Owner        -> return bot{params=p{owner=value}}
      UserCommands -> return bot{params=p{usercmd=readBool value}}
      RejoinKick   -> return bot{params=p{rejoinkick=read value}}
      MaxChanMsg   -> return bot{params=p{maxchanmsg=read value}}
      ChatChannel  -> return bot{params=p{chatchannel=value}}
      Topic        -> return bot{params=p{topic=value}}
      _            -> return bot
  where
    readBool a
      | (map toLower a) == "true"    = True
      | (map toLower a) == "yes"     = True
      | (map toLower a) == "on"      = True
      | (map toLower a) == "false"   = False
      | (map toLower a) == "no"      = False
      | (map toLower a) == "off"     = False
      | otherwise                    = True

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
    bot <- lift $ readMVar b
    let socket = (\(Bot s _ _) -> s) bot
    let nick = (\(Bot _ (Parameter {nick = n}) _) -> n) bot
    let rejoinkick = (\(Bot _ (Parameter {rejoinkick = r}) _) -> r) bot
    let bk = beenKicked nick line
    if (not $ null bk) then do lift (rejoinChannel socket bk rejoinkick)
      else if null msg then return ()
         else if chan == nick then do nb <- prvcmd bot ; _ <- lift $ swapMVar b nb ; return ()
           else if spokenTo nick msg then if null (tail msg) then return ()
                                          else if (head $ head $ tail msg) == '!'
                                            then do nb <- execCmd bot chan who (tail msg)
                                                    _ <- lift $ swapMVar b nb ; return ()
                                               else do nb <- reply bot chan who (tail msg)
                                                       _ <- lift $ swapMVar b nb ; return ()
             else do nb <- reply bot chan [] msg ; _ <- lift $ swapMVar b nb ; return ()
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
reply (Bot socket params fugly) chan nick msg = do
    _ <- if null chan then lift $ sentencePriv socket fugly 2 43 nick msg
         else if null nick then return [()]
           else lift $ sentenceReply socket fugly 1 43 chan nick msg
    n <- lift $ insertWords fugly True msg
    return (Bot socket params fugly{dict=n})

execCmd :: MonadTrans t => Bot -> String -> String -> [String] -> t IO Bot
execCmd bot _ _ [] = lift $ return bot
execCmd bot _ [] _ = lift $ return bot
execCmd bot [] _ _ = lift $ return bot
execCmd bot chan nick (x:xs) = do
    lift $ execCmd' bot
  where
    execCmd' :: Bot -> IO Bot
    execCmd' bot@(Bot socket params@(Parameter botnick owner fuglydir _
                                     usercmd rejoinkick maxchanmsg
                                     chatchan _)
                  fugly@(Fugly dict pgf wne aspell allow ban))
      | usercmd == False && nick /= owner = return bot
      | x == "!quit" =
        if nick == owner then case (length xs) of
          0 -> do stopFugly fuglydir fugly >>
                    write socket "QUIT" ":Bye" >> return bot
          _ -> do stopFugly fuglydir fugly >>
                    write socket "QUIT" (":" ++ unwords xs) >> return bot
        else return bot
      | x == "!save" = if nick == owner then catchIOError (saveDict fugly fuglydir)
                                       (const $ return ()) >> return bot else return bot
      | x == "!load" = if nick == owner then do
           (nd, na, nb) <- catchIOError (loadDict fuglydir) (const $ return (dict, [], []))
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
                   ++ show rejoinkick ++ "  maxchanmsg: " ++ show maxchanmsg
                   ++ "  chatchannel: " ++ chatchan) >> return bot
            _ -> replyMsg bot chan nick "Usage: !showparams" >> return bot
          else return bot
      | x == "!setparam" =
            if nick == owner then case (length xs) of
              2 -> changeParam bot (xs!!0) (xs!!1)
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
      | x == "!talk" = if nick == owner then
          if (length xs) > 2 then sentenceReply socket fugly 1 43 (xs!!0) (xs!!1) (tail xs)
                                  >> return bot
          else replyMsg bot chan nick "Usage: !talk <channel> <nick> <msg>" >> return bot
                     else return bot
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
            _ -> (sequence $ map (replyMsg bot chan nick)
                  (gfParseC pgf (unwords $ take 12 xs))) >> return bot
      -- | x == "!random" = case (length xs) of
      --       1 -> replyMsg bot chan nick (gfAll pgf (read (xs!!0))) >> return bot
      --       _ -> replyMsg bot chan nick "Usage: !random <number>" >> return bot
      | otherwise  = if nick == owner then replyMsg bot chan nick
                       ("Commands: !dict !wordlist !word !insertword !dropword "
                       ++ "!banword !allowword !name !insertname !closure !meet !parse "
                       ++ "!params !setparam !showparams !nick !join !part !talk !quit "
                       ++ "!readfile !load !save") >> return bot
                     else replyMsg bot chan nick ("Commands: !dict !word !wordlist !name "
                       ++ "!closure !meet !parse") >> return bot
    execCmd' bot = return bot

{-
   IRC messages are always lines of characters terminated with a CR-LF
   (Carriage Return - Line Feed) pair, and these messages SHALL NOT
   exceed 512 characters in length, counting all characters including
   the trailing CR-LF. Thus, there are 510 characters maximum allowed
   for the command and its parameters.  There is no provision for
   continuation of message lines.

   https://tools.ietf.org/html/rfc2812
-}

-- chanMsg :: Bot -> String -> String -> IO ()
-- chanMsg bot@(Bot socket (Parameter {maxchanmsg=mcm}) _) chan msg =
--   if length msg > mcm then do
--      write socket "PRIVMSG" (chan ++ " :" ++ (take mcm msg))
--      chanMsg bot chan (drop mcm msg)
--      else
--      chanMsg bot chan msg

sentenceReply :: Handle -> Fugly -> Int -> Int -> String -> String -> [String] -> IO [()]
sentenceReply h f n l chan nick m = do
    let msg = sentence f n l m False
    threadDelay 2000000
    _ <- sequence $ map (p h) msg
    sequence $ map d msg
  where
    p :: Handle -> IO String -> IO ()
    p h w = do
      ww <- w
      if null ww then return ()
        else if nick == chan then hPutStr h ("PRIVMSG " ++ (chan ++ " :" ++ ww) ++ "\r\n")
          else hPutStr h ("PRIVMSG " ++ (chan ++ " :" ++ nick ++ ": " ++ ww) ++ "\r\n")
    d :: IO String -> IO ()
    d w = do
      ww <- w
      if null ww then return ()
        else if nick == chan then hPutStr stdout ("> PRIVMSG " ++ (chan ++ " :" ++ ww) ++ "\n")
          else hPutStr stdout ("> PRIVMSG " ++ (chan ++ " :" ++ nick ++ ": " ++ ww) ++ "\n")

replyMsg :: Bot -> String -> String -> String -> IO ()
replyMsg bot@(Bot socket (Parameter {maxchanmsg=mcm}) _) chan nick msg
    | chan == nick   = if length msg > mcm then do
      write socket "PRIVMSG" (nick ++ " :" ++ (take mcm msg))
      replyMsg bot chan nick (drop mcm msg) else
        write socket "PRIVMSG" (nick ++ " :" ++ msg)
    | otherwise      = if length msg > mcm then do
      write socket "PRIVMSG" (chan ++ " :" ++ nick ++ ": " ++ (take mcm msg))
      replyMsg bot chan nick (drop mcm msg) else
        write socket "PRIVMSG" (chan ++ " :" ++ nick ++ ": " ++ msg)
replyMsg _ _ _ _ = return ()

sentencePriv :: Handle -> Fugly -> Int -> Int -> String -> [String] -> IO [()]
sentencePriv h f n l nick m = do
    let msg = sentence f n l m False
    threadDelay 2000000
    _ <- sequence $ map (p h) msg
    sequence $ map d msg
  where
    p :: Handle -> IO String -> IO ()
    p h w = do
            ww <- w
            if null ww then return ()
              else hPutStr h ("PRIVMSG " ++ (nick ++ " :" ++ ww) ++ "\r\n")
    d :: IO String -> IO ()
    d w = do
          ww <- w
          if null ww then return ()
            else hPutStr stdout ("> PRIVMSG " ++ (nick ++ " :" ++ ww) ++ "\n")

privMsg :: Bot -> String -> String -> IO ()
privMsg bot@(Bot socket (Parameter {maxchanmsg=mcm}) _) nick msg =
  if length msg > mcm then do
    write socket "PRIVMSG" (nick ++ " :" ++ (take mcm msg))
    privMsg bot nick (drop mcm msg)
    else
    write socket "PRIVMSG" (nick ++ " :" ++ msg)
privMsg _ _ _ = return ()

write :: Handle -> String -> String -> IO ()
write socket s msg = do
    hPutStr socket (s ++ " " ++ msg ++ "\r\n")
    hPutStr stdout ("> " ++ s ++ " " ++ msg ++ "\n")
    threadDelay 2000000

insertFromFile :: Bot -> FilePath -> IO Bot
insertFromFile (Bot s p fugly) file = do
    f <- readFile file
    n <- insertWords fugly False $ words f
    return (Bot s p fugly{dict=n})
