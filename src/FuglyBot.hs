import Data.Char (isDigit, isAscii, toLower)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Network
import System.Environment (getArgs, getProgName)
import System.Exit
import System.IO
import System.IO.Error
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception
import Text.Printf
import Prelude

import FuglyLib

data Bot = Bot {
    socket :: Handle,
    params :: Parameter,
    fugly :: Fugly
    }

data Parameter = Nick | Owner | UserCommands | RejoinKick | MaxChanMsg
               | ChatChannel | UnknownParam
               | Parameter {
                 nick        :: String,
                 owner       :: String,
                 fuglydir    :: FilePath,
                 wndir       :: FilePath,
                 usercmd     :: Bool,
                 rejoinkick  :: Int,
                 maxchanmsg  :: Int,
                 chatchannel :: String
                 }
               deriving (Eq, Ord, Show)

allParams = [Nick ..]

instance Enum Parameter where
    toEnum 1 = Nick
    toEnum 2 = Owner
    toEnum 3 = UserCommands
    toEnum 4 = RejoinKick
    toEnum 5 = MaxChanMsg
    toEnum 6 = ChatChannel
    toEnum 7 = UnknownParam
    fromEnum Nick           = 1
    fromEnum Owner          = 2
    fromEnum UserCommands   = 3
    fromEnum RejoinKick     = 4
    fromEnum MaxChanMsg     = 5
    fromEnum ChatChannel    = 6
    fromEnum UnknownParam   = 7
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
readParam _                                        = UnknownParam

main :: IO ()
main = do
    args <- cmdLine
    bracket (start args) stop (loop args)
  where
    stop         = do hClose . socket
    loop st args = catchIOError (run st args) (const $ return ())

start :: [String] -> IO Bot
start args = do
    let server   = args !! 0
    let port     = read $ args !! 1
    let channel  = args !! 4
    let nick     = cleanString isAscii (args !! 2)
    let owner    = args !! 3
    let fuglydir = args !! 5 :: FilePath
    let wndir    = args !! 6 :: FilePath
    socket <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering socket NoBuffering
    fugly <- initFugly fuglydir wndir
    return (Bot socket (Parameter nick owner fuglydir wndir False 10 460 channel) fugly)

run :: [String] -> Bot -> IO ()
run args bot@(Bot socket (Parameter nick _ _ _ _ _ _ _) fugly) = do
    let channel = args !! 4
    let passwd  = args !! 7
    write socket "NICK" nick
    write socket "USER" (nick ++ " 0 * :user")
    privMsg bot "nickserv" ("IDENTIFY " ++ passwd)
    joinChannel socket "JOIN" [channel]
    listenIRC bot
    return () :: IO ()

listenIRC :: Bot -> IO ()
listenIRC bot@(Bot socket params fugly) = do
    s <- hGetLine socket
    putStrLn s
    if ping s then pong s else do a <- processLine bot (words s) ; listenIRC a
  where
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write socket "PONG" (':' : drop 6 x) >> listenIRC bot

cmdLine :: IO [String]
cmdLine = do
    args <- getArgs
    prog <- getProgName
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

changeNick :: Bot -> [String] -> IO Bot
changeNick bot [] = return bot
changeNick bot@(Bot socket (Parameter nick owner _ _ _ _ _ _) fugly) (x:xs) = do
    let new = cleanString isAscii x
    write socket "NICK" new
    s <- hGetLine socket
    putStrLn s
    let line = words s
    if (length line) > 2 then testNick' bot new (take 2 (drop 1 line))
      else return "Something went wrong!" >>= privMsg bot owner >> return bot
  where
    testNick' :: Bot -> String -> [String] -> IO Bot
    testNick' bot@(Bot socket (Parameter nick owner fuglydir wndir usercmd
                               rejoinkick maxchanmsg chatchan) fugly)
      new line
        | (x == "NICK") && (drop 1 y) == new =
          return (Bot socket (Parameter new owner fuglydir wndir usercmd
                              rejoinkick maxchanmsg chatchan) fugly)
        | otherwise                          =
            return "Nick change failed!" >>= privMsg bot owner >> return bot
      where
        x = head line
        y = last line

joinChannel :: Handle -> String -> [String] -> IO ()
joinChannel h _  []    = return () :: IO ()
joinChannel h [] b     = joinChannel h "join" b
joinChannel h a (x:xs) = do
    if a == "JOIN" || a == "PART" then do
      write h a x
      joinChannel h a xs
        else return ()

changeParam :: Bot -> String -> String -> IO Bot
changeParam bot@(Bot socket (Parameter nick owner fuglydir wndir usercmd
                             rejoinkick maxchanmsg chatchan) fugly)
  param value = do
    case (readParam param) of
      Nick         -> changeNick bot (value : "" : [])
      Owner        -> return (Bot socket (Parameter nick value fuglydir wndir usercmd
                                          rejoinkick maxchanmsg chatchan) fugly)
      UserCommands -> return (Bot socket (Parameter nick owner fuglydir wndir
                                          (readBool value)
                                          rejoinkick maxchanmsg chatchan) fugly)
      RejoinKick   -> return (Bot socket (Parameter nick owner fuglydir wndir usercmd
                                          (read value) maxchanmsg chatchan) fugly)
      MaxChanMsg   -> return (Bot socket (Parameter nick owner fuglydir wndir usercmd
                                          rejoinkick (read value) chatchan) fugly)
      ChatChannel  -> return (Bot socket (Parameter nick owner fuglydir wndir usercmd
                                          rejoinkick maxchanmsg value) fugly)
      _            -> return bot
  where
    readBool a
      | a == "true"    = True
      | a == "yes"     = True
      | a == "on"      = True
      | a == "false"   = False
      | a == "no"      = False
      | a == "off"     = False
      | otherwise      = True

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
spokenTo n []         = False
spokenTo n b
    | c == n          = True
    | c == (n ++ ":") = True
    | otherwise       = False
  where
    c = head b

-- isPM :: String -> [String] -> Bool
-- isPM [] = False
-- isPM a b
--     | getChannel b == a = True
--     | otherwise         = False

beenKicked :: String -> [String] -> String
beenKicked _ [] = []
beenKicked n a
    | (head $ drop 1 a) == "KICK" = if (head $ drop 3 a) == n then getChannel a else []
    | otherwise                   = []

rejoinChannel :: Handle -> String -> Int -> IO ()
rejoinChannel h []   _  = return () :: IO ()
rejoinChannel h chan rk = do
    if rk == 0 then return () else rejoin' rk chan h >> return ()
  where
    rejoin' rk chan h = forkIO (threadDelay (rk * 1000000) >> hPrintf h "JOIN %s\r\n" chan)

processLine :: Bot -> [String] -> IO Bot
processLine bot [] = return bot
processLine bot@(Bot socket (Parameter nick owner fuglydir wndir
                             usercmd rejoinkick _ _) fugly) line
    | (not $ null $ beenKicked nick line) =
      rejoinChannel socket (beenKicked nick line) rejoinkick >> return bot
    | null msg          = return bot
    | chan == nick      = prvcmd
    | spokenTo nick msg = if null (tail msg) then return bot
                          else if (head $ head $ tail msg) == '!'
                               then evalCmd bot chan who (tail msg)
                               else reply bot chan who (tail msg)
    | otherwise         = reply bot chan [] msg
  where
    msg  = getMsg line
    who  = getNick line
    chan = getChannel line
    prvcmd = if (length $ head msg) > 0 then
               if (head $ head msg) == '!' then evalCmd bot who who msg
               else reply bot [] who msg
             else reply bot [] who msg

reply :: Bot -> String -> String -> [String] -> IO Bot
reply bot@(Bot socket params fugly@(dict, wne, markov, ban)) [] nick msg = do
    sentencePriv socket fugly 1 43 nick msg
    n <- insertWords fugly msg
    return (Bot socket params (n, wne, markov1 n markov 123 2 msg, ban))
reply bot@(Bot socket params fugly@(dict, wne, markov, ban)) chan [] msg = do
    n <- insertWords fugly msg
    return (Bot socket params (n, wne, markov1 n markov 123 1 msg, ban))
reply bot@(Bot socket params fugly@(dict, wne, markov, ban)) chan nick msg = do
    sentenceReply socket fugly 1 43 chan nick msg
    n <- insertWords fugly msg
    return (Bot socket params (n, wne, markov1 n markov 123 1 msg, ban))

evalCmd :: Bot -> String -> String -> [String] -> IO Bot
evalCmd bot@(Bot socket params@(Parameter _ owner fuglydir _ usercmd _ _ _)
             fugly@(dict, wne, markov, ban)) _ nick (x:xs)
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
        (nd, nm, nb) <- catchIOError (loadDict fuglydir) (const $ return (dict, [], []))
        return (Bot socket params (nd, wne, nm, nb))
                     else return bot
    | x == "!join" = if nick == owner then joinChannel socket "JOIN" xs >>
                                          return bot else return bot
    | x == "!part" = if nick == owner then joinChannel socket "PART" xs >>
                                          return bot else return bot
    | x == "!nick" = if nick == owner then changeNick bot xs else return bot
evalCmd bot@(Bot socket params@(Parameter _ owner _ _ _ _ _ _)
             fugly@(dict, wne, markov, ban)) chan nick (x:xs)
    | x == "!readfile" = if nick == owner then case (length xs) of
        1 -> catchIOError (insertFromFile bot (xs!!0)) (const $ return bot)
        _ -> replyMsg bot chan nick "Usage: !readfile <file>" >>
                                          return bot else return bot
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
            2 -> (wnGloss wne (xs!!0) (xs!!1)) >>= replyMsg bot chan nick >> return bot
            1 -> (wnGloss wne (xs!!0) []) >>= replyMsg bot chan nick >> return bot
            _ -> replyMsg bot chan nick "Usage: !dict <word> [part-of-speech]" >> return bot
    | x == "!wordlist" =
          let num = if read (xs!!0) > 100 then 100 else read (xs!!0) in
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
                    replyMsg bot chan nick ("Inserted word " ++ (xs!!0))
                    return (Bot socket params (ww, wne, markov, ban))
            _ -> replyMsg bot chan nick "Usage: !insertword <pos> <word>" >> return bot
                         else return bot
    | x == "!dropword" = if nick == owner then
          case (length xs) of
            1 -> replyMsg bot chan nick ("Dropped word " ++ (xs!!0)) >>
                 return (Bot socket params (dropWord dict (xs!!0), wne, markov, ban))
            _ -> replyMsg bot chan nick "Usage: !dropword <word>"
                 >> return bot
                         else return bot
    | x == "!banword" = if nick == owner then
          case (length xs) of
            1 -> replyMsg bot chan nick ("Banned word " ++ (xs!!0)) >>
                 return (Bot socket params (dropWord dict (xs!!0), wne, markov,
                                            nub $ ban ++ [(xs!!0)]))
            _ -> replyMsg bot chan nick "Usage: !banword <word>"
                 >> return bot
                         else return bot
    | x == "!unbanword" = if nick == owner then
          case (length xs) of
            1 -> return (Bot socket params (dict, wne, markov, nub $ delete (xs!!0) ban))
            _ -> replyMsg bot chan nick "Usage: !unbanword <word>"
                 >> return bot
                         else return bot
    | x == "!name" = case (length xs) of
            0 -> replyMsg bot chan nick "Usage: !name <name>" >> return bot
            _ -> replyMsg bot chan nick (listWordFull dict (xs!!0)) >> return bot
    | x == "!insertname" = if nick == owner then
        case (length xs) of
            1 -> do ww <- insertName fugly (xs!!0) [] []
                    return (Bot socket params (ww, wne, markov, ban))
            _ -> replyMsg bot chan nick "Usage: !insertname <name>" >> return bot
                           else return bot
    | x == "!closure" =
          case (length xs) of
            3 -> (wnClosure wne (xs!!0) (xs!!1) (xs!!2)) >>= replyMsg bot chan nick
                 >> return bot
            2 -> (wnClosure wne (xs!!0) (xs!!1) []) >>= replyMsg bot chan nick >> return bot
            1 -> (wnClosure wne (xs!!0) [] []) >>= replyMsg bot chan nick >> return bot
            _ -> replyMsg bot chan nick "Usage: !closure <word> [part-of-speech]"
                 >> return bot
    | x == "!meet" =
          case (length xs) of
            3 -> (wnMeet wne (xs!!0) (xs!!1) (xs!!2)) >>= replyMsg bot chan nick
                 >> return bot
            2 -> (wnMeet wne (xs!!0) (xs!!1) []) >>= replyMsg bot chan nick >> return bot
            _ -> replyMsg bot chan nick "Usage: !meet <word> <word> [part-of-speech]"
                 >> return bot
    | x == "!help" = if nick == owner then replyMsg bot chan nick
                       "Commands: !dict !wordlist !word !insertword !dropword !banword !unbanword !name !insertname !closure !meet !params !setparam !nick !join !part !quit !readfile !load !save" >> return bot
                     else replyMsg bot chan nick "Commands: !dict !word !wordlist !name !closure !meet" >> return bot
evalCmd bot _ _ _ = return bot

{-
   IRC messages are always lines of characters terminated with a CR-LF
   (Carriage Return - Line Feed) pair, and these messages SHALL NOT
   exceed 512 characters in length, counting all characters including
   the trailing CR-LF. Thus, there are 510 characters maximum allowed
   for the command and its parameters.  There is no provision for
   continuation of message lines.

   https://tools.ietf.org/html/rfc2812
-}

chanMsg :: Bot -> String -> String -> IO ()
chanMsg bot@(Bot socket (Parameter _ _ _ _ _ _ maxchanmsg _) fugly) chan msg =
  if length msg > maxchanmsg then do
     write socket "PRIVMSG" (chan ++ " :" ++ (take maxchanmsg msg))
     chanMsg bot chan (drop maxchanmsg msg)
     else
     chanMsg bot chan msg

sentenceReply :: Handle -> Fugly -> Int -> Int -> String -> String -> [String] -> IO [()]
sentenceReply h f n l chan nick m = do
    let msg = sentence f n l m
    threadDelay 2000000
    sequence $ map (p h) msg
    sequence $ map (d h) msg
  where
    p :: Handle -> String -> IO ()
    p h w = hPrintf h "PRIVMSG %s\r\n" (chan ++ " :" ++ nick ++ ": " ++ w)
    d :: Handle -> String -> IO ()
    d h w = printf    "> PRIVMSG %s\n" (chan ++ " :" ++ nick ++ ": " ++ w)

replyMsg :: Bot -> String -> String -> String -> IO ()
replyMsg bot@(Bot socket (Parameter _ _ _ _ _ _ maxchanmsg _) fugly) chan nick msg
    | chan == nick   = if length msg > maxchanmsg then do
      write socket "PRIVMSG" (nick ++ " :" ++ (take maxchanmsg msg))
      replyMsg bot chan nick (drop maxchanmsg msg) else
        write socket "PRIVMSG" (nick ++ " :" ++ msg)
    | otherwise      = if length msg > maxchanmsg then do
      write socket "PRIVMSG" (chan ++ " :" ++ nick ++ ": " ++ (take maxchanmsg msg))
      replyMsg bot chan nick (drop maxchanmsg msg) else
        write socket "PRIVMSG" (chan ++ " :" ++ nick ++ ": " ++ msg)

sentencePriv :: Handle -> Fugly -> Int -> Int -> String -> [String] -> IO [()]
sentencePriv h f n l nick m = do
    let msg = sentence f n l m
    threadDelay 2000000
    sequence $ map (p h) msg
    sequence $ map (d h) msg
  where
    p :: Handle -> String -> IO ()
    p h w = do hPrintf h "PRIVMSG %s\r\n" (nick ++ " :" ++ w)
    d :: Handle -> String -> IO ()
    d h w = do printf    "> PRIVMSG %s\n" (nick ++ " :" ++ w)

privMsg :: Bot -> String -> String -> IO ()
privMsg bot@(Bot socket (Parameter _ _ _ _ _ _ maxchanmsg _) fugly) nick msg =
  if length msg > maxchanmsg then do
    write socket "PRIVMSG" (nick ++ " :" ++ (take maxchanmsg msg))
    privMsg bot nick (drop maxchanmsg msg)
    else
    write socket "PRIVMSG" (nick ++ " :" ++ msg)

write :: Handle -> String -> String -> IO ()
write socket s msg = do
    hPrintf socket "%s %s\r\n" s msg
    printf         "> %s %s\n" s msg
    threadDelay 2000000

insertFromFile bot@(Bot socket params fugly@(dict, wne, markov, ban)) file = do
    f <- readFile file
    n <- insertWords fugly $ words f
    return (Bot socket params (n, wne, markov, ban))
