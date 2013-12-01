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

data Parameter = Nick | Owner | RejoinKick | MaxChanLines | ChatChannel
               | UnknownParam
               | Parameter {
                 nick :: String,
                 owner :: String,
                 fuglydir :: FilePath,
                 wndir :: FilePath,
                 rejoinkick :: Int,
                 maxchanlines :: Int,
                 chatchannel :: String
                 }
               deriving (Eq, Ord, Show)

allParams = [Nick ..]

instance Enum Parameter where
    toEnum 1 = Nick
    toEnum 2 = Owner
    toEnum 3 = RejoinKick
    toEnum 4 = MaxChanLines
    toEnum 5 = ChatChannel
    toEnum 6 = UnknownParam
    fromEnum Nick           = 1
    fromEnum Owner          = 2
    fromEnum RejoinKick     = 3
    fromEnum MaxChanLines   = 4
    fromEnum ChatChannel    = 5
    fromEnum UnknownParam   = 6
    enumFrom i = enumFromTo i UnknownParam
    enumFromThen i j = enumFromThenTo i j UnknownParam

readParam :: String -> Parameter
readParam a | (map toLower a) == "nick"            = Nick
readParam a | (map toLower a) == "owner"           = Owner
readParam a | (map toLower a) == "rejoinkick"      = RejoinKick
readParam a | (map toLower a) == "maxchanlines"    = MaxChanLines
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
    return (Bot socket (Parameter nick owner fuglydir wndir 10 2 channel) fugly)

run :: [String] -> Bot -> IO ()
run args bot@(Bot socket (Parameter nick _ _ _ _ _ _) fugly) = do
    let channel = args !! 4
    let passwd  = args !! 7
    write socket "NICK" nick
    write socket "USER" (nick ++ " 0 * :user")
    privMsg socket "nickserv" ("IDENTIFY " ++ passwd)
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
changeNick bot@(Bot socket (Parameter nick owner _ _ _ _ _) fugly) (x:xs) = do
    let new = cleanString isAscii x
    write socket "NICK" new
    s <- hGetLine socket
    putStrLn s
    let line = words s
    if (length line) > 2 then testNick' bot new (take 2 (drop 1 line))
      else return "Something went wrong!" >>= privMsg socket owner >> return bot
  where
    testNick' :: Bot -> String -> [String] -> IO Bot
    testNick' bot@(Bot socket (Parameter nick owner fuglydir wndir rejoinkick maxchanlines chatchan) fugly)
      new line
        | (x == "NICK") && (drop 1 y) == new =
          return (Bot socket (Parameter new owner fuglydir wndir rejoinkick maxchanlines chatchan) fugly)
        | otherwise                          =
            return "Nick change failed!" >>= privMsg socket owner >> return bot
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
changeParam bot@(Bot socket (Parameter nick owner fuglydir wndir
                             rejoinkick maxchanlines chatchan) fugly)
  param value = do
    case (readParam param) of
      Nick         -> changeNick bot (value : "" : [])
      Owner        -> return (Bot socket (Parameter nick value fuglydir wndir
                                          rejoinkick maxchanlines chatchan) fugly)
      RejoinKick   -> return (Bot socket (Parameter nick owner fuglydir wndir (read value)
                                          maxchanlines chatchan) fugly)
      MaxChanLines -> return (Bot socket (Parameter nick owner fuglydir wndir rejoinkick
                                          (read value) chatchan) fugly)
      ChatChannel  -> return (Bot socket (Parameter nick owner fuglydir wndir rejoinkick
                                          maxchanlines value) fugly)
      _            -> return bot

getMsg :: [String] -> [String]
getMsg [] = []
getMsg msg
    | (head $ drop 1 msg) == "PRIVMSG" = (drop 1 (msg!!3)) : (drop 4 msg)
    | otherwise                        = []

getNick :: [String] -> String
getNick = drop 1 . takeWhile (/= '!') . head

getChannel :: [String] -> String
getChannel = head . drop 2

spokenTo :: String -> [String] -> Bool
spokenTo n []         = False
spokenTo n b
    | c == n          = True
    | c == (n ++ ":") = True
    | otherwise       = False
  where
    c = (head b)

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
processLine bot@(Bot socket (Parameter nick owner fuglydir wndir rejoinkick _ _) fugly) line
    | (not $ null $ beenKicked nick line) =
      rejoinChannel socket (beenKicked nick line) rejoinkick >> return bot
    | null msg = return bot
    | chan == nick = if (head $ head msg) == '!' then evalCmd bot who who msg
                     else reply bot [] who msg
    | spokenTo nick msg = if null (tail msg) then return bot
                          else if (head $ head $ tail msg) == '!'
                               then evalCmd bot chan who (tail msg)
                               else reply bot chan who (tail msg)
    | otherwise = reply bot chan [] msg
    -- | otherwise           = return bot
  where
    msg  = getMsg line
    who  = getNick line
    chan = getChannel line

reply :: Bot -> String -> String -> [String] -> IO Bot
reply bot@(Bot socket params fugly) [] who msg = do
    return bot
reply bot@(Bot socket params fugly@(dict, wne)) chan [] msg = do
    -- store and process msg
    -- evaluate msg for spelling, grammar, etc
    -- formulate response
    n <- insertWords fugly msg
    return (Bot socket params (n, wne))
reply bot@(Bot socket params fugly) chan who msg = do
    -- store and process msg
    -- evaluate msg for spelling, grammar, etc
    -- decide whether to respond
    -- formulate response
    return bot

evalCmd :: Bot -> String -> String -> [String] -> IO Bot
evalCmd bot@(Bot socket params@(Parameter _ owner fuglydir _ _ _ _) fugly@(dict, wne)) _ who (x:xs)
    | x == "!quit" =
      if who == owner then case (length xs) of
        0 -> do stopFugly fuglydir fugly >>
                  write socket "QUIT" ":Bye" >> return bot
        _ -> do stopFugly fuglydir fugly >>
                  write socket "QUIT" (":" ++ unwords xs) >> return bot
      else return bot
    | x == "!save" = if who == owner then saveDict dict fuglydir
                                          >> return bot else return bot
    | x == "!load" = if who == owner then do
        nd <- loadDict fuglydir
        return (Bot socket params (nd, wne))
                     else return bot
    | x == "!join" = if who == owner then joinChannel socket "JOIN" xs >>
                                          return bot else return bot
    | x == "!part" = if who == owner then joinChannel socket "PART" xs >>
                                          return bot else return bot
    | x == "!nick" = if who == owner then changeNick bot xs else return bot
evalCmd bot@(Bot socket (Parameter _ owner _ _ _ _ _) fugly@(dict, wne)) chan who (x:xs)
    | x == "!setparam" =
        if who == owner then case (length xs) of
          2 -> changeParam bot (xs!!0) (xs!!1)
          _ -> replyMsg socket chan who "Usage: !setparam parameter value" >> return bot
        else return bot
    | x == "!params" =
        if who == owner then replyMsg socket chan who (init (concat $ map (++ " ")
                                      $ map show $ init allParams)) >> return bot
        else return bot
    | x == "!dict" =
          case (length xs) of
            2 -> (wnGloss wne (xs!!0) (xs!!1)) >>= replyMsg socket chan who >> return bot
            1 -> (wnGloss wne (xs!!0) []) >>= replyMsg socket chan who >> return bot
            _ -> replyMsg socket chan who "Usage: !dict word [part-of-speech]" >> return bot
    | x == "!words" =
          replyMsg socket chan who (unwords $ listWords dict)
                               >> return bot
    | x == "!word" =
          case (length xs) of
            1 -> replyMsg socket chan who (listWordFull dict (xs!!0)) >> return bot
            _ -> replyMsg socket chan who "Usage: !word word" >> return bot
    | x == "!help" = if who == owner then replyMsg socket chan who
                       "Commands: !dict !word !words !params !setparam !nick !join !part !quit !load !save"
                       >> return bot
                     else replyMsg socket chan who "Commands: !dict !word !words" >> return bot
evalCmd bot _ _ _ = return bot

chanMsg :: Handle -> String -> String -> IO ()
chanMsg h chan msg = write h "PRIVMSG" (chan ++ " :" ++ msg)

replyMsg :: Handle -> String -> String -> String -> IO ()
replyMsg socket chan nick msg
    | chan == nick   = write socket "PRIVMSG" (nick ++ " :" ++ msg)
    | otherwise      = write socket "PRIVMSG" (chan ++ " :" ++ nick ++ ": " ++ msg)

privMsg :: Handle -> String -> String -> IO ()
privMsg socket nick msg = write socket "PRIVMSG" (nick ++ " :" ++ msg)
--privMsg socket nick msg = replyMsg socket nick nick msg

write :: Handle -> String -> String -> IO ()
write socket s msg = do
    hPrintf socket "%s %s\r\n" s msg
    printf         "> %s %s\n" s msg
