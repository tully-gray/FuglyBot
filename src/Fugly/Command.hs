module Fugly.Command where

import           Control.Concurrent             (MVar, readMVar, swapMVar)
import           Control.Exception              (catch, SomeException)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.State.Lazy
import           Data.Char                      (isAscii)
import qualified Data.Map.Lazy                  as Map
import           Data.Maybe
import           Fugly.Parameter                as P
import           Fugly.Types                    hiding (topic)
import           Fugly.LoadSave
import           FuglyLib
import           Text.Regex.Posix               hiding (match)

msg1 :: String
msg1 = " already in dict."

msg2 :: String
msg2 = "<Default|Normal|Response|Action|GreetAction|Greeting|Enter> <default>"

msg3 :: String
msg3 = " not in dict."

quit :: Bot -> FState -> Bool -> (String -> String -> StateT FState IO ())
        -> [String] -> IO Bot
quit b st o f m =
    if o then
      case length m of
        0 -> do evalStateT (f "QUIT" ":Bye") st >> return b
        _ -> do evalStateT (f "QUIT" (":" ++ unwords m)) st >> return b
    else return b

save :: Bot -> FState -> Bool -> (String -> IO ())
        -> (String -> StateT FState IO ()) -> IO Bot
save b@Bot{fugly=f, params=p@Parameter{fuglyDir=fdir, dictFile=dfile}}
           st o f1 f2 =
    let l = getLock st in
    if o then
      catch (do
             saveDict l f fdir dfile $ paramsToList p
             saveNeural l f fdir)
      (\e -> do let err = show (e :: SomeException)
                _ <- evalStateT (f2 ("Exception saving state: " ++ err)) st
                return ()) >> f1 "Saved bot state!" >> return b
    else return b
save b _ _ _ _ = return b

load :: Bot -> FState -> Bool -> (String -> IO ())
        -> (String -> StateT FState IO ()) -> IO Bot
load b@Bot{fugly=f@(Fugly dict' defs' pgf' wne' aspell' ban' match' _ _ _),
           params=p@Parameter{P.nick=bn, owner=owner', fuglyDir=fdir,
           dictFile=dfile, nSetSize=nsets}} st o f1 f2 =
    if o then do
      (nd, nde, nb, nm, np) <- catch (loadDict fdir dfile)
        (\e -> do let err = show (e :: SomeException)
                  _ <- evalStateT (f2 ("Exception in loadDict: " ++ err)) st
                  return (dict', defs', ban', match', paramsToList p))
      (ns, nm') <- catch (loadNeural fdir nsets)
        (\e -> do let err = show (e :: SomeException)
                  _ <- evalStateT (f2 ("Exception in loadNeural: " ++ err)) st
                  return ([], Map.empty))
      _ <- f1 "Loaded bot state!"
      return b{params=(readParamsFromList np){P.nick=bn, owner=owner',
        fuglyDir=fdir, dictFile=dfile}, fugly=f{dict=nd, defs=nde, pgf=pgf',
        wne=wne', aspell=aspell', ban=nb, match=nm, nset=ns, nmap=nm'}}
    else return b
load b _ _ _ _ = return b

join :: Bot -> FState -> Bool -> (String -> [String] -> StateT FState IO ())
        -> [String] -> IO Bot
join b st o f m = if o then evalStateT (f "JOIN" m) st >> return b else return b

part :: Bot -> FState -> Bool -> MVar (Map.Map String [String])
        -> (String -> [String] -> StateT FState IO ()) -> [String] -> IO Bot
part b st o cn f m =
    if o then
      evalStateT (do
        cn' <- lift $ readMVar cn
        f "PART" m
        lift $ swapMVar cn (Map.delete (unwords m) cn')) st >> return b
    else return b

nick :: Bot -> FState -> Bool -> (String -> IO ())
        -> (String -> String -> StateT FState IO ()) -> [String] -> IO Bot
nick b st o f1 f2 m =
    if o then
      case length m of
        1 -> evalStateT ((\x' -> f2 "NICK" $ cleanStringWhite isAscii x') (m!!0)) st >> return b
        _ -> f1 "Usage: !nick <nick>" >> return b
    else return b

showParams :: Bot -> Parameter -> Bool -> (String -> IO ()) -> [String] -> IO Bot
showParams b (Parameter nick' owner' _ dfile uCmd rkick maxcmsg numt nsets
              sTries' slen plen learn slearn stopic aName allowPM' debug' topic'
              randoms' rWord timer' delay' greets actions') o f m =
    if o then
      case length m of
        0 -> f ("nick: " ++ nick' ++ "  owner: " ++ owner' ++ "  usercommands: " ++ show uCmd
              ++ "  rejoinkick: " ++ show rkick ++ "  maxchanmsg: " ++ show maxcmsg
              ++ "  numthreads: " ++ show numt ++ "  nsetsize: " ++ show nsets
              ++ "  sentencetries: " ++ show sTries' ++ "  sentencelength: " ++ show slen
              ++ "  parselength: " ++ show plen ++ "  dictfile: " ++ dfile
              ++ "  learning: " ++ learn ++ "  strictlearn: " ++ show slearn
              ++ "  stricttopic: " ++ show stopic ++ "  debug: " ++ show debug'
              ++ "  autoname: " ++ show aName ++ "  allowpm: " ++ show allowPM'
              ++ "  topic: " ++ topic' ++ "  randoms: " ++ show randoms'
              ++ "  replacewords: " ++ show rWord
              ++ "  timer: " ++ show timer' ++ "  delay: " ++ show delay'
              ++ "  greetings: " ++ show greets ++ "  actions: " ++ show actions') >> return b
        _ -> f "Usage: !showparams" >> return b
    else return b
showParams b _ _ _ _ = return b

setParam :: Bot -> FState -> Bool -> (String -> IO ())
            -> (String -> String -> StateT FState IO Bot) -> [String] -> IO Bot
setParam b st o f1 f2 m =
    if o then
      case length m of
        2 -> evalStateT (f2 (m!!0) (m!!1)) st
        _ -> f1 "Usage: !setparam <parameter> <value>" >> return b
    else return b

word :: Bot -> String -> (String -> IO ()) -> [String] -> IO Bot
word b@Bot{fugly=f@Fugly{dict=d}} x f1 m = case length m of
    1 -> f1 (listWordFull d (m!!0)) >> return b
    _ -> f1 ("Usage: " ++ x ++ " <" ++ (tail x) ++ ">") >> return b

wordList :: Bot -> String -> (String -> IO ()) -> [String] -> IO Bot
wordList b@Bot{fugly=f@Fugly{dict=d}} x f1 m =
    let num = if read (m!!0) > (100 :: Int) then 100 :: Int else read (m!!0)
        re  = "word|name|acronym" in
    case length m of
      2 -> f1 (unwords $ listWordsCountSort d num (x =~ re) (m!!1)) >>
             f1 ("Total " ++ (x =~ re) ++ " count with topic " ++ (m!!1) ++ ": " ++
             (show $ numWords d (x =~ re) (m!!1))) >> return b
      1 -> f1 (unwords $ listWordsCountSort d num (x =~ re) []) >>
             f1 ("Total " ++ (x =~ re) ++ " count: " ++
             (show $ numWords d (x =~ re) [])) >> return b
      _ -> f1 ("Usage: " ++ x ++ " <number> [topic]") >> return b

insertWord :: Bot -> FState -> Bool -> (String -> IO ())
              -> String -> [String] -> IO Bot
insertWord b@Bot{fugly=f@Fugly{dict=d}} st o f1 t m =
    if o then let l = getLock st in
    case length m of
      2 -> do ww <- insertWordRaw l f True (m!!0) [] [] t (m!!1)
              if isJust $ Map.lookup (m!!0) d then
                f1 ("Word " ++ (m!!0) ++ msg1) >> return b
                else
                  f1 ("Inserted word " ++ (m!!0) ++ ".") >> return b{fugly=f{dict=ww}}
      1 -> do ww <- insertWordRaw l f True (m!!0) [] [] t []
              if isJust $ Map.lookup (m!!0) d then
                f1 ("Word " ++ (m!!0) ++ msg1) >> return b
                else
                  f1 ("Inserted word " ++ (m!!0) ++ ".") >> return b{fugly=f{dict=ww}}
      _ -> f1 "Usage: !insertword <word> [pos]" >> return b
    else return b

insertName :: Bot -> FState -> Bool -> (String -> IO ())
              -> String -> [String] -> IO Bot
insertName b@Bot{fugly=f@Fugly{dict=d}} st o f1 t m =
    if o then let l = getLock st in
      case length m of
        1 -> do ww <- insertNameRaw l f True (m!!0) [] [] t
                if isJust $ Map.lookup (m!!0) d then
                  f1 ("Name " ++ (m!!0) ++ msg1) >> return b
                  else
                    f1 ("Inserted name " ++ (m!!0) ++ ".") >> return b{fugly=f{dict=ww}}
        _ -> f1 "Usage: !insertname <name>" >> return b
    else return b

insertAcronym :: Bot -> FState -> Bool -> (String -> IO ())
                 -> String -> [String] -> IO Bot
insertAcronym b@Bot{fugly=f@Fugly{dict=d}} st o f1 t m =
    if o then
      if length m > 1 then do
        ww <- insertAcroRaw (getLock st) f (m!!0) [] [] t $ unwords $ tail m
        if isJust $ Map.lookup (m!!0) d then
          f1 ("Acronym " ++ (m!!0) ++ msg1) >> return b
          else
            f1 ("Inserted acronym " ++ (m!!0) ++ ".") >> return b{fugly=f{dict=ww}}
      else
        f1 "Usage: !insertacronym <acronym> <definition>" >> return b
    else return b

insertDefault :: Bot -> Bool -> (String -> IO ()) -> [String] -> IO Bot
insertDefault b@Bot{fugly=f@Fugly{defs=d}} o f1 m =
    if o then
      if length m > 1 then
        f1 ("Inserted default " ++ show ((read $ head m) :: DType) ++ " " ++ (unwords $ tail m) ++ ".") >>
        return b{fugly=f{defs=d ++ [(read $ head m, unwords $ tail m)]}}
        else
          f1 ("Usage: !insertdefault " ++ msg2) >> return b
    else return b

dropDefault :: Bot -> Bool -> (String -> IO ()) -> [String] -> IO Bot
dropDefault b@Bot{fugly=f@Fugly{defs=d}} o f1 m =
    if o then
      if length m > 1 then
        f1 ("Dropped default " ++ show ((read $ head m) :: DType) ++ " " ++ (unwords $ tail m) ++ ".") >>
        return b{fugly=f{defs=filter (\(t, w) -> not (t == read (head m) && w == unwords (tail m))) d}}
        else
          f1 ("Usage: !dropdefault " ++ msg2) >> return b
    else return b

dropWord :: Bot -> Bool -> (String -> IO ()) -> [String] -> IO Bot
dropWord b@Bot{fugly=f@Fugly{dict=d}} o f1 m =
    if o then let n = b{fugly=f{dict=FuglyLib.dropWord d (m!!0)}} in
      case length m of
        1 -> if isJust $ Map.lookup (m!!0) d then
               f1 ("Dropped word " ++ (m!!0) ++ ".") >>
               return n
               else {-- Drop the word anyway because it might be a before or after word. --}
                 f1 ("Word " ++ (m!!0) ++ msg3) >>
                 return n
        _ -> f1 "Usage: !dropword <word>" >> return b
    else return b

dropAfter :: Bot -> Bool -> (String -> IO ()) -> [String] -> IO Bot
dropAfter b@Bot{fugly=f@Fugly{dict=d}} o f1 m =
    if o then
      case length m of
        2 -> if (isJust $ Map.lookup (m!!0) d) && (isJust $ Map.lookup (m!!1) d) then
               let nd = dropBefore d (m!!1) (m!!0) in
               f1 ("Dropped word " ++ (m!!1) ++ " after word " ++ (m!!0) ++ ".") >>
               return b{fugly=f{dict=FuglyLib.dropAfter nd (m!!0) (m!!1)}}
               else {-- Drop the association anyway, but report errors. --}
                 f1 ("Word " ++ (m!!0) ++ " or word " ++ (m!!1) ++ msg3) >>
                 return b{fugly=f{dict=FuglyLib.dropAfter (dropBefore d (m!!1) (m!!0)) (m!!0) (m!!1)}}
        1 -> if isJust $ Map.lookup (m!!0) d then
               f1 ("Dropped all words after word " ++ (m!!0) ++ ".") >>
               return b{fugly=f{dict=dropAllAfter d (m!!0)}}
               else
                 f1 ("Word " ++ (m!!0) ++ msg3) >>
                 return b{fugly=f{dict=dropAllAfter d (m!!0)}}
        _ -> f1 "Usage: !dropafter <word> [after-word]" >> return b
    else return b

topicList :: Bot -> (String -> IO ()) -> [String] -> IO Bot
topicList b@Bot{fugly=f@Fugly{dict=d}} f1 m =
    case length m of
      0 -> f1 ("topics: " ++ (unwords $ listTopics d)) >> return b
      _ -> f1 "Usage: !topiclist" >> return b

dropTopic :: Bot -> Bool -> (String -> IO ()) -> [String] -> IO Bot
dropTopic b@Bot{fugly=f@Fugly{dict=d}} o f1 m =
    if o then
      case length m of
        1 -> if elem (m!!0) $ listTopics d then
               f1 ("Dropped topic " ++ (m!!0) ++ ".") >>
               return b{fugly=f{dict=FuglyLib.dropTopic d (m!!0)}}
             else
               f1 ("Topic " ++ (m!!0) ++ msg3) >> return b
        _ -> f1 "Usage: !droptopic <topic>" >> return b
    else return b
