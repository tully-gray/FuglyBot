module Fugly.Command where

import           Control.Concurrent             (MVar, readMVar, swapMVar, threadDelay, ThreadId)
import           Control.Exception              (catch, SomeException)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.State.Lazy
import           Data.Char                      (isAscii)
import qualified Data.Map.Lazy                  as Map
import           Data.List                      (delete, nub)
import           Data.Maybe
import           Fugly.Parameter                as P
import           Fugly.Types                    hiding (topic)
import           Fugly.LoadSave
import           FuglyLib
import           NLP.WordNet.PrimTypes          (allForm, allPOS)
import           System.Random                  (getStdRandom, randomR)
import           Text.Regex.Posix               hiding (match)

alreadyMsg :: String
alreadyMsg = " already in dict."

notMsg :: String
notMsg = " not in dict."

rangeMsg :: String
rangeMsg = "Number not in range."

afterMsg :: String
afterMsg = " after word "

defMsg :: String
defMsg = "<Default|Normal|Response|Regex|Action|GreetAction|Greeting|Enter>"

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
           params=p@Parameter{P.nick=bn, owners=owners', fuglyDir=fdir,
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
      return b{params=(readParamsFromList np){P.nick=bn, owners=owners',
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
showParams b (Parameter nick' owners' _ dfile uCmd rkick maxcmsg numt nsets
              sTries' slen plen learn slearn stopic aName allowPM' debug' topic'
              randoms' rWord timer' delay' greets actions' matchC) o f m =
    if o then
      case length m of
        0 -> f ("nick: " ++ nick' ++ "  owners: " ++ unwords owners' ++ "  usercommands: " ++ show uCmd
              ++ "  rejoinkick: " ++ show rkick ++ "  maxchanmsg: " ++ show maxcmsg
              ++ "  numthreads: " ++ show numt ++ "  nsetsize: " ++ show nsets
              ++ "  sentencetries: " ++ show sTries' ++ "  sentencelength: " ++ show slen
              ++ "  parselength: " ++ show plen ++ "  dictfile: " ++ dfile
              ++ "  learning: " ++ learn ++ "  strictlearn: " ++ show slearn
              ++ "  stricttopic: " ++ show stopic ++ "  debug: " ++ show debug'
              ++ "  autoname: " ++ show aName ++ "  allowpm: " ++ show allowPM'
              ++ "  topic: " ++ topic' ++ "  randoms: " ++ show randoms'
              ++ "  replacewords: " ++ show rWord ++ "  matchchance: " ++ show matchC
              ++ "  timer: " ++ show timer' ++ "  delay: " ++ show delay'
              ++ "  greetings: " ++ show greets ++ "  actions: " ++ show actions') >> return b
        _ -> f "Usage: !showparams" >> return b
    else return b
showParams b _ _ _ _ = return b

setParam :: Bot -> FState -> Bool -> (String -> IO ())
            -> (String -> [String] -> StateT FState IO Bot) -> [String] -> IO Bot
setParam b st o f1 f2 m =
    let f    = evalStateT (f2 (m!!0) $ tail m) st
        lm   = length m
        msgH = "Usage: !setparam <parameter> <values>" in
    if o then
      if lm > 2 then
        if m!!0 == "owners" then f else f1 msgH >> return b
      else if lm > 1 then f
        else f1 msgH >> return b
    else return b

word :: Bot -> String -> (String -> IO ()) -> [String] -> IO Bot
word b@Bot{fugly=f@Fugly{dict=d}} x f1 m = return f >>
    case length m of
      1 -> f1 (listWordFull d (m!!0)) >> return b
      _ -> f1 ("Usage: " ++ x ++ " <" ++ (tail x) ++ ">") >> return b

wordList :: Bot -> String -> (String -> IO ()) -> [String] -> IO Bot
wordList b@Bot{fugly=f@Fugly{dict=d}} x f1 m = return f >>
    let num = if read (m!!0) > (100 :: Int) then 100 :: Int else read (m!!0)
        re  = x =~ "word|name|acronym"
        f2  = unwords . listWordsCountSort d num (x =~ re)
        f3  = show . numWords d (x =~ re) in
    case length m of
      2 -> f1 (f2 (m!!1)) >> f1 ("Total " ++ re ++ " count with topic " ++ (m!!1) ++ ": " ++ (f3 (m!!1))) >> return b
      1 -> f1 (f2 []) >> f1 ("Total " ++ re ++ " count: " ++ (f3 [])) >> return b
      _ -> f1 ("Usage: " ++ x ++ " <number> [topic]") >> return b

insertWord :: Bot -> FState -> Bool -> (String -> IO ())
              -> String -> [String] -> IO Bot
insertWord b@Bot{fugly=f@Fugly{dict=d}} st o f1 t m =
    if o then
      if lm == 0 || lm > 2 then
        f1 "Usage: !insertword <word> [Noun|Verb|Adj|Adv|Name]" >> return b
      else if isJust $ Map.lookup (m!!0) d then
        f1 ("Word " ++ (m!!0) ++ alreadyMsg) >> return b
      else do
        let p = if lm == 2 then m!!1 else []
        nd <- if p == "Name" then insertNameRaw l f True (m!!0) [] [] t
                else insertWordRaw l f True (m!!0) [] [] t p
        f1 ("Inserted word " ++ (m!!0) ++ ".") >> return b{fugly=f{dict=nd}}
    else return b
  where
    lm = length m
    l  = getLock st

insertAcronym :: Bot -> FState -> Bool -> (String -> IO ())
                 -> String -> [String] -> IO Bot
insertAcronym b@Bot{fugly=f@Fugly{dict=d}} st o f1 t m =
    if o then
      if length m > 1 then do
        ww <- insertAcroRaw (getLock st) f (m!!0) [] [] t $ unwords $ tail m
        if isJust $ Map.lookup (m!!0) d then
          f1 ("Acronym " ++ (m!!0) ++ alreadyMsg) >> return b
          else
            f1 ("Inserted acronym " ++ (m!!0) ++ ".") >> return b{fugly=f{dict=ww}}
      else
        f1 "Usage: !insertacronym <acronym> <definition>" >> return b
    else return b

insertDefault :: Bot -> Bool -> (String -> IO ()) -> [String] -> IO Bot
insertDefault b@Bot{fugly=f@Fugly{defs=d}} o f1 m =
    if o then
      if length m > 1 then let f' = \(t, w) -> t == read (head m) && w == unwords (tail m) in
        if null $ filter f' d then
          f1 ("Inserted default " ++ show ((read $ head m) :: DType) ++ " " ++ (unwords $ tail m) ++ ".") >>
          return b{fugly=f{defs=d ++ [(read $ head m, unwords $ tail m)]}}
          else f1 ("Default already stored.") >> return b
        else
        f1 ("Usage: !insertdefault " ++ defMsg ++ " <default>") >> return b
    else return b

dropDefault :: Bot -> Bool -> (String -> IO ()) -> [String] -> IO Bot
dropDefault b@Bot{fugly=f@Fugly{defs=d}} o f1 m =
    if o then
      if length m > 1 then let f' = \(t, w) -> t == read (head m) && w == unwords (tail m) in
        if null $ filter f' d then f1 ("No such default.") >> return b
          else
            f1 ("Dropped default " ++ show ((read $ head m) :: DType) ++ " " ++ (unwords $ tail m) ++ ".") >>
            return b{fugly=f{defs=filter (not . f') d}}
        else
        f1 ("Usage: !dropdefault " ++ defMsg ++ " <default>") >> return b
    else return b

defaultList :: Bot -> Bool -> (String -> IO ()) -> [String] -> IO Bot
defaultList b@Bot{fugly=f@Fugly{defs=d}} o f1 m = return f >>
    if o then
      if length m == 1 then
        let defs' = [de | (t, de) <- d, t == read (m!!0)] in
        mapM (\x' -> f1 x' >> threadDelay 1000000) defs' >> return b
      else
        f1 ("Usage: !defaultlist " ++ defMsg) >> return b
    else return b

dropWord :: Bot -> Bool -> (String -> IO ()) -> [String] -> IO Bot
dropWord b@Bot{fugly=f@Fugly{dict=d}} o f1 m =
    if o then let n = b{fugly=f{dict=FuglyLib.dropWord d (m!!0)}} in
      case length m of
        1 -> if isJust $ Map.lookup (m!!0) d then
               f1 ("Dropped word " ++ (m!!0) ++ ".") >>
               return n
             else {-- Drop the word anyway because it might be a before or after word. --}
               f1 ("Word " ++ (m!!0) ++ notMsg) >>
               return n
        _ -> f1 "Usage: !dropword <word>" >> return b
    else return b

dropAfter :: Bot -> Bool -> (String -> IO ()) -> [String] -> IO Bot
dropAfter b@Bot{fugly=f@Fugly{dict=d}} o f1 m =
    if o then
      case length m of
        2 -> if (isJust $ Map.lookup (m!!0) d) && (isJust $ Map.lookup (m!!1) d) then
               let nd = dropBefore d (m!!1) (m!!0) in
               f1 ("Dropped word " ++ (m!!1) ++ afterMsg ++ (m!!0) ++ ".") >>
               return b{fugly=f{dict=FuglyLib.dropAfter nd (m!!0) (m!!1)}}
             else {-- Drop the association anyway, but report errors. --}
               f1 ("Word " ++ (m!!0) ++ " or word " ++ (m!!1) ++ notMsg) >>
               return b{fugly=f{dict=FuglyLib.dropAfter (dropBefore d (m!!1) (m!!0)) (m!!0) (m!!1)}}
        1 -> if isJust $ Map.lookup (m!!0) d then
               f1 ("Dropped all words after word " ++ (m!!0) ++ ".") >>
               return b{fugly=f{dict=dropAllAfter d (m!!0)}}
             else
               f1 ("Word " ++ (m!!0) ++ notMsg) >>
               return b{fugly=f{dict=dropAllAfter d (m!!0)}}
        _ -> f1 "Usage: !dropafter <word> [after-word]" >> return b
    else return b

topicList :: Bot -> (String -> IO ()) -> [String] -> IO Bot
topicList b@Bot{fugly=f@Fugly{dict=d}} f1 m = return f >>
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
               f1 ("Topic " ++ (m!!0) ++ notMsg) >> return b
        _ -> f1 "Usage: !droptopic <topic>" >> return b
    else return b

dropTopicWords :: Bot -> Bool -> (String -> IO ()) -> [String] -> IO Bot
dropTopicWords b@Bot{fugly=f@Fugly{dict=d}} o f1 m =
    if o then
      case length m of
        1 -> if elem (m!!0) $ listTopics d then let nd = FuglyLib.dropTopicWords d (m!!0) in
               f1 ("Dropped all words in topic " ++ (m!!0) ++ ".") >>
               return b{fugly=f{dict=FuglyLib.dropTopic nd (m!!0)}}
             else
               f1 ("Topic " ++ (m!!0) ++ notMsg) >> return b
        _ -> f1 "Usage: !droptopicwords <topic>" >> return b
    else return b

banAfter :: Bot -> Bool -> (String -> IO ()) -> [String] -> IO Bot
banAfter b@Bot{fugly=f@Fugly{dict=d}} o f1 m =
    if o then
      case length m of
        3 -> if (m!!0) == "add" then
               let w = Map.lookup (m!!1) d in
               if isJust w then
                 let nd1 = Map.insert (m!!1) (addBanAfter (fromJust w) (m!!2)) d
                     nd2 = dropBefore nd1 (m!!2) (m!!1) in
                 if elem (m!!2) $ wordGetBanAfter $ fromJust w then
                   f1 ("Word " ++ (m!!2) ++ afterMsg ++ (m!!1) ++ " already banned.") >>
                   return b{fugly=f{dict=FuglyLib.dropAfter nd2 (m!!1) (m!!2)}}
                 else {-- Drop the association anyway, but report errors. --}
                   f1 ("Banned word " ++ (m!!2) ++ afterMsg ++ (m!!1) ++ ".") >>
                   return b{fugly=f{dict=FuglyLib.dropAfter nd2 (m!!1) (m!!2)}}
               else f1 ("Word " ++ (m!!1) ++ notMsg) >> return b
             else if (m!!0) == "delete" then let w = Map.lookup (m!!1) d in
             if isJust w then
               if notElem (m!!2) $ wordGetBanAfter $ fromJust w then
                 f1 ("Word " ++ (m!!2) ++ " not in ban list.") >> return b
               else
                 f1 ("Unbanned word " ++ (m!!2) ++ afterMsg ++ (m!!1) ++ ".") >>
                 return b{fugly=f{dict=Map.insert (m!!1) (deleteBanAfter (fromJust w) (m!!2)) d}}
             else f1 ("Word " ++ (m!!1) ++ notMsg) >> return b
                  else f1 msgH >> return b
        2 -> if (m!!0) == "list" then
               let w = Map.lookup (m!!1) d in
               if isJust w then
                 f1 ("Banned after word list: " ++ (unwords $ wordGetBanAfter $ fromJust w)) >> return b
               else f1 ("Word " ++ (m!!1) ++ notMsg) >> return b
             else f1 msgH >> return b
        _ -> f1 msgH >> return b
    else return b
  where
    msgH = "Usage: !banafter <list|add|delete> <word> <after-word>"

ageWord :: Bot -> FState -> Bool -> (String -> IO ())
           -> (String -> StateT FState IO ()) -> [String] -> IO Bot
ageWord b@Bot{fugly=f@Fugly{dict=d}} st o f1 f2 m =
    if o then
      case length m of
        2 -> do num <- catch (if (read (m!!1) :: Int) > 50 then return 50
                              else return (read (m!!1) :: Int))
                         {-- This has to be caught here? --}
                         (\e -> do let err = show (e :: SomeException)
                                   evalStateT (f2 ("Exception in ageword command: read: " ++ err)) st
                                   return 0)
                if isJust $ Map.lookup (m!!0) d then
                  if num > 0 then
                    f1 ("Aged word " ++ (m!!0) ++ ".") >>
                    return b{fugly=f{dict=FuglyLib.ageWord d (m!!0) num}}
                    else f1 rangeMsg >> return b
                  else f1 ("Word " ++ (m!!0) ++ notMsg) >> return b
        _ -> f1 "Usage: !ageword <word> <number>" >> return b
    else return b

forceLearn :: Bot -> FState -> Bool -> (String -> IO ())
              -> (String -> StateT FState IO ()) -> [String] -> IO Bot
forceLearn b@Bot{fugly=f, params=p@Parameter{topic=topic', autoName=an}}
             st o f1 f2 m = return p >>
    if o then let lenxs = length m in
      if lenxs == 0 || lenxs == 1 || lenxs == 2 then
        f1 "Usage: !forcelearn <number> <in> <out>" >> return b
      else do
        num <- catch (if (read (m!!0) :: Int) > 50 then return 50
                      else return (read (m!!0) :: Int))
                 {-- This has to be caught here? --}
                 (\e -> do let err = show (e :: SomeException)
                           _ <- evalStateT (f2 ("Exception in forcelearn command: read: " ++ err)) st
                           return 0)
        nd <- insertWordsN (getLock st) f an topic' num $ tail m
        if num > 0 then
          f1 ("Message learned " ++ (m!!0) ++ " times.") >>
          return b{fugly=f{dict=nd}}
          else f1 rangeMsg >> return b
    else return b
forceLearn b _ _ _ _ _ = return b

banWord :: Bot -> Bool -> (String -> IO ()) -> [String] -> IO Bot
banWord b@Bot{fugly=f@Fugly{ban=ban'}} o f1 m =
    if o then
      case length m of
        2 -> if (m!!0) == "add" then
               if elem (m!!1) ban' then
                 f1 ("Learning on word " ++ (m!!1) ++ " already banned.") >> return b
               else
                 f1 ("Banned learning on word " ++ (m!!1) ++ ".") >>
                 return b{fugly=f{ban=nub $ ban' ++ [(m!!1)]}}
             else if (m!!0) == "delete" then
               if notElem (m!!1) ban' then
                 f1 ("Word " ++ (m!!1) ++ " not in ban list.") >> return b
               else
                 f1 ("Unbanned learning on word " ++ (m!!1) ++ ".") >>
                 return b{fugly=f{ban=nub $ delete (m!!1) ban'}}
             else f1 msgH >> return b
        1 -> if (m!!0) == "list" then
                 f1 ("Banned word list: " ++ unwords ban') >> return b
             else f1 msgH >> return b
        _ -> f1 msgH >> return b
    else return b
  where
    msgH = "Usage: !banword <list|add|delete> <word>"

matchWord :: Bot -> Bool -> (String -> IO ()) -> [String] -> IO Bot
matchWord b@Bot{fugly=f@Fugly{match=match'}} o f1 m =
    if o then
      case length m of
        2 -> if (m!!0) == "add" then
               if elem (m!!1) match' then
                 f1 ("Word " ++ (m!!1) ++ " already matched.") >> return b
               else
                 f1 ("Matching word " ++ (m!!1) ++ ".") >>
                 return b{fugly=f{match=nub $ match' ++ [(m!!1)]}}
             else if (m!!0) == "delete" then
               if notElem (m!!1) match' then
                 f1 ("Word " ++ (m!!1) ++ " not in match list.") >> return b
               else
                 f1 ("No longer matching word " ++ (m!!1) ++ ".") >>
                 return b{fugly=f{match=nub $ delete (m!!1) match'}}
             else f1 msgH >> return b
        1 -> if (m!!0) == "list" then
               f1 ("Matched word list: " ++ unwords match') >> return b
             else f1 msgH >> return b
        _ -> f1 msgH >> return b
    else return b
  where
    msgH = "Usage: !matchword <list|add|delete> <word>"

talk :: Bot -> Bool -> (String -> IO ())
        -> (Bot -> Int -> [String] -> String -> String -> [String] -> IO ThreadId)
        -> IO [String] -> [String] -> IO Bot
talk b o f1 f2 f3 m = do
    if o then
      if length m > 2 then do
        r <- getStdRandom (randomR (0, 99))
        l <- f3 -- getLoad
        f2 b r l (m!!0) (m!!1) (drop 2 m) >> return b
      else f1 "Usage: !talk <channel> <nick> <msg>" >> return b
      else return b

raw :: Bot -> FState -> Bool -> (String -> IO ())
       -> (String -> String -> StateT FState IO ()) -> [String] -> IO Bot
raw b st o f1 f2 m =
    if o then
      if length m > 0 then evalStateT (f2 (m!!0) (unwords $ tail m)) st >> return b
      else f1 "Usage: !raw <msg>" >> return b
    else return b

dictL :: Bot -> FState -> (String -> IO ()) -> [String] -> IO Bot
dictL b@Bot{fugly=f} st f1 m = return f >>
    if not $ (length m) == 1 then
      f1 "Usage: !dict <word> [part-of-speech]" >> return b
    else
      (dictLookup (getLock st) f (m!!0) []) >>= (\x' -> f1 x') >> return b

closure :: Bot -> (String -> IO ()) -> (String -> String -> String -> IO String)
           -> [String] -> IO Bot
closure b f1 f2 m =
    case length m of
      3 -> (f2 (m!!0) (m!!1) (m!!2)) >>= (\x' -> f1 x') >> return b
      2 -> (f2 (m!!0) (m!!1) []) >>= (\x' -> f1 x') >> return b
      1 -> (f2 (m!!0) [] []) >>= (\x' -> f1 x') >> return b
      _ -> f1 "Usage: !closure <word> [form] [part-of-speech]" >> return b

meet :: Bot -> (String -> IO ()) -> (String -> String -> String -> IO String)
        -> [String] -> IO Bot
meet b f1 f2 m =
    case length m of
      3 -> (f2 (m!!0) (m!!1) (m!!2)) >>= (\x' -> f1 x') >> return b
      2 -> (f2 (m!!0) (m!!1) []) >>= (\x' -> f1 x') >> return b
      _ -> f1 "Usage: !meet <word> <word> [part-of-speech]" >> return b

parse :: Bot -> Bool -> (String -> IO ()) -> (String -> [String]) -> [String] -> IO Bot
parse b o f1 f2 m =
    if o then
       case length m of
         0 -> f1 "Usage: !parse <msg>" >> return b
         _ -> (mapM (\x' -> f1 x') $ take 3 (f2 (unwords $ take 12 m))) >> return b
    else return b

related :: Bot -> (String -> IO ()) -> (String -> String -> String -> IO String)
           -> [String] -> IO Bot
related b f1 f2 m =
    case length m of
      3 -> (f2 (m!!0) (m!!1) (m!!2)) >>= (\x' -> f1 x') >> return b
      2 -> (f2 (m!!0) (m!!1) []) >>= (\x' -> f1 x') >> return b
      1 -> (f2 (m!!0) [] []) >>= (\x' -> f1 x') >> return b
      _ -> f1 "Usage: !related <word> [form] [part-of-speech]" >> return b

showForms :: Bot -> (String -> IO ()) -> [String] -> IO Bot
showForms b f1 m =
    case length m of
          0 -> f1 (concat $ map (++ " ") $ map show allForm) >> return b
          _ -> f1 "Usage: !forms" >> return b

showParts :: Bot -> (String -> IO ()) -> [String] -> IO Bot
showParts b f1 m =
    case length m of
      0 -> f1 (concat $ map (++ " ") $ map show allPOS) >> return b
      _ -> f1 "Usage: !parts" >> return b

listParams :: Bot -> Bool -> (String -> IO ()) -> IO Bot
listParams b o f1 =
    if o then f1 (init (concat $ map (++ " ") $ map show $ init allParams)) >> return b
    else return b

isName :: Bot -> (String -> IO ()) -> (String -> IO Bool) -> [String] -> IO Bot
isName b f1 f2 m =
    case length m of
      1 -> f2 (m!!0) >>= (\x' -> f1 $ show x') >> return b
      _ -> f1 "Usage: !isname <word>" >> return b

isAcronym :: Bot -> (String -> IO ()) -> (String -> IO Bool) -> [String] -> IO Bot
isAcronym b f1 f2 m =
    case length m of
      1 -> f2 (m!!0) >>= (\x' -> f1 $ show x') >> return b
      _ -> f1 "Usage: !isacronym <word>" >> return b

help :: Bot -> Bool -> (String -> IO ()) -> IO Bot
help b o f1 = if o then f1
      ("Commands: !word !wordlist !insertword !name !namelist !acronym !acronymlist !insertacronym "
      ++ "!dropword !banword !matchword !insertdefault !dropdefault !defaultlist !dropafter !banafter "
      ++ "!ageword !topiclist !droptopic !droptopicwords !forcelearn "
      ++ "!dict !closure !meet !parse !related !forms !parts !isname !isacronym "
      ++ "!setparam !showparams !nick !join !part !talk !raw !quit !load !save") >> return b
              else f1
      ("Commands: !word !wordlist !name !namelist !acronym !acronymlist !topiclist "
      ++ "!dict !closure !meet !related !forms !parts !isname !isacronym") >> return b
