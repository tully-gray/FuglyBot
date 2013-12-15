module FuglyLib
       (
         initFugly,
         stopFugly,
         loadDict,
         saveDict,
         dictLookup,
         insertName,
         insertWord,
         insertWordRaw,
         insertWords,
         dropWord,
         listWords,
         listWordFull,
         listWordsCountSort,
         listWordsCountSort2,
         cleanString,
         wnClosure,
         wnMeet,
         sentence,
         Word,
         Fugly
       )
       where

import Control.Exception
import qualified Data.ByteString.Char8 as ByteString
import Data.Char (isAlpha, isAlphaNum, toLower, toUpper)
import Data.Either
import Data.IORef
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Tree (flatten)
import System.Random as Random
import System.IO
import System.IO.Error

import qualified Data.MarkovChain as Markov

import qualified Language.Aspell as Aspell

import NLP.WordNet hiding (Word)
import NLP.WordNet.Prims (indexLookup, senseCount, getSynset, getWords, getGloss)
import NLP.WordNet.PrimTypes

type Fugly = (Map.Map String Word, WordNetEnv, Aspell.SpellChecker, [String], [String])

data Word = Word {
              word    :: String,
              count   :: Int,
              before  :: Map.Map String Int,
              after   :: Map.Map String Int,
              related :: [String],
              pos     :: EPOS
              } |
            Name {
              name    :: String,
              count   :: Int,
              before  :: Map.Map String Int,
              after   :: Map.Map String Int,
              related :: [String]
              } |
            Place {
              word    :: String,
              count   :: Int,
              before  :: Map.Map String Int,
              after   :: Map.Map String Int,
              related :: [String]
              } |
            Phrase {
              word    :: String,
              count   :: Int,
              before  :: Map.Map String Int,
              after   :: Map.Map String Int,
              related :: [String]
              }

initFugly :: FilePath -> FilePath -> IO Fugly
initFugly fuglydir wndir = do
    (dict, allow, ban) <- catchIOError (loadDict fuglydir)
                          (const $ return (Map.empty, [], []))
    wne <- NLP.WordNet.initializeWordNetWithOptions
           (return wndir :: Maybe FilePath)
           (Just (\e f -> putStrLn (e ++ show (f :: SomeException))))
    a <- Aspell.spellChecker
    let aspell = head $ rights [a]
    return (dict, wne, aspell, allow, ban)

stopFugly :: FilePath -> Fugly -> IO ()
stopFugly fuglydir fugly@(_, wne, _, _, _) = do
    catchIOError (saveDict fugly fuglydir) (const $ return ())
    closeWordNet wne

saveDict :: Fugly -> FilePath -> IO ()
saveDict fugly@(dict, _, _, allow, ban) fuglydir = do
    let d = Map.toList dict
    if null d then putStrLn "Empty dict!"
      else do
        h <- openFile (fuglydir ++ "/dict.txt") WriteMode
        hSetBuffering h LineBuffering
        putStrLn "Saving dict file..."
        saveDict' h d
        hPutStrLn h ">END<"
        hPutStrLn h $ unwords allow
        hPutStrLn h $ unwords ban
        hFlush h
        hClose h
  where
    saveDict' :: Handle -> [(String, Word)] -> IO ()
    saveDict' _ [] = return ()
    saveDict' h (x:xs) = do
      let l = format' $ snd x
      if null l then return () else hPutStr h l
      saveDict' h xs
    format' m@(Word w c b a r p)
      | null w    = []
      | otherwise = unwords [("word: " ++ w ++ "\n"),
                             ("count: " ++ (show c) ++ "\n"),
                             ("before: " ++ (unwords $ listNeigh2 b) ++ "\n"),
                             ("after: " ++ (unwords $ listNeigh2 a) ++ "\n"),
                             ("related: " ++ (unwords r) ++ "\n"),
                             ("pos: " ++ (show p) ++ "\n")]
    format' m@(Name w c b a r)
      | null w    = []
      | otherwise = unwords [("name: " ++ w ++ "\n"),
                             ("count: " ++ (show c) ++ "\n"),
                             ("before: " ++ (unwords $ listNeigh2 b) ++ "\n"),
                             ("after: " ++ (unwords $ listNeigh2 a) ++ "\n"),
                             ("related: " ++ (unwords r) ++ "\n"),
                             ("pos: name\n")]

loadDict :: FilePath -> IO (Map.Map String Word, [String], [String])
loadDict fuglydir = do
    let w = (Word [] 0 Map.empty Map.empty [] UnknownEPos)
    h <- openFile (fuglydir ++ "/dict.txt") ReadMode
    eof <- hIsEOF h
    if eof then
      return (Map.empty, [], [])
      else do
      hSetBuffering h LineBuffering
      putStrLn "Loading dict file..."
      dict <- f h w [([], w)]
      allow <- hGetLine h
      ban <- hGetLine h
      let out = (Map.fromList dict, words allow, words ban)
      hClose h
      return out
  where
    getNeigh :: [String] -> Map.Map String Int
    getNeigh a = Map.fromList $ getNeigh' a []
    getNeigh' :: [String] -> [(String, Int)] -> [(String, Int)]
    getNeigh'        [] l = l
    getNeigh' (x:y:xs) [] = getNeigh' xs [(x, read y)]
    getNeigh' (x:y:xs)  l = getNeigh' xs (l ++ (x, read y) : [])
    f :: Handle -> Word -> [(String, Word)] -> IO [(String, Word)]
    f h word@(Name n c b a r) nm = f h (Word n c b a r (POS Noun)) nm
    f h word@(Word w c b a r p) nm = do
      l <- hGetLine h
      let wl = words l
      let l1 = length $ filter (\x -> x /= ' ') l
      let l2 = length wl
      let l3 = elem ':' l
      let l4 = if l1 > 3 && l2 > 0 && l3 == True then True else False
      let ll = if l4 == True then tail wl else ["BAD BAD BAD"]
      let ww = if l4 == True then case (head wl) of
                           "word:"    -> (Word (unwords ll) c b a r p)
                           "name:"    -> (Name (unwords ll) c b a r)
                           "count:"   -> (Word w (read $ unwords $ ll) b a r p)
                           "before:"  -> (Word w c (getNeigh $ ll) a r p)
                           "after:"   -> (Word w c b (getNeigh $ ll) r p)
                           "related:" -> (Word w c b a (joinWords '"' ll) p)
                           "pos:"     -> (Word w c b a r
                                          (readEPOS $ unwords $ ll))
                           _          -> (Word w c b a r p)
               else (Word w c b a r p)
      if l4 == False then do putStrLn ("Oops: " ++ l) >> return nm
        else if (head wl) == "pos:" then
               f h ww (nm ++ ((wordGetWord ww), ww) : [])
             else if (head wl) == ">END<" then
                    return nm
                  else
                    f h ww nm

wordGetWord    (Word w c b a r p) = w
wordGetWord    (Name n c b a r)   = n
wordGetAfter   (Word w c b a r p) = a
wordGetAfter   (Name n c b a r)   = a
wordGetBefore  (Word w c b a r p) = b
wordGetBefore  (Name n c b a r)   = b
wordGetRelated (Word w c b a r p) = r
wordGetRelated (Name n c b a r)   = r
wordGetwc      (Word w c _ _ _ _) = (c, w)
wordGetwc      (Name w c _ _ _)   = (c, w)

insertWords :: Fugly -> [String] -> IO (Map.Map String Word)
insertWords f@(dict, _, _, _, _) [] = return dict
insertWords f [x] = insertWord f x [] [] []
insertWords f@(dict, wne, aspell, allow, ban) msg@(x:y:xs) =
      case (len) of
        2 -> do ff <- insertWord f x [] y []
                insertWord (ff, wne, aspell, allow, ban) y x [] []
        _ -> insertWords' f 0 len msg
  where
    len = length msg
    insertWords' f@(d, w, s, a, b) _ _ [] = return d
    insertWords' f@(d, w, s, a, b) i l msg
      | i == 0     = do ff <- insertWord f (msg!!i) [] (msg!!(i+1)) []
                        insertWords' (ff, w, s, a, b) (i+1) l msg
      | i > l - 1  = return d
      | i == l - 1 = do ff <- insertWord f (msg!!i) (msg!!(i-1)) [] []
                        insertWords' (ff, w, s, a, b) (i+1) l msg
      | otherwise  = do ff <- insertWord f (msg!!i) (msg!!(i-1)) (msg!!(i+1)) []
                        insertWords' (ff, w, s, a, b) (i+1) l msg

insertWord :: Fugly -> String -> String -> String -> String -> IO (Map.Map String Word)
insertWord fugly@(dict, wne, aspell, allow, ban) [] _ _ _ = return dict
insertWord fugly@(dict, wne, aspell, allow, ban) word before after pos =
    if elem word ban || elem before ban || elem after ban then return dict
    else if isJust w then f $ fromJust w
         else insertWordRaw' fugly w ww bb aa pos
  where
    w = Map.lookup word dict
    a = Map.lookup after dict
    b = Map.lookup before dict
    f (Word _ _ _ _ _ _) = insertWordRaw' fugly w ww bi ai pos
    f (Name _ _ _ _ _)   = insertName'    fugly w word bi ai
    an (Word _ _ _ _ _ _) = aa
    an (Name _ _ _ _ _)   = after
    bn (Word _ _ _ _ _ _) = bb
    bn (Name _ _ _ _ _)   = before
    ai = if isJust a then an $ fromJust a else aa
    bi = if isJust b then bn $ fromJust b else bb
    aa = map toLower $ cleanString isAlpha after
    bb = map toLower $ cleanString isAlpha before
    ww = if isJust w then word else map toLower $ cleanString isAlpha word

insertWordRaw f@(d, _, _, _, _) w b a p = insertWordRaw' f (Map.lookup w d) w b a p
insertWordRaw' :: Fugly -> Maybe Word -> String -> String
                 -> String -> String -> IO (Map.Map String Word)
insertWordRaw' (dict, wne, aspell, _, _) _ [] _ _ _ = return dict
insertWordRaw' (dict, wne, aspell, allow, _) w word before after pos = do
  pp <- (if null pos then wnPartPOS wne word else return $ readEPOS pos)
  pa <- wnPartPOS wne after
  pb <- wnPartPOS wne before
  rel <- wnRelated wne word "Hypernym" pp
  as <- asSuggest aspell word
  let asw = words as
  let nn x y  = if elem x allow then x
                else if y == UnknownEPos && Aspell.check aspell (ByteString.pack x) == False then [] else x
  let i x = Map.insert x (Word x 1 (e (nn before pb)) (e (nn after pa)) rel pp) dict
  if isJust w then
    return $ Map.insert word (Word word c (nb before pb) (na after pa)
                            (wordGetRelated (fromJust w))
                            (((\x@(Word _ _ _ _ _ p) -> p)) (fromJust w))) dict
         else if elem word allow then
           return $ i word
           else if pp /= UnknownEPos || Aspell.check aspell (ByteString.pack word) then
                  return $ i word
                  else if (length asw) > 0 then return $ i (head asw)
                       else
                         return dict
  where
    e [] = Map.empty
    e x = Map.singleton x 1
    c = incCount' (fromJust w)
    na x y = if elem x allow then incAfter' (fromJust w) x
                else if y /= UnknownEPos || Aspell.check aspell (ByteString.pack x) then incAfter' (fromJust w) x
                     else wordGetAfter (fromJust w)
    nb x y = if elem x allow then incBefore' (fromJust w) x
                else if y /= UnknownEPos || Aspell.check aspell (ByteString.pack x) then incBefore' (fromJust w) x
                     else wordGetBefore (fromJust w)

insertName f@(d, _, _, _, _) w b a = insertName' f (Map.lookup w d) w b a
insertName' :: Fugly -> Maybe Word -> String -> String
              -> String -> IO (Map.Map String Word)
insertName' (dict, wne, aspell,  _, _) _ [] _ _ = return dict
insertName' (dict, wne, aspell, allow, _) w name before after = do
  pa <- wnPartPOS wne after
  pb <- wnPartPOS wne before
  rel <- wnRelated wne name "Hypernym" (POS Noun)
  if isJust w then
    return $ Map.insert name (Name name c (nb before pb) (na after pa)
                              (wordGetRelated (fromJust w))) dict
    else
    return $ Map.insert name (Name name 1 (e (nn before pb)) (e (nn after pa)) rel) dict
  where
    e [] = Map.empty
    e x = Map.singleton x 1
    c = incCount' (fromJust w)
    na x y = if elem x allow then incAfter' (fromJust w) x
                else if y /= UnknownEPos || Aspell.check aspell (ByteString.pack x) then incAfter' (fromJust w) x
                     else wordGetAfter (fromJust w)
    nb x y = if elem x allow then incBefore' (fromJust w) x
                else if y /= UnknownEPos || Aspell.check aspell (ByteString.pack x) then incBefore' (fromJust w) x
                     else wordGetBefore (fromJust w)
    nn x y  = if elem x allow then x
                else if y == UnknownEPos && Aspell.check aspell (ByteString.pack x) == False then [] else x

dropWord :: Map.Map String Word -> String -> Map.Map String Word
dropWord m word = Map.map del' (Map.delete word m)
    where
      del' (Word w c b a r p) = (Word w c (Map.delete word b) (Map.delete word a) r p)
      del' (Name w c b a r) = (Name w c (Map.delete word b) (Map.delete word a) r)

startWords :: [String]
startWords = ["me", "you"]

incCount' :: Word -> Int
incCount' (Word _ c _ _ _ _) = c + 1
incCount' (Name _ c _ _ _) = c + 1

incBefore' :: Word -> String -> Map.Map String Int
incBefore' (Word _ _ b _ _ _) []     = b
incBefore' (Word _ _ b _ _ _) before =
  if isJust w then
    Map.insert before ((fromJust w) + 1) b
    else
    Map.insert before 1 b
  where
    w = Map.lookup before b
incBefore' (Name _ _ b _ _)   []     = b
incBefore' (Name w c b a r) before   = incBefore' (Word w c b a r (POS Noun)) before

incBefore :: Map.Map String Word -> String -> String -> Map.Map String Int
incBefore m word before = do
  let w = Map.lookup word m
  if isJust w then incBefore' (fromJust w) before
    else Map.empty

incAfter' :: Word -> String -> Map.Map String Int
incAfter' (Word _ _ _ a _ _) []     = a
incAfter' (Word _ _ _ a _ _) after  =
  if isJust w then
    Map.insert after ((fromJust w) + 1) a
    else
    Map.insert after 1 a
  where
    w = Map.lookup after a
incAfter' (Name _ _ _ a _)   []     = a
incAfter' (Name w c b a r) after    = incAfter' (Word w c b a r (POS Noun)) after

incAfter :: Map.Map String Word -> String -> String -> Map.Map String Int
incAfter m word after = do
  let w = Map.lookup word m
  if isJust w then incAfter' (fromJust w) after
    else Map.empty

listNeigh :: Map.Map String Int -> [String]
listNeigh m = [w | (w, c) <- Map.toList m]

listNeigh2 :: Map.Map String Int -> [String]
listNeigh2 m = concat [[w, show c] | (w, c) <- Map.toList m]

listNeighMax :: Map.Map String Int -> [String]
listNeighMax m = [w | (w, c) <- Map.toList m, c == maximum [c | (w, c) <- Map.toList m]]

listNeighMax2 :: Map.Map String Int -> [String]
listNeighMax2 m = concat [[w, show c] | (w, c) <- Map.toList m,
                          c == maximum [c | (w, c) <- Map.toList m]]

listWords :: Map.Map String Word -> [String]
listWords m = map wordGetWord $ Map.elems m

listWordsCountSort :: Map.Map String Word -> [String]
listWordsCountSort m = reverse $ map snd (sort $ map wordGetwc $ Map.elems m)

listWordsCountSort2 :: Map.Map String Word -> Int -> [String]
listWordsCountSort2 m num = concat [[w, show c, ";"] | (c, w) <- take num $ reverse $
                            sort $ map wordGetwc $ Map.elems m]

listWordFull :: Map.Map String Word -> String -> String
listWordFull m word =
  if isJust ww then
    unwords $ f (fromJust ww)
  else
    "Nothing!"
  where
    ww = Map.lookup word m
    f (Word w c b a r p) = ["word:", w, "count:", show c, " before:",
                 unwords $ listNeigh2 b, " after:", unwords $ listNeigh2 a,
                 " pos:", (show p), " related:", unwords r]
    f (Name w c b a r) = ["name:", w, "count:", show c, " before:",
                 unwords $ listNeigh2 b, " after:", unwords $ listNeigh2 a,
                 " related:", unwords r]

cleanString :: (Char -> Bool) -> String -> String
cleanString _ [] = []
cleanString f (x:xs)
        | not $ f x = cleanString f xs
        | otherwise = x : cleanString f xs

strip :: Eq a => a -> [a] -> [a]
strip _ [] = []
strip a (x:xs)
    | x == a    = strip a xs
    | otherwise = x : strip a xs

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace a b (x:xs)
    | x == a    = b : replace a b xs
    | otherwise = x : replace a b xs

joinWords :: Char -> [String] -> [String]
joinWords _ [] = []
joinWords a (x:xs)
    | (head x) == a   = unwords (x : (take num xs)) : joinWords a (drop num xs)
    | otherwise       = x : joinWords a xs
  where num = (fromMaybe 0 (elemIndex a $ map last xs)) + 1

fixUnderscore :: String -> String
fixUnderscore = strip '"' . replace ' ' '_'

wnLength :: [[a]] -> Int
wnLength a = (length a) - (length $ elemIndices True (map null a))

wnPartString :: WordNetEnv -> String -> IO String
wnPartString _ [] = return "Unknown"
wnPartString w a  = do
    ind1 <- indexLookup w a Noun
    ind2 <- indexLookup w a Verb
    ind3 <- indexLookup w a Adj
    ind4 <- indexLookup w a Adv
    return (type' ((count' ind1) : (count' ind2) : (count' ind3) : (count' ind4) : []))
  where
    count' a = if isJust a then senseCount (fromJust a) else 0
    type' [] = "Other"
    type' a
      | a == [0, 0, 0, 0]                             = "Unknown"
      | fromMaybe (-1) (elemIndex (maximum a) a) == 0 = "Noun"
      | fromMaybe (-1) (elemIndex (maximum a) a) == 1 = "Verb"
      | fromMaybe (-1) (elemIndex (maximum a) a) == 2 = "Adj"
      | fromMaybe (-1) (elemIndex (maximum a) a) == 3 = "Adv"
      | otherwise                                     = "Unknown"

wnPartPOS :: WordNetEnv -> String -> IO EPOS
wnPartPOS _ [] = return UnknownEPos
wnPartPOS w a  = do
    ind1 <- indexLookup w a Noun
    ind2 <- indexLookup w a Verb
    ind3 <- indexLookup w a Adj
    ind4 <- indexLookup w a Adv
    return (type' ((count' ind1) : (count' ind2) : (count' ind3) : (count' ind4) : []))
  where
    count' a = if isJust a then senseCount (fromJust a) else 0
    type' [] = UnknownEPos
    type' a
      | a == [0, 0, 0, 0]                             = UnknownEPos
      | fromMaybe (-1) (elemIndex (maximum a) a) == 0 = POS Noun
      | fromMaybe (-1) (elemIndex (maximum a) a) == 1 = POS Verb
      | fromMaybe (-1) (elemIndex (maximum a) a) == 2 = POS Adj
      | fromMaybe (-1) (elemIndex (maximum a) a) == 3 = POS Adv
      | otherwise                                     = UnknownEPos

wnGloss :: WordNetEnv -> String -> String -> IO String
wnGloss _ [] _ = return "Nothing!  Error!  Abort!"
wnGloss wne word [] = do
    wnPos <- wnPartString wne (fixUnderscore word)
    wnGloss wne word wnPos
wnGloss wne word pos = do
    let wnPos = fromEPOS $ readEPOS pos
    let result = map (getGloss . getSynset) (runs wne (search
                     (fixUnderscore word) wnPos AllSenses))
    if (null result) then return "Nothing!" else
      return $ unwords result

wnRelated :: WordNetEnv -> String -> String -> EPOS -> IO [String]
wnRelated wne [] _ _  = return [[]] :: IO [String]
wnRelated wne c [] pos  = wnRelated wne c "Hypernym" pos
wnRelated wne c d pos = do
    let wnForm = readForm d
    let result = if (map toLower d) == "all" then concat $ map (fromMaybe [[]])
                    (runs wne (relatedByListAllForms (search (fixUnderscore c)
                     (fromEPOS pos) AllSenses)))
                 else fromMaybe [[]] (runs wne (relatedByList wnForm (search
                      (fixUnderscore c) (fromEPOS pos) AllSenses)))
    if (null result) || (null $ concat result) then return [] else
      return $ map (\x -> replace '_' ' ' $ unwords $ map (++ "\"") $
                    map ('"' :) $ concat $ map (getWords . getSynset) x) result

wnClosure :: WordNetEnv -> String -> String -> String -> IO String
wnClosure _ [] _ _         = return []
wnClosure wne word [] _    = wnClosure wne word "Hypernym" []
wnClosure wne word form [] = do
    wnPos <- wnPartString wne (fixUnderscore word)
    wnClosure wne word form wnPos
wnClosure wne word form pos = do
    let wnForm = readForm form
    let wnPos = fromEPOS $ readEPOS pos
    let result = runs wne (closureOnList wnForm
                           (search (fixUnderscore word) wnPos AllSenses))
    if (null result) then return [] else
      return $ unwords $ map (\x -> if isNothing x then return '?'
                                    else (replace '_' ' ' $ unwords $ map (++ "\"") $
                                          map ('"' :) $ nub $ concat $ map
                                          (getWords . getSynset)
                                          (flatten (fromJust x)))) result

wnMeet :: WordNetEnv -> String -> String -> String -> IO String
wnMeet _ [] _ _ = return []
wnMeet _ _ [] _ = return []
wnMeet w c d [] = do
    wnPos <- wnPartString w (fixUnderscore c)
    wnMeet w c d wnPos
wnMeet w c d e  = do
    let wnPos = fromEPOS $ readEPOS e
    let r1 = runs w (search (fixUnderscore c) wnPos 1)
    let r2 = runs w (search (fixUnderscore d) wnPos 1)
    if not (null r1) && not (null r2) then do
        let result = runs w (meet emptyQueue (head $ r1) (head $ r2))
        if isNothing result then return [] else
            return $ replace '_' ' ' $ unwords $ map (++ "\"") $ map ('"' :) $
                    getWords $ getSynset $ fromJust result
        else return []

asSuggest :: Aspell.SpellChecker -> String -> IO String
asSuggest aspell word = do w <- Aspell.suggest aspell (ByteString.pack word)
                           return $ unwords $ map ByteString.unpack w

-- markov1 :: Map.Map String Word -> [String] -> Int -> Int -> [String] -> [String]
-- markov1 dict markov num runs words
--     | length (markov ++ words) < 3 = take num $ Markov.run runs startWords 0 (Random.mkStdGen 17)
--     | otherwise = take num $ Markov.run runs (nub $ mix markov
--                             [x | x <- words, Map.member x dict]) 0 (Random.mkStdGen 17)
--   where
--     mix a b = if (length b) < 2 then a else concat [[b, a] | (a, b) <- zip a (cycle b)]

sentence :: Fugly -> Int -> Int -> [String] -> [IO String]
sentence _ _ _ [] = [return []] :: [IO String]
sentence fugly@(dict, wne, aspell, _, _) num len msg = do
  let l = ["a", "the", "is", "are", "and", "i", "I"]
  let r = ["is", "are", "what", "when", "who", "where", "am"]
  let s1a x = do
      w <- s1b fugly len 0 (findNextWord fugly x 1)
      return $ nub $ filter (\x -> length x > 0) w
  let s1d x = do
      w <- x
      if null w then return []
        else return ((init w) ++ ((last w) ++ if elem (head w) r then "?" else ".") : [])
  let s1e x = do
      w <- x
      if null w then return []
        else return ((s1c w : [] ) ++ tail w)
  let s1f x = do
      w <- x
      if null w then return []
        else if elem (last w) l then return $ init w else return w
  map (\x -> do y <-x ; return $ unwords y) (map (s1e . s1d . s1f . s1a) msg)
  where
    s1b :: Fugly -> Int -> Int -> IO [String] -> IO [String]
    s1b f@(d, w, s, a, b) n i words = do
      ww <- words
      www <- findNextWord f (last ww) i
      if null ww then return []
        else if i >= n then return $ nub ww else
               s1b f n (i + 1) (return $ ww ++ www)
    s1c :: [String] -> String
    s1c [] = []
    s1c w = ((toUpper $ head $ head w) : []) ++ (tail $ head w)

findNextWord :: Fugly -> String -> Int -> IO [String]
findNextWord fugly@(dict, wne, aspell, _, _) word i = do
  a <- asSuggest aspell word
  let next3 = if null $ words a then [] else head $ words a
  let f = if isJust w then
        if null neigh then next3
        else if isJust ww then
          if elem next1 nextNeigh then
            next2
          else
            next1
              else
                next1
          else next3
  return $ replace "i" "I" $ words f
    where
      w = Map.lookup word dict
      ww = Map.lookup next1 dict
      neigh = listNeigh $ wordGetAfter (fromJust w)
      nextNeigh = listNeigh $ wordGetAfter (fromJust ww)
      next1 = neigh!!(mod i (length neigh))
      next2 = nextNeigh!!(mod i (length nextNeigh))
      related     = map (strip '"') $ wordGetRelated (fromJust w)
      relatedNext = map (strip '"') $ wordGetRelated (fromJust ww)

dictLookup :: Fugly -> String -> String -> IO String
dictLookup fugly@(_, wne, aspell, _, _) word pos = do
    gloss <- wnGloss wne word pos
    if gloss == "Nothing!" then do a <- asSuggest aspell word
                                   return (gloss ++ " Perhaps you meant: " ++ a)
      else return gloss
