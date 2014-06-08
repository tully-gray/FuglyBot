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
         listNamesCountSort2,
         wordIs,
         cleanStringWhite,
         cleanStringBlack,
         wnClosure,
         wnMeet,
         -- gfAll,
         -- gfRandom,
         gfParseBool,
         gfParseC,
         gfCategories,
         -- gfTranslate,
         sentence,
         joinWords,
         toUpperSentence,
         endSentence,
         dePlenk,
         Word (..),
         Fugly (..)
       )
       where

import Control.Concurrent
import Control.Exception
import qualified Data.ByteString.Char8 as ByteString
import Data.Char
import Data.Either
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Tree (flatten)
import qualified System.Random as Random
import System.IO
import System.IO.Error
import System.IO.Unsafe

import qualified Language.Aspell as Aspell
import qualified Language.Aspell.Options as Aspell.Options

import NLP.WordNet hiding (Word)
import NLP.WordNet.Prims (indexLookup, senseCount, getSynset, getWords, getGloss)
import NLP.WordNet.PrimTypes

import PGF

data Fugly = Fugly {
              dict    :: Map.Map String Word,
              pgf     :: PGF,
              wne     :: WordNetEnv,
              aspell  :: Aspell.SpellChecker,
              allow   :: [String],
              ban     :: [String]
              }

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
              place   :: String,
              count   :: Int,
              before  :: Map.Map String Int,
              after   :: Map.Map String Int,
              related :: [String]
              } |
            Phrase {
              phrase  :: String,
              count   :: Int,
              before  :: Map.Map String Int,
              after   :: Map.Map String Int,
              related :: [String]
              }

initFugly :: FilePath -> FilePath -> FilePath -> String -> IO Fugly
initFugly fuglydir wndir gfdir topic = do
    (dict, allow, ban) <- catchIOError (loadDict fuglydir topic)
                          (const $ return (Map.empty, [], []))
    pgf <- readPGF (gfdir ++ "/ParseEng.pgf")
    wne <- NLP.WordNet.initializeWordNetWithOptions
           (return wndir :: Maybe FilePath)
           (Just (\e f -> putStrLn (e ++ show (f :: SomeException))))
    -- a <- Aspell.spellCheckerWithOptions [Aspell.Options.Lang (ByteString.pack "en_US"), Aspell.Options.IgnoreCase False, Aspell.Options.Size Aspell.Options.Small, Aspell.Options.SuggestMode Aspell.Options.Fast]
    a <- Aspell.spellChecker
    let aspell = head $ rights [a]
    return (Fugly dict pgf wne aspell allow ban)

stopFugly :: FilePath -> Fugly -> String -> IO ()
stopFugly fuglydir fugly@(Fugly _ _ wne _ _ _) topic = do
    catchIOError (saveDict fugly fuglydir topic) (const $ return ())
    closeWordNet wne

saveDict :: Fugly -> FilePath -> String -> IO ()
saveDict fugly@(Fugly dict _ _ _ allow ban) fuglydir topic = do
    let d = Map.toList dict
    if null d then putStrLn "Empty dict!"
      else do
        h <- openFile (fuglydir ++ "/" ++ topic ++ "-dict.txt") WriteMode
        hSetBuffering h LineBuffering
        putStrLn "Saving dict file..."
        saveDict' h d
        hPutStrLn h ">END<"
        hPutStrLn h $ unwords $ sort allow
        hPutStrLn h $ unwords $ sort ban
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
                             ("pos: " ++ (show p) ++ "\n"),
                             ("end: \n")]
    format' m@(Name w c b a r)
      | null w    = []
      | otherwise = unwords [("name: " ++ w ++ "\n"),
                             ("count: " ++ (show c) ++ "\n"),
                             ("before: " ++ (unwords $ listNeigh2 b) ++ "\n"),
                             ("after: " ++ (unwords $ listNeigh2 a) ++ "\n"),
                             ("related: " ++ (unwords r) ++ "\n"),
                             ("end: \n")]

loadDict :: FilePath -> String -> IO (Map.Map String Word, [String], [String])
loadDict fuglydir topic = do
    let w = (Word [] 0 Map.empty Map.empty [] UnknownEPos)
    h <- openFile (fuglydir ++ "/" ++ topic ++ "-dict.txt") ReadMode
    eof <- hIsEOF h
    if eof then
      return (Map.empty, [], [])
      else do
      hSetBuffering h LineBuffering
      putStrLn "Loading dict file..."
      dict <- ff h w [([], w)]
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
    ff :: Handle -> Word -> [(String, Word)] -> IO [(String, Word)]
    ff h word nm = do
      l <- hGetLine h
      let wl = words l
      let l1 = length $ filter (\x -> x /= ' ') l
      let l2 = length wl
      let l3 = elem ':' l
      let l4 = if l1 > 3 && l2 > 0 && l3 == True then True else False
      let ll = if l4 == True then tail wl else ["BAD BAD BAD"]
      let ww = if l4 == True then case (head wl) of
                           "word:"    -> (Word (unwords ll) 0 Map.empty Map.empty [] UnknownEPos)
                           "name:"    -> (Name (unwords ll) 0 Map.empty Map.empty [])
                           -- "place:"   -> word{place=(unwords ll)}
                           -- "phrase:"  -> word{phrase=(unwords ll)}
                           "count:"   -> word{count=(read $ unwords $ ll)}
                           "before:"  -> word{before=(getNeigh $ ll)}
                           "after:"   -> word{after=(getNeigh $ ll)}
                           "related:" -> word{related=(joinWords '"' ll)}
                           "pos:"     -> word{FuglyLib.pos=(readEPOS $ unwords $ ll)}
                           "end:"     -> word
                           _          -> word
               else word
      if l4 == False then do putStrLn ("Oops: " ++ l) >> return nm
        else if (head wl) == "end:" then
               ff h ww (nm ++ ((wordGetWord ww), ww) : [])
             else if (head wl) == ">END<" then
                    return nm
                  else
                    ff h ww nm

qWords = ["if", "is", "are", "why", "what", "when", "who", "where", "want", "am"]
badEndWords = ["a", "the", "I", "I've", "I'll", "I'm", "i", "and", "are", "your", "you're", "you", "with", "was",
               "to", "in", "is", "as", "if", "do", "so", "am", "of", "for", "or", "he", "she", "they", "it's", "its"]

wordIs         (Word w c b a r p) = "word"
wordIs         (Name n c b a r)   = "name"
wordIs         (Place p c b a r)  = "place"
wordIs         (Phrase p c b a r) = "phrase"
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
insertWords (Fugly {dict=d}) [] = return d
insertWords fugly [x] = insertWord fugly x [] [] []
insertWords fugly@(Fugly dict pgf _ _ _ _) msg@(x:y:xs) =
  case (len) of
    2 -> do ff <- insertWord fugly x [] y []
            insertWord fugly{dict=ff} y x [] []
    _ -> insertWords' fugly 0 len msg
  where
    len = length msg
    insertWords' (Fugly {dict=d}) _ _ [] = return d
    insertWords' f@(Fugly {dict=d}) i l msg
      | i == 0     = do ff <- insertWord f (msg!!i) [] (msg!!(i+1)) []
                        insertWords' f{dict=ff} (i+1) l msg
      | i > l - 1  = return d
      | i == l - 1 = do ff <- insertWord f (msg!!i) (msg!!(i-1)) [] []
                        insertWords' f{dict=ff} (i+1) l msg
      | otherwise  = do ff <- insertWord f (msg!!i) (msg!!(i-1)) (msg!!(i+1)) []
                        insertWords' f{dict=ff} (i+1) l msg

insertWord :: Fugly -> String -> String -> String -> String -> IO (Map.Map String Word)
insertWord fugly@(Fugly dict pgf wne aspell allow ban) [] _ _ _ = return dict
insertWord fugly@(Fugly dict pgf wne aspell allow ban) word before after pos =
    if elem word ban || elem before ban || elem after ban then return dict
    else if isJust w then f $ fromJust w
         else if isJust ww then insertWordRaw' fugly ww (cw word) bi ai pos
              else insertWordRaw' fugly w (cw word) bi ai pos
  where
    -- cw m = map toLower $ strip ':' $ strip ';' $ strip ',' $ strip '.' $ strip '!' $ strip '?' m
    cw m = strip ':' $ strip ';' $ strip ',' $ strip '.' $ strip '!' $ strip '?' m
    w = Map.lookup word dict
    ww = Map.lookup (cw word) dict
    a = Map.lookup after dict
    b = Map.lookup before dict
    ai = if isJust a then after else cw after
    bi = if isJust b then before else cw before
    f (Word {})  = insertWordRaw' fugly w word bi ai pos
    f (Name {})  = insertName'    fugly w word bi ai

insertWordRaw f@(Fugly {dict=d}) w b a p = insertWordRaw' f (Map.lookup w d) w b a p
insertWordRaw' :: Fugly -> Maybe Word -> String -> String
                 -> String -> String -> IO (Map.Map String Word)
insertWordRaw' (Fugly {dict=d}) _ [] _ _ _ = return d
insertWordRaw' (Fugly dict _ wne aspell allow _) w word before after pos = do
  pp <- (if null pos then wnPartPOS wne word else return $ readEPOS pos)
  pa <- wnPartPOS wne after
  pb <- wnPartPOS wne before
  rel <- wnRelated wne word "Hypernym" pp
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

insertName f@(Fugly {dict=d}) w b a = insertName' f (Map.lookup w d) w b a
insertName' :: Fugly -> Maybe Word -> String -> String
              -> String -> IO (Map.Map String Word)
insertName' (Fugly dict _ wne aspell _ _) _ [] _ _ = return dict
insertName' (Fugly dict _ wne aspell allow _) w name before after = do
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

listNeighLeast :: Map.Map String Int -> [String]
listNeighLeast m = [w | (w, c) <- Map.toList m, c == minimum [c | (w, c) <- Map.toList m]]

listWords :: Map.Map String Word -> [String]
listWords m = map wordGetWord $ Map.elems m

listWordsCountSort :: Map.Map String Word -> [String]
listWordsCountSort m = reverse $ map snd (sort $ map wordGetwc $ Map.elems m)

listWordsCountSort2 :: Map.Map String Word -> Int -> [String]
listWordsCountSort2 m num = concat [[w, show c, ";"] | (c, w) <- take num $ reverse $
                            sort $ map wordGetwc $ Map.elems m]

listNamesCountSort2 :: Map.Map String Word -> Int -> [String]
listNamesCountSort2 m num = concat [[w, show c, ";"] | (c, w) <- take num $ reverse $
                            sort $ map wordGetwc $ filter (\x -> wordIs x == "name") $ Map.elems m]

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

cleanStringWhite :: (Char -> Bool) -> String -> String
cleanStringWhite _ [] = []
cleanStringWhite f (x:xs)
        | not $ f x =     cleanStringWhite f xs
        | otherwise = x : cleanStringWhite f xs

cleanStringBlack :: (Char -> Bool) -> String -> String
cleanStringBlack _ [] = []
cleanStringBlack f (x:xs)
        | f x       =     cleanStringBlack f xs
        | otherwise = x : cleanStringBlack f xs

dePlenk :: String -> String
dePlenk []  = []
dePlenk s   = dePlenk' s []
  where
    dePlenk' [] l  = l
    dePlenk' [x] l = l ++ x:[]
    dePlenk' a@(x:xs) l
      | length a == 1                             = l ++ x:[]
      | x == ' ' && (h == ' ' || isPunctuation h) = dePlenk' (tail xs) (l ++ h:[])
      | otherwise                                 = dePlenk' xs (l ++ x:[])
      where
        h = fHead "dePlenk" '!' xs

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
    | (fHead "joinWords" ' ' x) == a = unwords (x : (take num xs)) : joinWords a (drop num xs)
    | otherwise                     = x : joinWords a xs
  where num = (fromMaybe 0 (elemIndex a $ map (\x -> fLast "joinWords" '!' x) xs)) + 1

fixUnderscore :: String -> String
fixUnderscore = strip '"' . replace ' ' '_'

toUpperSentence :: [String] -> [String]
toUpperSentence []  = []
toUpperSentence msg = (up' msg : [] ) ++ tail msg
  where
    up' w = ((toUpper $ head $ head w) : []) ++ (tail $ head w)

endSentence :: [String] -> [String]
endSentence []  = []
endSentence msg = (init msg) ++ ((fLast "endSentence" [] msg) ++ if elem (head msg) qWords then "?" else ".") : []

fHead a b [] = unsafePerformIO (do putStrLn ("fHead: error in " ++ a) ; return b)
fHead a b c  = head c
fLast a b [] = unsafePerformIO (do putStrLn ("fLast: error in " ++ a) ; return b)
fLast a b c  = last c

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

-- LD_PRELOAD=/usr/lib64/libjemalloc.so.1
asSuggest :: Aspell.SpellChecker -> String -> IO String
asSuggest aspell word = runInBoundThread (do w <- Aspell.suggest aspell (ByteString.pack word)
                                             return $ unwords $ map ByteString.unpack w)

{--
gfRandom :: Fugly -> Int -> Int -> IO String
gfRandom fugly lim num = do r <- gfRandom' fugly lim
                            return $ unwords $ toUpperSentence $ endSentence $ words $ cleanStringBlack isDigit $
                              cleanStringBlack isPunctuation $ unwords $ take num r
  where
    gfRandom' :: Fugly -> Int -> IO [String]
    gfRandom' (Fugly {pgf=p}) lim = do
      r <- Random.newStdGen
      let rr = generateRandomDepth r p (startCat p) (Just lim)
      return (if null rr then [] else words $ linearize p (head $ languages p) (head rr))

gfAll :: PGF -> Int -> String
gfAll pgf num = unwords $ toUpperSentence $ endSentence $ take 15 $ words $ linearize pgf (head $ languages pgf) ((generateAllDepth pgf (startCat pgf) (Just 3))!!num)

gfTranslate :: PGF -> String -> String
gfTranslate pgf s = case parseAllLang pgf (startCat pgf) s of
    (lg,t:_):_ -> unlines [linearize pgf l t | l <- languages pgf, l /= lg]
    _          -> "Me no understand Engrish."
--}

gfParseBool :: PGF -> String -> Bool
gfParseBool pgf msg
  | elem (last w) badEndWords = False
  | length w > 5  = (gfParseBoolA pgf $ take 5 w) && (gfParseBool pgf (unwords $ drop 5 w))
  | otherwise     = gfParseBoolA pgf w
    where
      w = words msg

gfParseBoolA :: PGF -> [String] -> Bool
gfParseBoolA pgf msg
  | null msg                                 = False
  | null $ parse pgf lang (startCat pgf) m   = False
  | otherwise                                = True
  where
    m = unwords msg
    lang = head $ languages pgf

{--
gfParseBool2 :: PGF -> String -> Bool
gfParseBool2 pgf msg = lin pgf lang (parse_ pgf lang (startCat pgf) Nothing msg)
  where
    lin pgf lang (ParseOk tl, _)      = True
    lin pgf lang _                    = False
    lang = head $ languages pgf
--}

gfParseC :: PGF -> String -> [String]
gfParseC pgf msg = lin pgf lang (parse_ pgf lang (startCat pgf) Nothing msg)
  where
    lin pgf lang (ParseOk tl, b)      = map (lin' pgf lang) tl
    lin pgf lang (ParseFailed a, b)   = ["parse failed at " ++ show a ++
                                        " tokens: " ++ showBracketedString b]
    lin pgf lang (ParseIncomplete, b) = ["parse incomplete: " ++ showBracketedString b]
    lin pgf lang _                    = ["No parse!"]
    lin' pgf lang t = "parse: " ++ showBracketedString (bracketedLinearize pgf lang t)
    lang = head $ languages pgf

gfCategories :: PGF -> [String]
gfCategories pgf = map showCId (categories pgf)

sentence :: Fugly -> Int -> Int -> [String] -> [IO String]
sentence _ _ _ [] = [return []] :: [IO String]
sentence fugly@(Fugly dict pgf wne aspell _ ban) stries slen msg = do
  let s1f x = if null x then return []
              else if gfParseBool pgf (unwords x) && length x > 2 then return x else return []
  let s1a x = do
      w <- s1b fugly slen 2 $ findNextWord fugly x 0
      putStrLn ("DEBUG > " ++ unwords w)
      s1f ([x] ++ (filter (\y -> length y > 0 && not (elem y ban)) w))
  let s1d x = do
      w <- x
      if null w then return []
        else return ((init w) ++ ((fLast "sentence: s1d" [] w) ++
                                  if elem (head w) qWords then "?" else ".") : [])
  let s1e x = do
      w <- x
      if null w then return []
        else return ((s1c w : [] ) ++ tail w)
  take stries $ map (\x -> do y <- x ; return $ dePlenk $ unwords y) (map (s1e . s1d . s1a) (cycle msg))
  where
    s1b :: Fugly -> Int -> Int -> IO [String] -> IO [String]
    s1b f@(Fugly d p w s a b) n i msg = do
      ww <- msg
      if null ww then return []
        else if i >= n then return $ nub ww else do
               www <- findNextWord f (fLast "sentence: s1b" [] ww) i
               s1b f n (i + 1) (return $ ww ++ www)
    s1c :: [String] -> String
    s1c [] = []
    s1c w = ((toUpper $ head $ head w) : []) ++ (tail $ head w)

findNextWord :: Fugly -> String -> Int -> IO [String]
findNextWord fugly@(Fugly dict pgf wne aspell _ _) word i = do
  let ln       = if isJust w then length neigh else 0
  let lm       = if isJust w then length neighmax else 0
  let ll       = if isJust w then length neighleast else 0
  nr <- Random.getStdRandom (Random.randomR (0, ln - 1))
  mr <- Random.getStdRandom (Random.randomR (0, lm - 1))
  lr <- Random.getStdRandom (Random.randomR (0, ll - 1))
  let ff = if isJust w && (length neigh > 0) then case mod i 5 of
        0 -> neigh!!nr
        1 -> neighleast!!lr
        2 -> neighmax!!mr
        3 -> neigh!!nr
        4 -> neighleast!!lr
        _ -> "Doesn't happen!"
           else []
  return $ replace "i" "I" $ words ff
    where
      w          = Map.lookup word dict
      neigh      = listNeigh $ wordGetAfter (fromJust w)
      neighmax   = listNeighMax $ wordGetAfter (fromJust w)
      neighleast = listNeighLeast $ wordGetAfter (fromJust w)

dictLookup :: Fugly -> String -> String -> IO String
dictLookup fugly@(Fugly _ _ wne aspell _ _) word pos = do
    gloss <- wnGloss wne word pos
    if gloss == "Nothing!" then
       do a <- asSuggest aspell word
          return (gloss ++ " Perhaps you meant: " ++
                  (unwords (filter (\x -> x /= word) (words a))))
      else return gloss
