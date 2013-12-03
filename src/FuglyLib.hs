module FuglyLib
       (
         initFugly,
         stopFugly,
         loadDict,
         saveDict,
         insertWord,
         insertWords,
         listWords,
         listWordFull,
         listWordsCountSort,
         listWordsCountSort2,
         cleanString,
         wnClosure,
         wnGloss,
         wnMeet,
         s1,
         s2,
         Word,
         Fugly
       )
       where

import Control.Exception
import Data.Char (isAlpha, isAlphaNum, toLower)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Tree (flatten)
import System.Random as Random
import System.IO
import System.IO.Error

import qualified Data.MarkovChain as Markov

import NLP.WordNet hiding (Word)
import NLP.WordNet.Prims (indexLookup, senseCount, getSynset, getWords, getGloss)
import NLP.WordNet.PrimTypes

type Fugly = (Map.Map String Word, WordNetEnv)

data Word = Word {
              word    :: String,
              count   :: Int,
              before  :: Map.Map String Int,
              after   :: Map.Map String Int,
              related :: [String],
              pos     :: EPOS
              } |
            Name {
              word    :: String,
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
    dict <- catchIOError (loadDict fuglydir) (const $ return Map.empty)
    wne <- NLP.WordNet.initializeWordNetWithOptions
           (return wndir :: Maybe FilePath)
           (Just (\e f -> putStrLn (e ++ show (f :: SomeException))))
    return (dict, wne)

stopFugly :: FilePath -> Fugly -> IO ()
stopFugly fuglydir fugly@(dict, wne) = do
    catchIOError (saveDict dict fuglydir) (const $ return ())
    closeWordNet wne

saveDict :: Map.Map String Word -> FilePath -> IO ()
saveDict dict fuglydir = do
    let d = Map.toList dict
    if null d then putStrLn "Empty dict!"
      else do
        h <- openFile (fuglydir ++ "/dict.txt") WriteMode
        hSetBuffering h LineBuffering
        putStrLn "Saving dict file..."
        saveDict' h d
        hPutStrLn h ">END<"
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

loadDict :: FilePath -> IO (Map.Map String Word)
loadDict fuglydir = do
    let w = (Word [] 0 Map.empty Map.empty [] UnknownEPos)
    h <- openFile (fuglydir ++ "/dict.txt") ReadMode
    eof <- hIsEOF h
    if eof then
      return Map.empty
      else do
      hSetBuffering h LineBuffering
      putStrLn "Loading dict file..."
      ff <- f h w [([], w)]
      let m = Map.fromList ff
      hClose h
      return m
  where
    getNeigh :: [String] -> Map.Map String Int
    getNeigh a = Map.fromList $ getNeigh' a []
    getNeigh' :: [String] -> [(String, Int)] -> [(String, Int)]
    getNeigh'        [] l = l
    getNeigh' (x:y:xs) [] = getNeigh' xs [(x, read y)]
    getNeigh' (x:y:xs)  l = getNeigh' xs (l ++ (x, read y) : [])
    f :: Handle -> Word -> [(String, Word)] -> IO [(String, Word)]
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
                           "count:"   -> (Word w (read $ unwords $ ll) b a r p)
                           "before:"  -> (Word w c (getNeigh $ ll) a r p)
                           "after:"   -> (Word w c b (getNeigh $ ll) r p)
                           "related:" -> (Word w c b a (joinWords '"' ll) p)
                           "pos:"     -> (Word w c b a r
                                          (readEPOS $ unwords $ ll))
                           _          -> (Word w c b a r p)
               else (Word w c b a r p)
      if l4 == False then do putStrLn "hmm..." >> return nm
        else if (head wl) == "pos:" then
               f h ww (nm ++ (((\x@(Word w _ _ _ _ _) -> w) ww), ww) : [])
             else if (head wl) == ">END<" then
                    return nm
                  else
                    f h ww nm

insertWords :: Fugly -> [String] -> IO (Map.Map String Word)
insertWords f@(dict, wne) [] = return dict
insertWords f [x] = insertWord f x [] [] []
insertWords f@(dict, wne) msg@(x:y:xs) =
      case (len) of
        2 -> do ff <- insertWord f x [] y [] ; insertWord (ff, wne) y x [] []
        _ -> insertWords' f 0 len msg
  where
    len = length msg
    insertWords' f@(d,w) _ _ [] = return d
    insertWords' f@(d,w) a l msg
      | a == 0     = do ff <- insertWord f (msg!!a) [] (msg!!(a+1)) []
                        insertWords' (ff, wne) (a+1) l msg
      | a > l - 1  = return d
      | a == l - 1 = do ff <- insertWord f (msg!!a) (msg!!(a-1)) [] []
                        insertWords' (ff, wne) (a+1) l msg
      | otherwise  = do ff <- insertWord f (msg!!a) (msg!!(a-1)) (msg!!(a+1)) []
                        insertWords' (ff, wne) (a+1) l msg

insertWord :: Fugly -> String -> String -> String -> String -> IO (Map.Map String Word)
insertWord f@(dict, wne) [] _ _ _ = return dict
insertWord f@(dict, wne) word before after pos = do
  pp <- (if null pos then wnPartPOS wne ww else return $ readEPOS pos)
  pa <- wnPartPOS wne aa
  pb <- wnPartPOS wne bb
  rel <- wnRelated wne ww "Hypernym" pp
  if isJust w then
    return $ Map.insert ww (Word ww c (nb bb pb) (na aa pa) ((\x@(Word _ _ _ _ r _) -> r) (fromJust w))
                                   (((\x@(Word _ _ _ _ _ p) -> p)) (fromJust w))) dict
         else if elem ww allowedWords then
           return $ Map.insert ww (Word ww 1 (e (nn bb pb)) (e (nn aa pa)) rel pp) dict
           else if pp == UnknownEPos || (length ww < 3) then
                  return dict
                  else
                  return $ Map.insert ww (Word ww 1 (e (nn bb pb))
                                          (e (nn aa pa)) rel pp) dict
  where
    w = Map.lookup ww dict
    wa = (\x@(Word _ _ _ a _ _) -> a) (fromJust w)
    wb = (\x@(Word _ _ b _ _ _) -> b) (fromJust w)
    e [] = Map.empty
    e x = Map.singleton x 1
    aa = map toLower $ cleanString isAlpha after
    bb = map toLower $ cleanString isAlpha before
    ww = map toLower $ cleanString isAlpha word
    c = incCount' (fromJust w)
    na x y = if elem x allowedWords then incAfter' (fromJust w) x
                else if y == UnknownEPos || (length x < 3) then wa
                     else incAfter' (fromJust w) x
    nb x y = if elem x allowedWords then incBefore' (fromJust w) x
                else if y == UnknownEPos || (length x < 3) then wb
                     else incBefore' (fromJust w) x
    nn x y  = if elem x allowedWords then x
                else if y == UnknownEPos || (length x < 3) then [] else x

allowedWords :: [String]
allowedWords = ["a", "i", "to", "go", "me", "no", "you", "her", "him", "got", "get",
                "had", "have", "has", "it", "the", "them", "there", "their", "what",
                "that", "this", "where", "were", "in", "on", "at", "is", "was",
                "could", "would", "for", "us", "we", "do", "did", "if", "anyone"]

incCount' :: Word -> Int
incCount' w@(Word _ c _ _ _ _) = c + 1

incBefore' :: Word -> String -> Map.Map String Int
incBefore' word@(Word _ _ b _ _ _) []     = b
incBefore' word@(Word _ _ b _ _ _) before =
  if isJust w then
    Map.insert before ((fromJust w) + 1) b
    else
    Map.insert before 1 b
  where
    w = Map.lookup before b

incBefore :: Map.Map String Word -> String -> String -> Map.Map String Int
incBefore m word before = do
  let w = Map.lookup word m
  if isJust w then incBefore' (fromJust w) before
    else Map.empty

incAfter' :: Word -> String -> Map.Map String Int
incAfter' word@(Word _ _ _ a _ _) []     = a
incAfter' word@(Word _ _ _ a _ _) after  =
  if isJust w then
    Map.insert after ((fromJust w) + 1) a
    else
    Map.insert after 1 a
  where
    w = Map.lookup after a

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
listNeighMax2 m = concat [[w, show c] | (w, c) <- Map.toList m, c == maximum [c | (w, c) <- Map.toList m]]

listWords :: Map.Map String Word -> [String]
listWords m = map (\x@(Word w _ _ _ _ _) -> w) $ Map.elems m

listWordsCountSort :: Map.Map String Word -> [String]
listWordsCountSort m = reverse $ map snd (sort $ map (\x@(Word w c _ _ _ _) -> (c, w)) $ Map.elems m)

listWordsCountSort2 :: Map.Map String Word -> Int -> [String]
listWordsCountSort2 m num = concat [[w, show c, ";"] | (c, w) <- take num $ reverse $
                            sort $ map (\x@(Word w c _ _ _ _)
                                                   -> (c, w)) $ Map.elems m]

listWordFull :: Map.Map String Word -> String -> String
listWordFull m word =
  if isJust ww then
    unwords $ (\x@(Word w c b a r p) ->
                ["word:", w, "count:", show c, " before:", unwords $ listNeigh2 b,
                 " after:", unwords $ listNeigh2 a, " pos:", (show p),
                 " related:", unwords r]) (fromJust ww)
  else
    "Nothing!"
  where
    ww = Map.lookup word m

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

wnFixWord :: String -> String
wnFixWord = strip '"' . replace ' ' '_'

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
    wnPos <- wnPartString wne (wnFixWord word)
    wnGloss wne word wnPos
wnGloss wne word pos = do
    let wnPos = fromEPOS $ readEPOS pos
    let result = map (getGloss . getSynset) (runs wne (search
                     (wnFixWord word) wnPos AllSenses))
    if (null result) then return [] else
      return $ unwords result

wnRelated :: WordNetEnv -> String -> String -> EPOS -> IO [String]
wnRelated wne [] _ _  = return [[]] :: IO [String]
wnRelated wne c [] pos  = wnRelated wne c "Hypernym" pos
wnRelated wne c d pos = do
    let wnForm = readForm d
    let result = if (map toLower d) == "all" then concat $ map (fromMaybe [[]])
                    (runs wne (relatedByListAllForms (search (wnFixWord c)
                     (fromEPOS pos) AllSenses)))
                 else fromMaybe [[]] (runs wne (relatedByList wnForm (search
                      (wnFixWord c) (fromEPOS pos) AllSenses)))
    if (null result) || (null $ concat result) then return [] else
      return $ map (\x -> replace '_' ' ' $ unwords $ map (++ "\"") $
                    map ('"' :) $ concat $ map (getWords . getSynset) x) result

wnClosure :: WordNetEnv -> String -> String -> String -> IO String
wnClosure _ [] _ _         = return []
wnClosure wne word [] _    = wnClosure wne word "Hypernym" []
wnClosure wne word form [] = do
    wnPos <- wnPartString wne (wnFixWord word)
    wnClosure wne word form wnPos
wnClosure wne word form pos = do
    let wnForm = readForm form
    let wnPos = fromEPOS $ readEPOS pos
    let result = runs wne (closureOnList wnForm
                           (search (wnFixWord word) wnPos AllSenses))
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
    wnPos <- wnPartString w (wnFixWord c)
    wnMeet w c d wnPos
wnMeet w c d e  = do
    let wnPos = fromEPOS $ readEPOS e
    let r1 = runs w (search (wnFixWord c) wnPos 1)
    let r2 = runs w (search (wnFixWord d) wnPos 1)
    if not (null r1) && not (null r2) then do
        let result = (runs w (meet emptyQueue (head $ r1) (head $ r2)))
        if (isNothing result) then return [] else
            return (replace '_' ' ' $ unwords $ map (++ "\"") $ map ('"' :) $
                    getWords $ getSynset (fromJust result))
        else return []

s1 m num runs = take num $ Markov.run runs (listWords m) 0 (Random.mkStdGen 123)

s2 :: Map.Map String Word -> Int -> String -> [String]
s2 m num word = filter (\x -> length x > 0)
                (word : (s2' m num 0 ((findNextWord1 m word 1) : [])))
  where
    s2' :: Map.Map String Word -> Int -> Int -> [String] -> [String]
    s2' m num i words
      | i == num  = words
      | otherwise = s2' m num (i + 1) (words ++ [(findNextWord1 m (last words) i)])

findNextWord1 :: Map.Map String Word -> String -> Int -> String
findNextWord1 m word i =
  if isJust w then
    if null neigh then []
    else if isJust ww then
      if elem next nextNeigh then
        nextNeigh!!(mod i (length nextNeigh))
      else
        next
          else
            next
      else []
  where
    w = Map.lookup word m
    neigh = listNeigh $ (\x@(Word _ _ _ a _ _) -> a) (fromJust w)
    next = neigh!!(mod i (length neigh))
    ww = Map.lookup next m
    nextNeigh = listNeigh $ (\x@(Word _ _ _ a _ _) -> a) (fromJust ww)


    --related = map (strip '"') ((\x@(Word _ _ _ _ r _) -> r) (fromJust ww))
