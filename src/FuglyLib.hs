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
         ageWord,
         ageWords,
         fixWords,
         listWords,
         listWordFull,
         listWordsCountSort,
         listWordsCountSort2,
         listNamesCountSort2,
         wordIs,
         cleanStringWhite,
         cleanStringBlack,
         cleanString,
         wnRelated,
         wnClosure,
         wnMeet,
         asReplaceWords,
         -- gfAll,
         -- gfRandom,
         gfParseBool,
         gfParseC,
         gfCategories,
         -- gfTranslate,
         sentence,
         chooseWord,
         findRelated,
         joinWords,
         toUpperSentence,
         endSentence,
         dePlenk,
         fHead,
         fLast,
         fTail,
         Word (..),
         Fugly (..)
       )
       where

-- import Control.Concurrent
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
import System.IO.Unsafe
-- import Text.Regex.Posix

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
              ban     :: [String],
              match   :: [String]
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
              }

initFugly :: FilePath -> FilePath -> FilePath -> String -> IO Fugly
initFugly fuglydir wndir gfdir topic = do
    (dict', allow', ban', match') <- catch (loadDict fuglydir topic)
                                     (\e -> do let err = show (e :: SomeException)
                                               hPutStrLn stderr ("init fugly: " ++ err)
                                               return (Map.empty, [], [], []))
    pgf' <- readPGF (gfdir ++ "/ParseEng.pgf")
    wne' <- NLP.WordNet.initializeWordNetWithOptions
            (return wndir :: Maybe FilePath)
            (Just (\e f -> hPutStrLn stderr (e ++ show (f :: SomeException))))
    a <- Aspell.spellCheckerWithOptions [Aspell.Options.Lang (ByteString.pack "en_US"),
                                         Aspell.Options.IgnoreCase False, Aspell.Options.Size Aspell.Options.Large,
                                         Aspell.Options.SuggestMode Aspell.Options.Normal]
    let aspell' = head $ rights [a]
    return (Fugly dict' pgf' wne' aspell' allow' ban' match')

stopFugly :: FilePath -> Fugly -> String -> IO ()
stopFugly fuglydir fugly@(Fugly {wne=wne'}) topic = do
    catch (saveDict fugly fuglydir topic) (\e -> do let err = show (e :: SomeException)
                                                    hPutStrLn stderr ("stop fugly: " ++ err)
                                                    return ())
    closeWordNet wne'

saveDict :: Fugly -> FilePath -> String -> IO ()
saveDict (Fugly dict' _ _ _ allow' ban' match') fuglydir topic = do
    let d = Map.toList dict'
    if null d then hPutStrLn stderr "Empty dict!"
      else do
        h <- openFile (fuglydir ++ "/" ++ topic ++ "-dict.txt") WriteMode
        hSetBuffering h LineBuffering
        hPutStrLn stdout "Saving dict file..."
        saveDict' h d
        hPutStrLn h ">END<"
        hPutStrLn h $ unwords $ sort allow'
        hPutStrLn h $ unwords $ sort ban'
        hPutStrLn h $ unwords $ sort match'
        hClose h
  where
    saveDict' :: Handle -> [(String, Word)] -> IO ()
    saveDict' _ [] = return ()
    saveDict' h (x:xs) = do
      let l = format' $ snd x
      if null l then return () else hPutStr h l
      saveDict' h xs
    format' (Word w c b a r p)
      | null w    = []
      | otherwise = unwords [("word: " ++ w ++ "\n"),
                             ("count: " ++ (show c) ++ "\n"),
                             ("before: " ++ (unwords $ listNeigh2 b) ++ "\n"),
                             ("after: " ++ (unwords $ listNeigh2 a) ++ "\n"),
                             ("related: " ++ (unwords r) ++ "\n"),
                             ("pos: " ++ (show p) ++ "\n"),
                             ("end: \n")]
    format' (Name w c b a r)
      | null w    = []
      | otherwise = unwords [("name: " ++ w ++ "\n"),
                             ("count: " ++ (show c) ++ "\n"),
                             ("before: " ++ (unwords $ listNeigh2 b) ++ "\n"),
                             ("after: " ++ (unwords $ listNeigh2 a) ++ "\n"),
                             ("related: " ++ (unwords r) ++ "\n"),
                             ("end: \n")]

loadDict :: FilePath -> String -> IO (Map.Map String Word, [String], [String], [String])
loadDict fuglydir topic = do
    let w = (Word [] 0 Map.empty Map.empty [] UnknownEPos)
    h <- openFile (fuglydir ++ "/" ++ topic ++ "-dict.txt") ReadMode
    eof <- hIsEOF h
    if eof then
      return (Map.empty, [], [], [])
      else do
      hSetBuffering h LineBuffering
      hPutStrLn stdout "Loading dict file..."
      dict' <- ff h w [([], w)]
      allow' <- hGetLine h
      ban' <- hGetLine h
      match' <- hGetLine h
      let out = (Map.fromList dict', words allow', words ban', words match')
      hClose h
      return out
  where
    getNeigh :: [String] -> Map.Map String Int
    getNeigh a = Map.fromList $ getNeigh' a []
    getNeigh' :: [String] -> [(String, Int)] -> [(String, Int)]
    getNeigh'        [] l = l
    getNeigh' (x:y:xs) [] = getNeigh' xs [(x, fReadInt "getNeigh1" 0 y)]
    getNeigh' (x:y:xs)  l = getNeigh' xs (l ++ (x, fReadInt "getNeigh2" 0 y) : [])
    getNeigh'         _ l = l
    ff :: Handle -> Word -> [(String, Word)] -> IO [(String, Word)]
    ff h word' nm = do
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
                           "count:"   -> word'{count=(fReadInt "ff" 0 $ unwords $ ll)}
                           "before:"  -> word'{FuglyLib.before=(getNeigh $ ll)}
                           "after:"   -> word'{FuglyLib.after=(getNeigh $ ll)}
                           "related:" -> word'{related=(joinWords '"' ll)}
                           "pos:"     -> word'{FuglyLib.pos=(readEPOS $ unwords $ ll)}
                           "end:"     -> word'
                           _          -> word'
               else word'
      if l4 == False then do hPutStrLn stderr ("Oops: " ++ l) >> return nm
        else if (head wl) == "end:" then
               ff h ww (nm ++ ((wordGetWord ww), ww) : [])
             else if (head wl) == ">END<" then
                    return nm
                  else
                    ff h ww nm

qWords :: [String]
qWords = ["if", "is", "does", "are", "do", "why", "what", "when", "who", "where", "want", "am", "can", "will"]

badEndWords :: [String]
badEndWords = ["a", "the", "I", "I've", "I'll", "I'm", "I'd", "i", "and", "are", "an", "your", "you're", "you", "who", "with", "was",
               "to", "in", "is", "as", "if", "do", "so", "am", "of", "for", "or", "he", "she", "they", "they're", "we", "it", "it's",
               "its", "from", "go", "my", "that", "that's", "whose", "when", "what", "has", "had", "make", "makes", "person's", "but",
               "our", "their", "than", "at", "on", "into", "just", "by", "he's", "she's", "mr", "Mr"]

class Word_ a where
  wordIs :: a -> String
  wordGetWord :: a -> String
  wordGetCount :: a -> Int
  wordGetAfter :: a -> Map.Map String Int
  wordGetBefore :: a -> Map.Map String Int
  wordGetRelated :: a -> [String]
  wordGetPos :: a -> EPOS
  wordGetwc :: a -> (Int, String)

instance Word_ Word where
  wordIs (Word {}) = "word"
  wordIs (Name {}) = "name"
  wordGetWord (Word {word=w}) = w
  wordGetWord (Name {name=n}) = n
  wordGetCount (Word {count=c}) = c
  wordGetCount _                = 0
  wordGetAfter (Word {after=a}) = a
  wordGetAfter _                = Map.empty
  wordGetBefore (Word {before=b}) = b
  wordGetBefore _                 = Map.empty
  wordGetRelated (Word {related=r}) = r
  wordGetRelated _                  = []
  wordGetPos (Word {FuglyLib.pos=p}) = p
  wordGetPos _                       = UnknownEPos
  wordGetwc (Word w c _ _ _ _) = (c, w)
  wordGetwc (Name w c _ _ _)   = (c, w)

insertWords :: Fugly -> Bool -> [String] -> IO (Map.Map String Word)
insertWords (Fugly {dict=d}) _ [] = return d
insertWords fugly autoname [x] = insertWord fugly autoname x [] [] []
insertWords fugly autoname msg@(x:y:_) =
  case (len) of
    2 -> do ff <- insertWord fugly autoname x [] y []
            insertWord fugly{dict=ff} autoname y x [] []
    _ -> insertWords' fugly autoname 0 len msg
  where
    len = length msg
    insertWords' (Fugly {dict=d}) _ _ _ [] = return d
    insertWords' f@(Fugly {dict=d}) a i l m
      | i == 0     = do ff <- insertWord f a (m!!i) [] (m!!(i+1)) []
                        insertWords' f{dict=ff} a (i+1) l m
      | i > l - 1  = return d
      | i == l - 1 = do ff <- insertWord f a (m!!i) (m!!(i-1)) [] []
                        insertWords' f{dict=ff} a (i+1) l m
      | otherwise  = do ff <- insertWord f a (m!!i) (m!!(i-1)) (m!!(i+1)) []
                        insertWords' f{dict=ff} a (i+1) l m

insertWord :: Fugly -> Bool -> String -> String -> String -> String -> IO (Map.Map String Word)
insertWord (Fugly {dict=d}) _ [] _ _ _ = return d
insertWord fugly@(Fugly {dict=dict', aspell=aspell', ban=ban'}) autoname word' before' after' pos' = do
    n  <- asIsName aspell' word'
    nb <- asIsName aspell' before'
    na <- asIsName aspell' after'
    let out = if elem word' ban' || elem before' ban' || elem after' ban' then return dict'
              else if isJust w then f nb na $ fromJust w
                   else if n && autoname then insertName' fugly w (toUpperWord $ cleanString word') (bi nb) (ai na)
                        else if isJust ww then insertWordRaw' fugly ww (map toLower $ cleanString word') (bi nb) (ai na) pos'
                             else insertWordRaw' fugly w (map toLower $ cleanString word') (bi nb) (ai na) pos'
    out
  where
    w = Map.lookup word' dict'
    ww = Map.lookup (map toLower $ cleanString word') dict'
    a = Map.lookup after' dict'
    b = Map.lookup before' dict'
    ai an = if isJust a then after'
            else if an && autoname then toUpperWord $ cleanString after'
                 else map toLower $ cleanString after'
    bi bn = if isJust b then before'
            else if bn && autoname then toUpperWord $ cleanString before'
                 else map toLower $ cleanString before'
    f bn an (Word {}) = insertWordRaw' fugly w word' (bi bn) (ai an) pos'
    f bn an (Name {}) = insertName'    fugly w word' (bi bn) (ai an)

insertWordRaw :: Fugly -> String -> String -> String -> String -> IO (Map.Map String Word)
insertWordRaw f@(Fugly {dict=d}) w b a p = insertWordRaw' f (Map.lookup w d) w b a p

insertWordRaw' :: Fugly -> Maybe Word -> String -> String
                 -> String -> String -> IO (Map.Map String Word)
insertWordRaw' (Fugly {dict=d}) _ [] _ _ _ = return d
insertWordRaw' (Fugly dict' _ wne' aspell' allow' _ _) w word' before' after' pos' = do
  pp <- (if null pos' then wnPartPOS wne' word' else return $ readEPOS pos')
  pa <- wnPartPOS wne' after'
  pb <- wnPartPOS wne' before'
  rel <- wnRelated' wne' word' "Hypernym" pp
  let nn x y  = if elem x allow' then x
                else if y == UnknownEPos && Aspell.check aspell'
                        (ByteString.pack x) == False then [] else x
  let i x = Map.insert x (Word x 1 (e (nn before' pb)) (e (nn after' pa)) rel pp) dict'
  if isJust w then
    return $ Map.insert word' (Word word' c (nb before' pb) (na after' pa)
                               (wordGetRelated $ fromJust w)
                               (wordGetPos $ fromJust w)) dict'
         else if elem word' allow' then
           return $ i word'
           else if pp /= UnknownEPos || Aspell.check aspell'
                   (ByteString.pack word') then
                  return $ i word'
                else
                  return dict'
  where
    e [] = Map.empty
    e x = Map.singleton x 1
    c = incCount' (fromJust w) 1
    na x y = if elem x allow' then incAfter' (fromJust w) x 1
                else if y /= UnknownEPos || Aspell.check aspell'
                        (ByteString.pack x) then incAfter' (fromJust w) x 1
                     else wordGetAfter (fromJust w)
    nb x y = if elem x allow' then incBefore' (fromJust w) x 1
                else if y /= UnknownEPos || Aspell.check aspell'
                        (ByteString.pack x) then incBefore' (fromJust w) x 1
                     else wordGetBefore (fromJust w)

insertName :: Fugly -> String -> String -> String -> IO (Map.Map String Word)
insertName f@(Fugly {dict=d}) w b a = insertName' f (Map.lookup w d) w b a

insertName' :: Fugly -> Maybe Word -> String -> String
              -> String -> IO (Map.Map String Word)
insertName' (Fugly dict' _ _ _ _ _ _) _ [] _ _ = return dict'
insertName' (Fugly dict' _ wne' aspell' allow' _ _) w name' before' after' = do
  pa <- wnPartPOS wne' after'
  pb <- wnPartPOS wne' before'
  rel <- wnRelated' wne' name' "Hypernym" (POS Noun)
  if isJust w then
    return $ Map.insert name' (Name name' c (nb before' pb) (na after' pa)
                               (wordGetRelated (fromJust w))) dict'
    else
    return $ Map.insert name' (Name name' 1 (e (nn before' pb))
                               (e (nn after' pa)) rel) dict'
  where
    e [] = Map.empty
    e x = Map.singleton x 1
    c = incCount' (fromJust w) 1
    na x y = if elem x allow' then incAfter' (fromJust w) x 1
                else if y /= UnknownEPos || Aspell.check aspell' (ByteString.pack x) then incAfter' (fromJust w) x 1
                     else wordGetAfter (fromJust w)
    nb x y = if elem x allow' then incBefore' (fromJust w) x 1
                else if y /= UnknownEPos || Aspell.check aspell' (ByteString.pack x) then incBefore' (fromJust w) x 1
                     else wordGetBefore (fromJust w)
    nn x y  = if elem x allow' then x
                else if y == UnknownEPos && Aspell.check aspell' (ByteString.pack x) == False then [] else x

dropWord :: Map.Map String Word -> String -> Map.Map String Word
dropWord m word' = Map.map del' (Map.delete word' m)
    where
      del' (Word w c b a r p) = (Word w c (Map.delete word' b) (Map.delete word' a) r p)
      del' (Name w c b a r) = (Name w c (Map.delete word' b) (Map.delete word' a) r)

ageWord :: Map.Map String Word -> String -> Int -> Map.Map String Word
ageWord m word' num = age m word' num 0
  where
    age m' w n i
      | i >= n    = m'
      | otherwise = age (ageWord' m' w) w n (i + 1)

ageWord' :: Map.Map String Word -> String -> Map.Map String Word
ageWord' m word' = Map.map age m
    where
      age ww@(Word w c _ _ r p) = (Word w (if w == word' then if c - 1 < 1 then 1 else c - 1 else c)
                                   (incBefore' ww word' (-1)) (incAfter' ww word' (-1)) r p)
      age ww@(Name w c _ _ r)   = (Name w (if w == word' then if c - 1 < 1 then 1 else c - 1 else c)
                                   (incBefore' ww word' (-1)) (incAfter' ww word' (-1)) r)

ageWords :: Map.Map String Word -> Int -> Map.Map String Word
ageWords m num = Map.filter (\x -> wordGetCount x > 0) $ f m (listWords m) num
    where
      f m' []     _ = m'
      f m' (x:xs) n = f (ageWord m' x n) xs n

cleanWords :: Map.Map String Word -> Map.Map String Word
cleanWords m = Map.filter (\x -> wordGetCount x > 0) $ f m (listWords m)
    where
      f m' []     = m'
      f m' (x:xs) = if null $ cleanString x then f (dropWord m' x) xs
                    else f m' xs

fixWords :: Aspell.SpellChecker -> Map.Map String Word -> IO (Map.Map String Word)
fixWords aspell' m = do
    x <- mapM f $ Map.toList $ Map.filter (\x -> wordGetCount x > 0) $ cleanWords m
    return $ Map.fromList x
    where
      f (s, (Word w c b a r p)) = do
        n <- asIsName aspell' w
        cna <- cn a
        cnb <- cn b
        if n then return (toUpperWord $ cleanString s, (Name (toUpperWord $ cleanString w)
                                                        c (Map.fromList cnb) (Map.fromList cna) r))
          else
            return (map toLower $ cleanString s, (Word (map toLower $ cleanString w)
                                                  c (Map.fromList cnb) (Map.fromList cna) r p))
      f (s, (Name w c b a r)) = do
        cna <- cn a
        cnb <- cn b
        return (toUpperWord $ cleanString s, (Name (toUpperWord $ cleanString w)
                                              c (Map.fromList cnb) (Map.fromList cna) r))
      cn m' = mapM cm $ Map.toList $ Map.filter (\x -> x > 0) m'
      cm (w, c) = do
        n <- asIsName aspell' w
        let cw = cleanString w
        if n then return ((toUpperWord cw), c)
          else return ((map toLower cw), c)

incCount' :: Word -> Int -> Int
incCount' (Word _ c _ _ _ _) n = if c + n < 1 then 1 else c + n
incCount' (Name _ c _ _ _)   n = if c + n < 1 then 1 else c + n

incBefore' :: Word -> String -> Int -> Map.Map String Int
incBefore' (Word _ _ b _ _ _) []      _ = b
incBefore' (Word _ _ b _ _ _) before' n =
  if isJust w then
    if (fromJust w) + n < 1 then b
    else Map.insert before' ((fromJust w) + n) b
  else if n < 0 then b
       else Map.insert before' n b
  where
    w = Map.lookup before' b
incBefore' (Name _ _ b _ _)   []    _ = b
incBefore' (Name w c b a r) before' n = incBefore' (Word w c b a r (POS Noun)) before' n

incAfter' :: Word -> String -> Int -> Map.Map String Int
incAfter' (Word _ _ _ a _ _) []     _ = a
incAfter' (Word _ _ _ a _ _) after' n =
  if isJust w then
    if (fromJust w) + n < 1 then a
    else Map.insert after' ((fromJust w) + n) a
  else if n < 0 then a
       else Map.insert after' n a
  where
    w = Map.lookup after' a
incAfter' (Name _ _ _ a _)   []   _ = a
incAfter' (Name w c b a r) after' n = incAfter' (Word w c b a r (POS Noun)) after' n

listNeigh :: Map.Map String Int -> [String]
listNeigh m = [w | (w, _) <- Map.toList m]

listNeigh2 :: Map.Map String Int -> [String]
listNeigh2 m = concat [[w, show c] | (w, c) <- Map.toList m]

listNeighMax :: Map.Map String Int -> [String]
listNeighMax m = [w | (w, c) <- Map.toList m, c == maximum [c' | (_, c') <- Map.toList m]]

listNeighLeast :: Map.Map String Int -> [String]
listNeighLeast m = [w | (w, c) <- Map.toList m, c == minimum [c' | (_, c') <- Map.toList m]]

listWords :: Map.Map String Word -> [String]
listWords m = map wordGetWord $ Map.elems m

listWordsCountSort :: Map.Map String Word -> [String]
listWordsCountSort m = reverse $ map snd (sort $ map wordGetwc $ Map.elems m)

listWordsCountSort2 :: Map.Map String Word -> Int -> [String]
listWordsCountSort2 m num = concat [[w, show c, ";"] | (c, w) <- take num $ reverse $
                            sort $ map wordGetwc $ Map.elems m]

listNamesCountSort2 :: Map.Map String Word -> Int -> [String]
listNamesCountSort2 m num = concat [[w, show c, ";"] | (c, w) <- take num $ reverse $
                            sort $ map wordGetwc $ filter (\x -> wordIs x == "name") $
                            Map.elems m]

listWordFull :: Map.Map String Word -> String -> String
listWordFull m word' =
  if isJust ww then
    unwords $ f (fromJust ww)
  else
    "Nothing!"
  where
    ww = Map.lookup word' m
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

cleanString :: String -> String
cleanString [] = []
cleanString "i" = "I"
cleanString x
  | length x > 1 = filter (\y -> isAlpha y || y == '\'' || y == '-' || y == ' ') x
  | x == "I" || map toLower x == "a" = x
  | otherwise = []

dePlenk :: String -> String
dePlenk []  = []
dePlenk s   = dePlenk' s []
  where
    dePlenk' [] l  = l
    dePlenk' [x] l = l ++ x:[]
    dePlenk' a@(x:xs) l
      | length a == 1                             = l ++ x:[]
      | x == ' ' && (h == ' ' || isPunctuation h) = dePlenk' (fTail [] xs) (l ++ h:[])
      | otherwise                                 = dePlenk' xs (l ++ x:[])
      where
        h = fHead '!' xs

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
joinWords a s = joinWords' a $ filter (not . null) s
  where
    joinWords' _ [] = []
    joinWords' a' (x:xs)
      | (fHead ' ' x) == a' = unwords (x : (take num xs)) : joinWords a' (drop num xs)
      | otherwise                       = x : joinWords a' xs
      where
        num = (fromMaybe 0 (elemIndex a' $ map (\y -> fLast '!' y) xs)) + 1

fixUnderscore :: String -> String
fixUnderscore = strip '"' . replace ' ' '_'

toUpperWord :: String -> String
toUpperWord [] = []
toUpperWord w = (toUpper $ head w) : tail w

toUpperSentence :: [String] -> [String]
toUpperSentence []     = []
toUpperSentence [x]    = [toUpperWord x]
toUpperSentence (x:xs) = toUpperWord x : xs

endSentence :: [String] -> [String]
endSentence []  = []
endSentence msg = (init msg) ++ ((fLast [] msg) ++ if elem (head msg) qWords then "?" else ".") : []

fReadInt :: String -> Int -> String -> Int
fReadInt a b c = unsafePerformIO (do catch (evaluate (read c :: Int))
                                       (\e -> do hPutStrLn stderr ("fRead: " ++ show
                                                                   (e :: SomeException)
                                                                   ++ " in " ++ a) ; return b))

{--
fHead :: String -> a -> [a] -> a
fHead a b [] = unsafePerformIO (do hPutStrLn stderr ("fHead: error in " ++ a) ; return b)
fHead _ _ c  = head c

fLast :: String -> a -> [a] -> a
fLast a b [] = unsafePerformIO (do hPutStrLn stderr ("fLast: error in " ++ a) ; return b)
fLast _ _ c  = last c

fTail :: String -> [a] -> [a] -> [a]
fTail a b [] = unsafePerformIO (do hPutStrLn stderr ("fTail: error in " ++ a) ; return b)
fTail _ _ c  = tail c
--}

fHead :: a -> [a] -> a
fHead b [] = b
fHead _ c  = head c

fLast :: a -> [a] -> a
fLast b [] = b
fLast _ c  = last c

fTail :: [a] -> [a] -> [a]
fTail b [] = b
fTail _ c  = tail c

wnPartString :: WordNetEnv -> String -> IO String
wnPartString _ [] = return "Unknown"
wnPartString w a  = do
    ind1 <- catch (indexLookup w a Noun) (\e -> do let err = show (e :: SomeException)
                                                   hPutStrLn stderr ("WordNet: indexLookup Noun: " ++ err)
                                                   return Nothing)
    ind2 <- catch (indexLookup w a Verb) (\e -> do let err = show (e :: SomeException)
                                                   hPutStrLn stderr ("WordNet: indexLookup Verb: " ++ err)
                                                   return Nothing)
    ind3 <- catch (indexLookup w a Adj)  (\e -> do let err = show (e :: SomeException)
                                                   hPutStrLn stderr ("WordNet: indexLookup Adj: " ++ err)
                                                   return Nothing)
    ind4 <- catch (indexLookup w a Adv)  (\e -> do let err = show (e :: SomeException)
                                                   hPutStrLn stderr ("WordNet: indexLookup Adv: " ++ err)
                                                   return Nothing)
    return (type' ((count' ind1) : (count' ind2) : (count' ind3) : (count' ind4) : []))
  where
    count' a' = if isJust a' then senseCount (fromJust a') else 0
    type' [] = "Other"
    type' a'
      | a' == [0, 0, 0, 0]                              = "Unknown"
      | fromMaybe (-1) (elemIndex (maximum a') a') == 0 = "Noun"
      | fromMaybe (-1) (elemIndex (maximum a') a') == 1 = "Verb"
      | fromMaybe (-1) (elemIndex (maximum a') a') == 2 = "Adj"
      | fromMaybe (-1) (elemIndex (maximum a') a') == 3 = "Adv"
      | otherwise                                       = "Unknown"

wnPartPOS :: WordNetEnv -> String -> IO EPOS
wnPartPOS _ [] = return UnknownEPos
wnPartPOS w a  = do
    ind1 <- catch (indexLookup w a Noun) (\e -> do let err = show (e :: SomeException)
                                                   hPutStrLn stderr ("WordNet: indexLookup Noun: " ++ err)
                                                   return Nothing)
    ind2 <- catch (indexLookup w a Verb) (\e -> do let err = show (e :: SomeException)
                                                   hPutStrLn stderr ("WordNet: indexLookup Verb: " ++ err)
                                                   return Nothing)
    ind3 <- catch (indexLookup w a Adj)  (\e -> do let err = show (e :: SomeException)
                                                   hPutStrLn stderr ("WordNet: indexLookup Adj: " ++ err)
                                                   return Nothing)
    ind4 <- catch (indexLookup w a Adv)  (\e -> do let err = show (e :: SomeException)
                                                   hPutStrLn stderr ("WordNet: indexLookup Adv: " ++ err)
                                                   return Nothing)
    return (type' ((count' ind1) : (count' ind2) : (count' ind3) : (count' ind4) : []))
  where
    count' a' = if isJust a' then senseCount (fromJust a') else 0
    type' [] = UnknownEPos
    type' a'
      | a' == [0, 0, 0, 0]                              = UnknownEPos
      | fromMaybe (-1) (elemIndex (maximum a') a') == 0 = POS Noun
      | fromMaybe (-1) (elemIndex (maximum a') a') == 1 = POS Verb
      | fromMaybe (-1) (elemIndex (maximum a') a') == 2 = POS Adj
      | fromMaybe (-1) (elemIndex (maximum a') a') == 3 = POS Adv
      | otherwise                                       = UnknownEPos

wnGloss :: WordNetEnv -> String -> String -> IO String
wnGloss _    []     _ = return "Nothing!  Error!  Abort!"
wnGloss wne' word' [] = do
    wnPos <- wnPartString wne' (fixUnderscore word')
    wnGloss wne' word' wnPos
wnGloss wne' word' pos' = do
    let wnPos = fromEPOS $ readEPOS pos'
    s <- runs wne' $ search (fixUnderscore word') wnPos AllSenses
    let result = map (getGloss . getSynset) (runs wne' s)
    if (null result) then return "Nothing!" else
      return $ unwords result

wnRelated :: WordNetEnv -> String -> String -> String -> IO String
wnRelated wne' word' []   _    = wnRelated wne' word' "Hypernym" []
wnRelated wne' word' form []   = do
    wnPos <- wnPartString wne' (fixUnderscore word')
    wnRelated wne' word' form wnPos
wnRelated wne' word' form pos' = do
    x <- wnRelated' wne' word' form $ readEPOS pos'
    f (filter (not . null) x) []
  where
    f []     a = return a
    f (x:xs) a = f xs (x ++ " " ++ a)

wnRelated' :: WordNetEnv -> String -> String -> EPOS -> IO [String]
wnRelated' _ [] _ _             = return [[]] :: IO [String]
wnRelated' wne' word' form pos' = catch (do
    let wnForm = readForm form
    s <- runs wne' $ search (fixUnderscore word') (fromEPOS pos') AllSenses
    r <- runs wne' $ relatedByList wnForm s
    ra <- runs wne' $ relatedByListAllForms s
    let result = if (map toLower form) == "all" then concat $ map (fromMaybe [[]])
                    (runs wne' ra)
                 else fromMaybe [[]] (runs wne' r)
    if (null result) || (null $ concat result) then return [] else
      return $ map (\x -> replace '_' ' ' $ unwords $ map (++ "\"") $
                    map ('"' :) $ concat $ map (getWords . getSynset) x) result)
                            (\e -> do let err = show (e :: SomeException)
                                      hPutStrLn stderr ("WordNet: relatedBy: " ++ err)
                                      return [])

wnClosure :: WordNetEnv -> String -> String -> String -> IO String
wnClosure _ [] _ _           = return []
wnClosure wne' word' []   _  = wnClosure wne' word' "Hypernym" []
wnClosure wne' word' form [] = do
    wnPos <- wnPartString wne' (fixUnderscore word')
    wnClosure wne' word' form wnPos
wnClosure wne' word' form pos' = do
    let wnForm = readForm form
    let wnPos = fromEPOS $ readEPOS pos'
    s <- runs wne' $ search (fixUnderscore word') wnPos AllSenses
    result <- runs wne' $ closureOnList wnForm s
    if null result then return [] else
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
    s1 <- runs w $ search (fixUnderscore c) wnPos 1
    s2 <- runs w $ search (fixUnderscore d) wnPos 1
    let r1 = runs w s1
    let r2 = runs w s2
    m <- runs w $ meet emptyQueue (head $ r1) (head $ r2)
    if not (null r1) && not (null r2) then do
        let result = m
        if isNothing result then return [] else
            return $ replace '_' ' ' $ unwords $ map (++ "\"") $ map ('"' :) $
                    getWords $ getSynset $ fromJust result
        else return []

asIsName :: Aspell.SpellChecker -> String -> IO Bool
asIsName _       []    = return False
asIsName aspell' word' = do
    let l = map toLower word'
    let w = toUpperWord l
    n1 <- asSuggest aspell' l
    n2 <- asSuggest aspell' w
    let nn1 = if null n1 then False else elem l $ words n1
    let nn2 = if null n2 then False else (head $ words n2) == w
    return (if nn1 == False && nn2 == True then True else False)

asSuggest :: Aspell.SpellChecker -> String -> IO String
asSuggest _       []    = return []
asSuggest aspell' word' = do w <- Aspell.suggest aspell' (ByteString.pack word')
                             return $ unwords $ map ByteString.unpack w

gfParseBool :: PGF -> Int -> String -> Bool
gfParseBool _ _ [] = False
gfParseBool pgf' len msg
  | elem lw badEndWords = False
  | length w > len = (gfParseBoolA pgf' $ take len w) &&
                     (gfParseBool pgf' len (unwords $ drop len w))
  | otherwise      = gfParseBoolA pgf' w
    where
      w = words msg
      lw = strip '?' $ strip '.' $ last w

gfParseBoolA :: PGF -> [String] -> Bool
gfParseBoolA pgf' msg
  | null msg                                 = False
  | null $ parse pgf' lang (startCat pgf') m = False
  | otherwise                                = True
  where
    m = unwords msg
    lang = head $ languages pgf'

{--
gfParseBool2 :: PGF -> String -> Bool
gfParseBool2 pgf msg = lin pgf lang (parse_ pgf lang (startCat pgf) Nothing msg)
  where
    lin pgf lang (ParseOk tl, _)      = True
    lin pgf lang _                    = False
    lang = head $ languages pgf
--}

gfParseC :: PGF -> String -> [String]
gfParseC pgf' msg = lin pgf' lang (parse_ pgf' lang (startCat pgf') Nothing msg)
  where
    lin p l (ParseOk tl, _)      = map (lin' p l) tl
    lin _ _ (ParseFailed a, b)   = ["parse failed at " ++ show a ++
                                    " tokens: " ++ showBracketedString b]
    lin _ _ (ParseIncomplete, b) = ["parse incomplete: " ++ showBracketedString b]
    lin _ _ _                    = ["No parse!"]
    lin' p l t = "parse: " ++ showBracketedString (head $ bracketedLinearize p l t)
    lang = head $ languages pgf'

gfCategories :: PGF -> [String]
gfCategories pgf' = map showCId (categories pgf')

sentence :: Fugly -> Int -> Int -> Int -> Int -> [String] -> [IO String]
sentence _ _ _ _ _ [] = [return []] :: [IO String]
sentence fugly@(Fugly {pgf=pgf', ban=ban'}) randoms stries slen plen msg = do
  let s1f x = if null x then return []
              else if gfParseBool pgf' plen x && length (words x) > 2 then return x else return []
  let s1a x = do
      z <- findNextWord fugly 0 randoms True x
      let zz = if null z then [] else head z
      y <- findNextWord fugly 1 randoms True zz
      let yy = if null y then [] else head y
      let c = if null zz && null yy then 2 else if null zz || null yy then 3 else 4
      w <- s1b fugly slen c $ findNextWord fugly 1 randoms False x
      ww <- wnReplaceWords fugly randoms w
      return $ filter (not . null) ([yy] ++ [zz] ++ [x] ++ (filter (\a -> length a > 0 && not (elem a ban')) ww))
  let s1d x = do
      w <- x
      if null w then return []
        else return ((init w) ++ ((fLast [] w) ++
                                  if elem (map toLower $ head w) qWords then "?" else ".") : [])
  let s1e x = do
      w <- x
      if null w then return []
        else return ((s1c w : [] ) ++ fTail [] w)
  let s1g = if slen == 0 then [return []] else
              take stries $ map (\x -> do y <- x ; return $ dePlenk $ unwords y) (map (s1e . s1d . s1a) (cycle msg))
  map (\x -> do y <- x ; s1f y) s1g
  where
    s1b :: Fugly -> Int -> Int -> IO [String] -> IO [String]
    s1b f n i msg' = do
      ww <- msg'
      if null ww then return []
        else if i >= n then return $ nub ww else do
               www <- findNextWord f i randoms False (fLast [] ww)
               s1b f n (i + 1) (return $ ww ++ www)
    s1c :: [String] -> String
    s1c [] = []
    s1c w = ((toUpper $ head $ head w) : []) ++ (fTail [] $ head w)

chooseWord :: [String] -> IO [String]
chooseWord [] = return []
chooseWord msg = do
  cc <- c1 msg
  c2 cc []
  where
    c1 m = do
      r <- Random.getStdRandom (Random.randomR (0, 1)) :: IO Int
      if r == 0 then return m
        else return $ reverse m
    c2 [] m  = return m
    c2 [x] m = return (m ++ [x])
    c2 (x:xs) m = do
      r <- Random.getStdRandom (Random.randomR (0, 1)) :: IO Int
      if r == 0 then c2 xs (m ++ [x])
        else c2 ([x] ++ tail xs) (m ++ [head xs])

wnReplaceWords :: Fugly -> Int -> [String] -> IO [String]
wnReplaceWords _ _ [] = return []
wnReplaceWords fugly@(Fugly {wne=wne'}) randoms msg = do
  cw <- chooseWord msg
  cr <- Random.getStdRandom (Random.randomR (0, (length cw) - 1))
  rr <- Random.getStdRandom (Random.randomR (0, 99))
  w <- if not $ null cw then findRelated wne' (cw!!cr) else return []
  let out = map cleanString $ filter (not . null) ((takeWhile (/= (cw!!cr)) msg) ++ [w] ++ (tail $ dropWhile (/= (cw!!cr)) msg))
  if rr + randoms < 90 then
    return out
    else if randoms < 90 then
      wnReplaceWords fugly randoms out
      else mapM (\x -> findRelated wne' x) msg

asReplaceWords :: Fugly -> [String] -> IO [String]
asReplaceWords _ [] = return [[]]
asReplaceWords fugly msg = do
  mapM (\x -> asReplace fugly x) msg

asReplace :: Fugly -> String -> IO String
asReplace _ [] = return []
asReplace (Fugly dict' _ wne' aspell' _ _ _) word' =
  if (elem ' ' word') || (elem '\'' word') || (head word' == (toUpper $ head word')) then return word'
    else do
    a  <- asSuggest aspell' word'
    p <- wnPartPOS wne' word'
    let w = Map.lookup word' dict'
    let rw = words a
    rr <- Random.getStdRandom (Random.randomR (0, (length rw) - 1))
    if null rw || p /= UnknownEPos || isJust w then return word' else
      if head rw == word' then return word' else return (rw!!rr)

findNextWord :: Fugly -> Int -> Int -> Bool -> String -> IO [String]
findNextWord _ _ _ _ [] = return []
findNextWord (Fugly dict' _ _ _ _ _ _) i randoms prev word' = do
  let ln = if isJust w then length neigh else 0
  let lm = if isJust w then length neighmax else 0
  let ll = if isJust w then length neighleast else 0
  nr <- Random.getStdRandom (Random.randomR (0, ln - 1))
  mr <- Random.getStdRandom (Random.randomR (0, lm - 1))
  lr <- Random.getStdRandom (Random.randomR (0, ll - 1))
  rr <- Random.getStdRandom (Random.randomR (0, 99))
  let f1 = if isJust w && length neigh > 0 then neighleast!!lr else []
  let f2 = if isJust w && length neigh > 0 then case mod i 3 of
        0 -> neigh!!nr
        1 -> neighleast!!lr
        2 -> neighleast!!lr
        _ -> []
           else []
  let f3 = if isJust w && length neigh > 0 then case mod i 5 of
        0 -> neighleast!!lr
        1 -> neigh!!nr
        2 -> neighmax!!mr
        3 -> neigh!!nr
        4 -> neigh!!nr
        _ -> []
           else []
  let f4 = if isJust w && length neigh > 0 then case mod i 3 of
        0 -> neighmax!!mr
        1 -> neigh!!nr
        2 -> neighmax!!mr
        _ -> []
           else []
  let f5 = if isJust w && length neigh > 0 then neighmax!!mr else []
  if randoms > 89 then return $ replace "i" "I" $ words f1 else
    if rr < randoms - 25 then return $ replace "i" "I" $ words f2 else
      if rr < randoms + 35 then return $ replace "i" "I" $ words f3 else
        if rr < randoms + 65 then return $ replace "i" "I" $ words f4 else
          return $ replace "i" "I" $ words f5
    where
      w          = Map.lookup word' dict'
      wordGet'   = if prev then wordGetBefore else wordGetAfter
      neigh      = listNeigh $ wordGet' (fromJust w)
      neighmax   = listNeighMax $ wordGet' (fromJust w)
      neighleast = listNeighLeast $ wordGet' (fromJust w)

findRelated :: WordNetEnv -> String -> IO String
findRelated wne' word' = do
  pp <- wnPartPOS wne' word'
  if pp /= UnknownEPos then do
    hyper <- wnRelated' wne' word' "Hypernym" pp
    hypo  <- wnRelated' wne' word' "Hyponym" pp
    anto  <- wnRelated' wne' word' "Antonym" pp
    let hyper' = filter (\x -> not $ elem ' ' x && length x > 2) $ map (strip '"') hyper
    let hypo'  = filter (\x -> not $ elem ' ' x && length x > 2) $ map (strip '"') hypo
    let anto'  = filter (\x -> not $ elem ' ' x && length x > 2) $ map (strip '"') anto
    if null anto' then
      if null hypo' then
        if null hyper' then
          return word'
          else do
            r <- Random.getStdRandom (Random.randomR (0, (length hyper') - 1))
            return (hyper'!!r)
        else do
          r <- Random.getStdRandom (Random.randomR (0, (length hypo') - 1))
          return (hypo'!!r)
      else do
        r <- Random.getStdRandom (Random.randomR (0, (length anto') - 1))
        return (anto'!!r)
    else return word'

dictLookup :: Fugly -> String -> String -> IO String
dictLookup (Fugly _ _ wne' aspell' _ _ _) word' pos' = do
    gloss <- wnGloss wne' word' pos'
    if gloss == "Nothing!" then
       do a <- asSuggest aspell' word'
          return (gloss ++ " Perhaps you meant: " ++
                  (unwords (filter (\x -> x /= word') (words a))))
      else return gloss
