module FuglyLib
       (
         initFugly,
         stopFugly,
         loadDict,
         saveDict,
         dictLookup,
         insertWords,
         insertWord,
         insertWordRaw,
         insertNameRaw,
         insertAcroRaw,
         wordGetBanAfter,
         addBanAfter,
         deleteBanAfter,
         dropWord,
         dropAfter,
         dropAllAfter,
         dropBefore,
         ageWord,
         ageWords,
         numWords,
         listWords,
         listWordFull,
         listWordsCountSort,
         wordIs,
         cleanStringWhite,
         cleanStringBlack,
         cleanString,
         wnRelated,
         wnClosure,
         wnMeet,
         wnReplaceWords,
         asReplace,
         asReplaceWords,
         asIsName,
         asIsAcronym,
         gfLin,
         gfShowExpr,
         gfParseBool,
         gfParseC,
         gfCategories,
         gfRandom,
         gfRandom2,
         gfAll,
         sentence,
         insertCommas,
         chooseWord,
         findRelated,
         joinWords,
         toUpperSentence,
         endSentence,
         dePlenk,
         fHead,
         fHeadUnsafe,
         fLast,
         fLastUnsafe,
         fTail,
         fTailUnsafe,
         Word (..),
         Fugly (..)
       )
       where

import Control.Concurrent (MVar, putMVar, takeMVar)
import Control.Exception
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (StateT, evalStateT, get)
import qualified Data.ByteString.Char8 as ByteString
import Data.Char
import Data.Either
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Tree (flatten)
import qualified System.Random as Random
import System.IO
import System.IO.Unsafe {-- For easy debugging. --}

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
              ban     :: [String],
              match   :: [String]
              }

data Word = Word {
              word     :: String,
              count    :: Int,
              before   :: Map.Map String Int,
              after    :: Map.Map String Int,
              banafter :: [String],
              related  :: [String],
              pos      :: EPOS
              } |
            Name {
              name     :: String,
              count    :: Int,
              before   :: Map.Map String Int,
              after    :: Map.Map String Int,
              banafter :: [String],
              related  :: [String]
              } |
            Acronym {
              acronym    :: String,
              count      :: Int,
              before     :: Map.Map String Int,
              after      :: Map.Map String Int,
              banafter   :: [String],
              related    :: [String],
              definition :: String
              }

class Word_ a where
  wordIs          :: a -> String
  wordGetWord     :: a -> String
  wordGetCount    :: a -> Int
  wordGetAfter    :: a -> Map.Map String Int
  wordGetBanAfter :: a -> [String]
  wordGetBefore   :: a -> Map.Map String Int
  wordGetRelated  :: a -> [String]
  wordGetPos      :: a -> EPOS
  wordGetwc       :: a -> (Int, String)

instance Word_ Word where
  wordIs Word{}                       = "word"
  wordIs Name{}                       = "name"
  wordIs Acronym{}                    = "acronym"
  wordGetWord Word{word=w}            = w
  wordGetWord Name{name=n}            = n
  wordGetWord Acronym{acronym=a}      = a
  wordGetCount Word{count=c}          = c
  wordGetCount Name{count=c}          = c
  wordGetCount Acronym{count=c}       = c
  wordGetCount _                      = 0
  wordGetAfter Word{after=a}          = a
  wordGetAfter Name{after=a}          = a
  wordGetAfter Acronym{after=a}       = a
  wordGetAfter _                      = Map.empty
  wordGetBanAfter Word{banafter=a}    = a
  wordGetBanAfter Name{banafter=a}    = a
  wordGetBanAfter Acronym{banafter=a} = a
  wordGetBanAfter _                   = []
  wordGetBefore Word{before=b}        = b
  wordGetBefore Name{before=b}        = b
  wordGetBefore Acronym{before=b}     = b
  wordGetBefore _                     = Map.empty
  wordGetRelated Word{related=r}      = r
  wordGetRelated _                    = []
  wordGetPos Word{FuglyLib.pos=p}     = p
  wordGetPos _                        = UnknownEPos
  wordGetwc (Word w c _ _ _ _ _)      = (c, w)
  wordGetwc (Name n c _ _ _ _)        = (c, n)
  wordGetwc (Acronym a c _ _ _ _ _)   = (c, a)

initFugly :: FilePath -> FilePath -> FilePath -> String -> IO (Fugly, [String])
initFugly fuglydir wndir gfdir topic = do
    (dict', ban', match', params') <- catch (loadDict fuglydir topic)
                                     (\e -> do let err = show (e :: SomeException)
                                               hPutStrLn stderr ("Exception in initFugly: " ++ err)
                                               return (Map.empty, [], [], []))
    pgf' <- readPGF (gfdir ++ "/ParseEng.pgf")
    wne' <- NLP.WordNet.initializeWordNetWithOptions
            (return wndir :: Maybe FilePath)
            (Just (\e f -> hPutStrLn stderr (e ++ show (f :: SomeException))))
    a <- Aspell.spellCheckerWithOptions [Aspell.Options.Lang (ByteString.pack "en_US"),
                                         Aspell.Options.IgnoreCase False, Aspell.Options.Size Aspell.Options.Large,
                                         Aspell.Options.SuggestMode Aspell.Options.Normal, Aspell.Options.Ignore 2]
    let aspell' = head $ rights [a]
    return ((Fugly dict' pgf' wne' aspell' ban' match'), params')

stopFugly :: (MVar ()) -> FilePath -> Fugly -> String -> [String] -> IO ()
stopFugly st fuglydir fugly@(Fugly {wne=wne'}) topic params = do
    catch (saveDict st fugly fuglydir topic params)
      (\e -> do let err = show (e :: SomeException)
                evalStateT (hPutStrLnLock stderr ("Exception in stopFugly: " ++ err)) st
                return ())
    closeWordNet wne'

saveDict :: (MVar ()) -> Fugly -> FilePath -> String -> [String] -> IO ()
saveDict st (Fugly dict' _ _ _ ban' match') fuglydir topic params = do
    let d = Map.toList dict'
    if null d then evalStateT (hPutStrLnLock stderr "> Empty dict!") st
      else do
        h <- openFile (fuglydir ++ "/" ++ topic ++ "-dict.txt") WriteMode
        hSetBuffering h LineBuffering
        evalStateT (hPutStrLnLock stdout "Saving dict file...") st
        saveDict' h d
        evalStateT (hPutStrLnLock h ">END<") st
        evalStateT (hPutStrLnLock h $ unwords $ sort ban') st
        evalStateT (hPutStrLnLock h $ unwords $ sort match') st
        evalStateT (hPutStrLnLock h $ unwords params) st
        hClose h
  where
    saveDict' :: Handle -> [(String, Word)] -> IO ()
    saveDict' _ [] = return ()
    saveDict' h (x:xs) = do
      let l = format' $ snd x
      if null l then return () else evalStateT (hPutStrLock h l) st
      saveDict' h xs
    format' (Word w c b a ba r p)
      | null w    = []
      | otherwise = unwords [("word: " ++ w ++ "\n"),
                             ("count: " ++ (show c) ++ "\n"),
                             ("before: " ++ (unwords $ listNeigh2 b) ++ "\n"),
                             ("after: " ++ (unwords $ listNeigh2 a) ++ "\n"),
                             ("ban-after: " ++ (unwords ba) ++ "\n"),
                             ("related: " ++ (unwords r) ++ "\n"),
                             ("pos: " ++ (show p) ++ "\n"),
                             ("end: \n")]
    format' (Name w c b a ba r)
      | null w    = []
      | otherwise = unwords [("name: " ++ w ++ "\n"),
                             ("count: " ++ (show c) ++ "\n"),
                             ("before: " ++ (unwords $ listNeigh2 b) ++ "\n"),
                             ("after: " ++ (unwords $ listNeigh2 a) ++ "\n"),
                             ("ban-after: " ++ (unwords ba) ++ "\n"),
                             ("related: " ++ (unwords r) ++ "\n"),
                             ("end: \n")]
    format' (Acronym w c b a ba r d)
      | null w    = []
      | otherwise = unwords [("acronym: " ++ w ++ "\n"),
                             ("count: " ++ (show c) ++ "\n"),
                             ("before: " ++ (unwords $ listNeigh2 b) ++ "\n"),
                             ("after: " ++ (unwords $ listNeigh2 a) ++ "\n"),
                             ("ban-after: " ++ (unwords ba) ++ "\n"),
                             ("related: " ++ (unwords r) ++ "\n"),
                             ("definition: " ++ d ++ "\n"),
                             ("end: \n")]

loadDict :: FilePath -> String -> IO (Map.Map String Word, [String], [String], [String])
loadDict fuglydir topic = do
    let w = (Word [] 0 Map.empty Map.empty [] [] UnknownEPos)
    h <- openFile (fuglydir ++ "/" ++ topic ++ "-dict.txt") ReadMode
    eof <- hIsEOF h
    if eof then
      return (Map.empty, [], [], [])
      else do
      hSetBuffering h LineBuffering
      hPutStrLn stdout "Loading dict file..."
      dict'   <- ff h w [([], w)]
      ban'    <- hGetLine h
      match'  <- hGetLine h
      params' <- hGetLine h
      let out = (Map.fromList $ tail dict', words ban', words match', words params')
      hClose h
      return out
  where
    getNeigh :: [String] -> Map.Map String Int
    getNeigh a = Map.fromList $ getNeigh' a []
    getNeigh' :: [String] -> [(String, Int)] -> [(String, Int)]
    getNeigh'        [] l = l
    getNeigh' (x:y:xs) [] = getNeigh' xs [(x, read y :: Int)]
    getNeigh' (x:y:xs)  l = getNeigh' xs (l ++ (x, read y :: Int) : [])
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
                           "word:"       -> Word    (unwords ll) 0 Map.empty Map.empty [] [] UnknownEPos
                           "name:"       -> Name    (unwords ll) 0 Map.empty Map.empty [] []
                           "acronym:"    -> Acronym (unwords ll) 0 Map.empty Map.empty [] [] []
                           "count:"      -> word'{count=read (unwords $ ll) :: Int}
                           "before:"     -> word'{before=getNeigh ll}
                           "after:"      -> word'{after=getNeigh ll}
                           "ban-after:"  -> word'{banafter=ll}
                           "related:"    -> word'{related=joinWords '"' ll}
                           "pos:"        -> word'{FuglyLib.pos=readEPOS $ unwords ll}
                           "definition:" -> word'{definition=unwords ll}
                           "end:"        -> word'
                           _             -> word'
               else word'
      if l4 == False then do hPutStrLn stderr ("Oops: " ++ l) >> return nm
        else if (head wl) == "end:" then
               ff h ww (nm ++ ((wordGetWord ww), ww) : [])
             else if (head wl) == ">END<" then
                    return nm
                  else
                    ff h ww nm

qWords :: [String]
qWords = ["am", "are", "can", "did", "do", "does", "if", "is", "want", "what", "when", "where", "who", "why", "will"]

badEndWords :: [String]
badEndWords = ["a", "am", "an", "and", "are", "as", "at", "but", "by", "do", "for", "from", "go", "had", "has", "he", "he's", "i", "i'd", "if", "i'll", "i'm", "in", "into", "is", "it", "its", "it's", "i've", "just", "make", "makes", "mr", "mrs", "my", "of", "oh", "on", "or", "our", "person's", "she", "she's", "so", "than", "that", "that's", "the", "their", "there's", "they", "they're", "to", "us", "was", "we", "what", "when", "which", "with", "who", "whose", "you", "your", "you're", "you've"]

sWords :: [String]
sWords = ["a", "am", "an", "as", "at", "by", "do", "go", "he", "i", "if", "in", "is", "it", "me", "my", "no", "of", "oh", "on", "or", "so", "to", "us", "we"]

insertWords :: (MVar ()) -> Fugly -> Bool -> [String] -> IO (Map.Map String Word)
insertWords _ (Fugly{dict=d}) _ [] = return d
insertWords st fugly autoname [x]  = insertWord st fugly autoname x [] [] []
insertWords st fugly@(Fugly{dict=dict', ban=ban'}) autoname msg =
  case (len) of
    0 -> return dict'
    2 -> do ff <- insertWord st fugly autoname mx [] my []
            insertWord st fugly{dict=ff} autoname my mx [] []
    _ -> insertWords' st fugly autoname 0 len mmsg
  where
    mmsg@(mx:my:_) = (\\) msg ban'
    len = length mmsg
    insertWords' _ (Fugly{dict=d}) _ _ _ [] = return d
    insertWords' st' f@(Fugly{dict=d}) a i l m
      | i == 0     = do ff <- insertWord st' f a (m!!i) [] (m!!(i+1)) []
                        insertWords' st' f{dict=ff} a (i+1) l m
      | i > l - 1  = return d
      | i == l - 1 = do ff <- insertWord st' f a (m!!i) (m!!(i-1)) [] []
                        insertWords' st' f{dict=ff} a (i+1) l m
      | otherwise  = do ff <- insertWord st' f a (m!!i) (m!!(i-1)) (m!!(i+1)) []
                        insertWords' st' f{dict=ff} a (i+1) l m

insertWord :: (MVar ()) -> Fugly -> Bool -> String -> String -> String -> String -> IO (Map.Map String Word)
insertWord _ (Fugly{dict=d}) _ [] _ _ _ = return d
insertWord st fugly@(Fugly{dict=dict', aspell=aspell', ban=ban'}) autoname word' before' after' pos' = do
    n   <- asIsName st aspell' word'
    nb  <- asIsName st aspell' before'
    na  <- asIsName st aspell' after'
    ac  <- asIsAcronym st aspell' word'
    acb <- asIsAcronym st aspell' before'
    aca <- asIsAcronym st aspell' after'
    if (length word' == 1 || length word' == 2) && (not $ elem (map toLower word') sWords) then return dict'
      else if isJust w then {--do evalStateT (hPutStrLnLock stderr ("> debug: insertWord: " ++ word')) st >>--} (f st nb na acb aca $ fromJust w)
        else if ac && autoname then
                if elem (acroword word') ban' then return dict'
                else insertAcroRaw' st fugly wa (acroword word') (bi nb acb) (ai na aca) []
          else if n && autoname then
                  if elem (upperword word') ban' then return dict'
                  else insertNameRaw' st fugly wn (upperword word') (bi nb acb) (ai na aca) False
            else if isJust ww then insertWordRaw' st fugly ww (lowerword word') (bi nb acb) (ai na aca) pos'
              else insertWordRaw' st fugly w (lowerword word') (bi nb acb) (ai na aca) pos'
  where
    lowerword = map toLower . cleanString
    upperword = toUpperWord . cleanString
    acroword  = map toUpper . cleanString
    w  = Map.lookup word'     dict'
    wa = Map.lookup (acroword  word') dict'
    wn = Map.lookup (upperword word') dict'
    ww = Map.lookup (lowerword word') dict'
    a  = Map.lookup after'    dict'
    b  = Map.lookup before'   dict'
    ai an aa = if isJust a then after'
               else if (length after' == 1 || length after' == 2) && (not $ elem (map toLower after') sWords) then []
                    else if aa && autoname then
                           if elem (acroword after') ban' then []
                           else if isJust wa then if elem (acroword after') (wordGetBanAfter $ fromJust wa) then []
                                                  else acroword after'
                                else acroword after'
                         else if an && autoname then
                                if elem (upperword after') ban' then []
                                else upperword after'
                              else lowerword after'
    bi bn ba = if isJust b then before'
               else if (length before' == 1 || length before' == 2) && (not $ elem (map toLower before') sWords) then []
                    else if ba && autoname then
                           if elem (acroword before') ban' then []
                           else acroword before'
                         else if bn && autoname then
                                if elem (upperword before') ban' then []
                                else upperword before'
                              else lowerword before'
    f st' bn an ba aa (Word{})    = insertWordRaw' st' fugly w  word'             (bi bn ba) (ai an aa) pos'
    f st' bn an ba aa (Name{})    = insertNameRaw' st' fugly wn (upperword word') (bi bn ba) (ai an aa) False
    f st' bn an ba aa (Acronym{}) = insertAcroRaw' st' fugly wa (acroword word')  (bi bn ba) (ai an aa) []

insertWordRaw :: (MVar ()) -> Fugly -> String -> String -> String -> String -> IO (Map.Map String Word)
insertWordRaw st f@(Fugly{dict=d}) w b a p = insertWordRaw' st f (Map.lookup w d) w b a p

insertWordRaw' :: (MVar ()) -> Fugly -> Maybe Word -> String -> String
                 -> String -> String -> IO (Map.Map String Word)
insertWordRaw' _ (Fugly{dict=d}) _ [] _ _ _ = return d
insertWordRaw' st (Fugly dict' _ wne' aspell' _ _) w word' before' after' pos' = do
  pp <- (if null pos' then wnPartPOS wne' word' else return $ readEPOS pos')
  pa <- wnPartPOS wne' after'
  pb <- wnPartPOS wne' before'
  rel <- wnRelated' wne' word' "Hypernym" pp
  as <- evalStateT (asSuggest aspell' word') st
  let asw = words as
  let nn x y  = if isJust $ Map.lookup x dict' then x
                else if y == UnknownEPos && Aspell.check aspell'
                        (ByteString.pack x) == False then [] else x
  let insert' x = Map.insert x (Word x 1 (e (nn before' pb)) (e (nn after' pa)) [] rel pp) dict'
  let msg w' = evalStateT (hPutStrLnLock stdout ("> inserted new word: " ++ w')) st
  if isJust w then let w' = fromJust w in return $ Map.insert word' w'{count=c, before=nb before' pb, after=na after' pa} dict'
    else if pp /= UnknownEPos || Aspell.check aspell' (ByteString.pack word') then msg word' >> return (insert' word')
      else if (length asw) > 0 then let hasw = head asw in
        if (length hasw < 3 && (not $ elem (map toLower hasw) sWords)) || (isJust $ Map.lookup hasw dict')
        then return dict' else msg hasw >> return (insert' hasw)
          else return dict'
  where
    e [] = Map.empty
    e x = Map.singleton x 1
    c = incCount' (fromJust w) 1
    na x y = if isJust $ Map.lookup x dict' then incAfter' (fromJust w) x 1
                else if y /= UnknownEPos || Aspell.check aspell'
                        (ByteString.pack x) then incAfter' (fromJust w) x 1
                     else wordGetAfter (fromJust w)
    nb x y = if isJust $ Map.lookup x dict' then incBefore' (fromJust w) x 1
                else if y /= UnknownEPos || Aspell.check aspell'
                        (ByteString.pack x) then incBefore' (fromJust w) x 1
                     else wordGetBefore (fromJust w)

insertNameRaw :: (MVar ()) -> Fugly -> String -> String -> String -> Bool -> IO (Map.Map String Word)
insertNameRaw st f@(Fugly{dict=d}) w b a s = insertNameRaw' st f (Map.lookup w d) w b a s

insertNameRaw' :: (MVar ()) -> Fugly -> Maybe Word -> String -> String
                  -> String -> Bool -> IO (Map.Map String Word)
insertNameRaw' _  (Fugly{dict=d}) _ [] _ _ _ = return d
insertNameRaw' st (Fugly dict' _ wne' aspell' _ _) w name' before' after' s = do
  pa <- wnPartPOS wne' after'
  pb <- wnPartPOS wne' before'
  rel <- wnRelated' wne' name' "Hypernym" (POS Noun)
  let n = if s then name' else toUpperWord $ map toLower name'
  let msg w' = evalStateT (hPutStrLnLock stdout ("> inserted new name: " ++ w')) st
  if isJust w then let w' = fromJust w in return $ Map.insert name' w'{count=c, before=nb before' pb, after=na after' pa} dict'
    else msg n >> return (Map.insert n (Name n 1 (e (nn before' pb)) (e (nn after' pa)) [] rel) dict')
  where
    e [] = Map.empty
    e x = Map.singleton x 1
    c = incCount' (fromJust w) 1
    na x y = if isJust $ Map.lookup x dict' then incAfter' (fromJust w) x 1
                else if y /= UnknownEPos || Aspell.check aspell' (ByteString.pack x) then incAfter' (fromJust w) x 1
                     else wordGetAfter (fromJust w)
    nb x y = if isJust $ Map.lookup x dict' then incBefore' (fromJust w) x 1
                else if y /= UnknownEPos || Aspell.check aspell' (ByteString.pack x) then incBefore' (fromJust w) x 1
                     else wordGetBefore (fromJust w)
    nn x y  = if isJust $ Map.lookup x dict' then x
                else if y == UnknownEPos && Aspell.check aspell' (ByteString.pack x) == False then [] else x

insertAcroRaw :: (MVar ()) -> Fugly -> String -> String -> String -> String -> IO (Map.Map String Word)
insertAcroRaw st f@(Fugly{dict=d}) w b a def = insertAcroRaw' st f (Map.lookup w d) w b a def

insertAcroRaw' :: (MVar ()) -> Fugly -> Maybe Word -> String -> String
                  -> String -> String -> IO (Map.Map String Word)
insertAcroRaw' _  (Fugly{dict=d}) _ [] _ _ _ = return d
insertAcroRaw' st (Fugly dict' _ wne' aspell' _ _) w acro' before' after' def = do
  pa <- wnPartPOS wne' after'
  pb <- wnPartPOS wne' before'
  rel <- wnRelated' wne' acro' "Hypernym" (POS Noun)
  let msg w' = evalStateT (hPutStrLnLock stdout ("> inserted new acronym: " ++ w')) st
  if isJust w then let w' = fromJust w in return $ Map.insert acro' w'{count=c, before=nb before' pb, after=na after' pa} dict'
    else msg acro' >> return (Map.insert acro' (Acronym acro' 1 (e (nn before' pb)) (e (nn after' pa)) [] rel def) dict')
  where
    e [] = Map.empty
    e x = Map.singleton x 1
    c = incCount' (fromJust w) 1
    na x y = if isJust $ Map.lookup x dict' then incAfter' (fromJust w) x 1
                else if y /= UnknownEPos || Aspell.check aspell' (ByteString.pack x) then incAfter' (fromJust w) x 1
                     else wordGetAfter (fromJust w)
    nb x y = if isJust $ Map.lookup x dict' then incBefore' (fromJust w) x 1
                else if y /= UnknownEPos || Aspell.check aspell' (ByteString.pack x) then incBefore' (fromJust w) x 1
                     else wordGetBefore (fromJust w)
    nn x y  = if isJust $ Map.lookup x dict' then x
                else if y == UnknownEPos && Aspell.check aspell' (ByteString.pack x) == False then [] else x

addBanAfter :: Word -> String -> Word
addBanAfter w ban' = let ba = wordGetBanAfter w in w{banafter=sort $ nub $ ban' : ba}

deleteBanAfter :: Word -> String -> Word
deleteBanAfter w ban' = let ba = wordGetBanAfter w in w{banafter=sort $ nub $ delete ban' ba}

dropWord :: Map.Map String Word -> String -> Map.Map String Word
dropWord m word' = Map.map del' $ Map.delete word' m
    where
      del' w = w{before=Map.delete word' (wordGetBefore w), after=Map.delete word' (wordGetAfter w)}

dropAfter :: Map.Map String Word -> String -> String -> Map.Map String Word
dropAfter m word' after' = Map.adjust del' word' m
    where
      del' w = w{after=Map.delete after' (wordGetAfter w)}

dropAllAfter :: Map.Map String Word -> String -> Map.Map String Word
dropAllAfter m word' = Map.adjust del' word' m
    where
      del' w = w{after=Map.empty}

dropBefore :: Map.Map String Word -> String -> String -> Map.Map String Word
dropBefore m word' before' = Map.adjust del' word' m
    where
      del' w = w{after=Map.delete before' (wordGetBefore w)}

ageWord :: Map.Map String Word -> String -> Int -> Map.Map String Word
ageWord m word' num = age m word' num 0
  where
    age m' w n i
      | i >= n    = m'
      | otherwise = age (ageWord' m' w) w n (i + 1)

ageWord' :: Map.Map String Word -> String -> Map.Map String Word
ageWord' m word' = Map.map age m
    where
      w = wordGetWord  $ fromJust $ Map.lookup word' m
      c = wordGetCount $ fromJust $ Map.lookup word' m
      age w' = w'{count=if w == word' then if c - 1 < 1 then 1 else c - 1 else c,
                  before=incBefore' w' word' (-1), after=incAfter' w' word' (-1)}

ageWords :: Map.Map String Word -> Int -> Map.Map String Word
ageWords m num = Map.filter (\x -> wordGetCount x > 0) $ f m (listWords m) num
    where
      f m' []     _ = m'
      f m' (x:xs) n = f (ageWord m' x n) xs n

incCount' :: Word -> Int -> Int
incCount' w n = let c = wordGetCount w in if c + n < 1 then 1 else c + n

incBefore' :: Word -> String -> Int -> Map.Map String Int
incBefore' w []      _ = wordGetBefore w
incBefore' w before' n =
  if isJust w' then
    if (fromJust w') + n < 1 then b
    else Map.insert before' ((fromJust w') + n) b
  else if n < 0 then b
       else Map.insert before' n b
  where
    b  = wordGetBefore w
    w' = Map.lookup before' b

incAfter' :: Word -> String -> Int -> Map.Map String Int
incAfter' w []     _ = wordGetAfter w
incAfter' w after' n =
  if isJust w' then
    if (fromJust w') + n < 1 then a
    else Map.insert after' ((fromJust w') + n) a
  else if n < 0 then a
       else Map.insert after' n a
  where
    a  = wordGetAfter w
    w' = Map.lookup after' a

numWords :: Word_ a => Map.Map k a -> String -> Int
numWords m t = length $ filter (\x -> wordIs x == t) $ Map.elems m

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

listWordsCountSort :: Map.Map String Word -> Int -> String -> [String]
listWordsCountSort m num t = concat [[w, show c, ";"] | (c, w) <- take num $ reverse $
                             sort $ map wordGetwc $ filter (\x -> wordIs x == t) $
                             Map.elems m]

listWordFull :: Map.Map String Word -> String -> String
listWordFull m word' =
  if isJust ww then
    unwords $ f (fromJust ww)
  else
    "Nothing!"
  where
    ww = Map.lookup word' m
    f (Word w c b a ba r p) = ["word:", w, "count:", show c, " before:",
                 unwords $ listNeigh2 b, " after:", unwords $ listNeigh2 a,
                 " ban after:", unwords ba, " pos:", (show p), " related:", unwords r]
    f (Name w c b a ba r) = ["name:", w, "count:", show c, " before:",
                 unwords $ listNeigh2 b, " after:", unwords $ listNeigh2 a,
                 " ban after:", unwords ba, " related:", unwords r]
    f (Acronym w c b a ba r d) = ["acronym:", w, "count:", show c, " before:",
                 unwords $ listNeigh2 b, " after:", unwords $ listNeigh2 a,
                 " ban after:", unwords ba, " definition:", d, " related:", unwords r]

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
toUpperWord w = (toUpper $ fHead ' '  w) : tail w

toUpperLast :: String -> String
toUpperLast [] = []
toUpperLast w = init w ++ [toUpper $ last w]

toUpperSentence :: [String] -> [String]
toUpperSentence []     = []
toUpperSentence [x]    = [toUpperWord x]
toUpperSentence (x:xs) = toUpperWord x : xs

endSentence :: [String] -> [String]
endSentence []  = []
endSentence msg = (init msg) ++ ((fLast [] msg) ++ if elem (fHead [] msg) qWords then "?" else ".") : []

fHead :: a -> [a] -> a
fHead b [] = b
fHead _ c  = head c

fLast :: a -> [a] -> a
fLast b [] = b
fLast _ c  = last c

fTail :: [a] -> [a] -> [a]
fTail b [] = b
fTail _ c  = tail c

fHeadUnsafe :: String -> a -> [a] -> a
fHeadUnsafe a b [] = unsafePerformIO (do hPutStrLn stderr ("fHead: error in " ++ a) ; return b)
fHeadUnsafe _ _ c  = head c

fLastUnsafe :: String -> a -> [a] -> a
fLastUnsafe a b [] = unsafePerformIO (do hPutStrLn stderr ("fLast: error in " ++ a) ; return b)
fLastUnsafe _ _ c  = last c

fTailUnsafe :: String -> [a] -> [a] -> [a]
fTailUnsafe a b [] = unsafePerformIO (do hPutStrLn stderr ("fTail: error in " ++ a) ; return b)
fTailUnsafe _ _ c  = tail c

wnPartString :: WordNetEnv -> String -> IO String
wnPartString _ [] = return "Unknown"
wnPartString w a  = do
    ind1 <- catch (indexLookup w a Noun) (\e -> return (e :: SomeException) >> return Nothing)
    ind2 <- catch (indexLookup w a Verb) (\e -> return (e :: SomeException) >> return Nothing)
    ind3 <- catch (indexLookup w a Adj)  (\e -> return (e :: SomeException) >> return Nothing)
    ind4 <- catch (indexLookup w a Adv)  (\e -> return (e :: SomeException) >> return Nothing)
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
    ind1 <- catch (indexLookup w a Noun) (\e -> return (e :: SomeException) >> return Nothing)
    ind2 <- catch (indexLookup w a Verb) (\e -> return (e :: SomeException) >> return Nothing)
    ind3 <- catch (indexLookup w a Adj)  (\e -> return (e :: SomeException) >> return Nothing)
    ind4 <- catch (indexLookup w a Adv)  (\e -> return (e :: SomeException) >> return Nothing)
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
wnRelated _    []    _    _    = return []
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
wnRelated' _    []    _    _    = return [[]] :: IO [String]
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
                            (\e -> return (e :: SomeException) >> return [])

wnClosure :: WordNetEnv -> String -> String -> String -> IO String
wnClosure _    []    _    _  = return []
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

asIsName :: (MVar ()) -> Aspell.SpellChecker -> String -> IO Bool
asIsName _ _ []    = return False
asIsName _ _ "i"   = return True
asIsName _ _ "I"   = return True
asIsName st aspell' word' = do
    let l = map toLower word'
    let u = toUpperWord l
    let b = toUpperLast l
    nl <- evalStateT (asSuggest aspell' l) st
    nb <- evalStateT (asSuggest aspell' b) st
    return $ if length word' < 3 then False
             else if (length $ words nb) < 3 then False
                  else if word' == (words nb)!!1 then False
                       else if (word' == (words nb)!!0 || u == (words nb)!!0) && (not $ null nl) then True
                            else False

asIsAcronym :: (MVar ()) -> Aspell.SpellChecker -> String -> IO Bool
asIsAcronym _  _       []    = return False
asIsAcronym _  _      (_:[]) = return False
asIsAcronym st aspell' word' = do
    let n = ["who"]
    let a = [toLower $ head word'] ++ (tail $ map toUpper word')
    let u = map toUpper word'
    na <- evalStateT (asSuggest aspell' a) st
    return $ if elem (map toLower word') n && (not $ word' == u) then False
             else if u == word' && null na then True
                  else if (length $ words na) > 2 && word' == (words na)!!1 || elem word' sWords then False
                       else if (not $ null na) && u == (words na)!!0 then True
                            else False

dictLookup :: (MVar ()) -> Fugly -> String -> String -> IO String
dictLookup st (Fugly _ _ wne' aspell' _ _) word' pos' = do
    gloss <- wnGloss wne' word' pos'
    if gloss == "Nothing!" then
       do a <- evalStateT (asSuggest aspell' word') st
          return (gloss ++ " Perhaps you meant: " ++
                  (unwords (filter (\x -> x /= word') (words a))))
      else return gloss

asSuggest :: Aspell.SpellChecker -> String -> StateT (MVar ()) IO String
asSuggest _       []    = return []
asSuggest aspell' word' = do
  l <- get :: StateT (MVar ()) IO (MVar ())
  lock <- lift $ takeMVar l
  w <- lift $ Aspell.suggest aspell' (ByteString.pack word')
  let ww = map ByteString.unpack w
  lift $ if null ww then do putMVar l lock ; return []
           else if word' == head ww then do putMVar l lock ; return []
             else do putMVar l lock ; return $ unwords ww

gfLin :: PGF -> String -> String
gfLin _ [] = []
gfLin pgf' msg
  | isJust expr = linearize pgf' (head $ languages pgf') $ fromJust $ expr
  | otherwise   = []
    where
      expr = readExpr msg

gfParseBool :: PGF -> Int -> String -> Bool
gfParseBool _ _ [] = False
gfParseBool pgf' len msg
  | elem (map toLower lw) badEndWords = False
  | elem '\'' (map toLower lw)        = False
  | len == 0       = True
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

gfRandom :: PGF -> Int -> String
gfRandom pgf' num = dePlenk $ unwords $ toUpperSentence $ endSentence $ take 95 $
                    filter (not . null) $ map cleanString $ words $ gfRandom'
    where
      gfRandom' = linearize pgf' (head $ languages pgf') $ head $
                  generateRandomDepth (Random.mkStdGen num) pgf' (startCat pgf') (Just num)

gfRandom2 :: PGF -> IO String
gfRandom2 pgf' = do
  num <- Random.getStdRandom (Random.randomR (0, 9999))
  return $ dePlenk $ unwords $ toUpperSentence $ endSentence $
    filter (not . null) $ map cleanString $ take 12 $ words $ gfRandom' num
    where
      gfRandom' n = linearize pgf' (head $ languages pgf') $ head $
                    generateRandomDepth (Random.mkStdGen n) pgf' (startCat pgf') (Just n)

gfShowExpr :: PGF -> String -> Int -> String
gfShowExpr pgf' type' num = if isJust $ readType type' then
    let c = fromJust $ readType type'
    in
      head $ filter (not . null) $ map (\x -> fromMaybe [] (unStr x))
      (generateRandomDepth (Random.mkStdGen num) pgf' c (Just num))
                          else "Not a GF type."

gfAll :: PGF -> Int -> String
gfAll pgf' num = dePlenk $ unwords $ toUpperSentence $ endSentence $ take 15 $ words $
                 linearize pgf' (head $ languages pgf') ((generateAllDepth pgf' (startCat pgf') (Just 3))!!num)

sentence :: (MVar ()) -> Fugly -> Bool -> Int -> Int -> Int -> Int -> [String] -> [IO String]
sentence _ _ _ _ _ _ _ [] = [return []] :: [IO String]
sentence st fugly@(Fugly{dict=dict', pgf=pgf', wne=wne', aspell=aspell', ban=ban'}) rwords randoms stries slen plen msg = do
  let s1f x = if null x then return []
              else if gfParseBool pgf' plen x && length (words x) > 2 then return x
                   else {--evalStateT (hPutStrLnLock stderr ("> debug: sentence try: " ++ x)) st >>--} return []
  let s1h n a x = if a then map toUpper x else if n then x else map toLower x
  let s1i x = do
      a <- s1m x
      n <- s1n x
      return $ if a then map toUpper x else if n then x else map toLower x
  let s1a x = do
      a <- s1m x
      n <- s1n x
      z <- findNextWord fugly 0 randoms True x
      let zz = if null z then [] else head z
      y <- findNextWord fugly 1 randoms True zz
      let yy = if null y then [] else head y
      let c = if null zz && null yy then 2 else if null zz || null yy then 3 else 4
      w <- s1b fugly slen c $ findNextWord fugly 1 randoms False x
      ww <- s1b fugly slen 0 $ mapM s1i msg
      res <- preSentence fugly $ map (\m -> map toLower m) msg
      let d = if length msg < 4 then ww else (words res) ++ [yy] ++ [zz] ++ [s1h n a x] ++ w
      rep <- wnReplaceWords fugly rwords randoms $ filter (\a' -> length a' > 0 && not (elem a' ban'))
             $ filter (\b -> if length b < 3 && (not $ elem b sWords) then False else True) $ take stries d
      return $ filter (\p -> {-- gfParseBool pgf' plen p && --}(not $ null p)) rep
  let s1d x = do
      w <- x
      if null w then return []
        else return (init w ++ (last w ++ if elem (map toLower $ head w) qWords then "?" else ".") : [])
  let s1e x = do
      w <- x
      if null w then return []
        else return ([s1c w] ++ tail w)
  let s1g = map (\x -> do y <- insertCommas wne' 0 x ; return $ dePlenk $ unwords y) (map (s1e . s1d . s1a) (msg ++ sWords))
  -- let s1g = map (\x -> do y <- x ; z <- insertCommas wne' 0 $ return y ; evalStateT (hPutStrLnLock stderr ("> debug: pre-comma: " ++ unwords y ++ "\n> debug: post-comma: " ++ unwords z)) st ; return $ dePlenk $ unwords z) (map (s1e . s1d . s1a) (msg ++ sWords))
  map (\x -> do y <- x ; s1f y) s1g
  where
    s1b :: Fugly -> Int -> Int -> IO [String] -> IO [String]
    s1b f n i msg' = do
      ww <- msg'
      if null ww then return []
        else if i >= n then return $ nub ww else do
               www <- findNextWord f i randoms False $ fLast [] ww
               s1b f n (i + 1) (return $ ww ++ www)
    s1c :: [String] -> String
    s1c [] = []
    s1c w = [toUpper $ head $ head w] ++ (fTail [] $ head w)
    s1m :: String -> IO Bool
    s1m [] = return False
    s1m w = do
      a <- asIsAcronym st aspell' w
      let ww = Map.lookup w dict'
      return $ if isJust ww then wordIs (fromJust ww) == "acronym" else a
    s1n :: String -> IO Bool
    s1n [] = return False
    s1n w = do
      n <- asIsName st aspell' w
      let ww = Map.lookup w dict'
      return $ if isJust ww then wordIs (fromJust ww) == "name" else n

insertCommas :: WordNetEnv -> Int -> IO [String] -> IO [String]
insertCommas wne' i w = do
  w' <- w
  r <- Random.getStdRandom (Random.randomR (0, 5)) :: IO Int
  let x  = fHead [] w'
  let xs = fTail [] w'
  let y  = fHead [] xs
  let bad = ["a", "an", "and", "as", "but", "by", "for", "from", "had", "has", "I", "in", "is", "of", "on", "or", "that", "the", "this", "to", "very", "was", "with"]
  px <- wnPartPOS wne' x
  py <- wnPartPOS wne' y
  if length xs < 1 then w
    else if (elem x bad) || i < 3 then do
    xs' <- insertCommas wne' (i + 1) $ return xs
    return (x : xs')
         else if px == POS Noun && (py == POS Noun || py == POS Adj) && r < 4 then do
           xs' <- insertCommas wne' 0 $ return xs
           return ((x ++ if r < 2 then ", or" else ", and") : xs')
              else if (y == "a" || y == "the" || y == "then") && r < 3 then do
                xs' <- insertCommas wne' 0 $ return xs
                return ((x ++ ",") : xs')
                   else if px == POS Adj && py == POS Adj then do
                     xs' <- insertCommas wne' 0 $ return xs
                     return ((x ++ ",") : xs')
                        else do
                          xs' <- insertCommas wne' (i + 1) $ return xs
                          return (x : xs')

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

wnReplaceWords :: Fugly -> Bool -> Int -> [String] -> IO [String]
wnReplaceWords _ _     _ []  = return []
wnReplaceWords _ False _ msg = return msg
wnReplaceWords fugly@(Fugly{wne=wne', ban=ban'}) True randoms msg = do
  cw <- chooseWord msg
  cr <- Random.getStdRandom (Random.randomR (0, (length cw) - 1))
  rr <- Random.getStdRandom (Random.randomR (0, 99))
  w <- findRelated wne' (cw!!cr)
  let ww = if elem w ban' then cw!!cr else w
  if randoms == 0 then
    return msg
    else if randoms == 100 then
      mapM (\x -> findRelated wne' x) msg
      else if rr + 20 < randoms then
        wnReplaceWords fugly True randoms $ filter (not . null) ((takeWhile (/= (cw!!cr)) msg) ++
                                                                 [ww] ++ (tail $ dropWhile (/= (cw!!cr)) msg))
        else
          return msg

asReplaceWords :: (MVar ()) -> Fugly -> [String] -> IO [String]
asReplaceWords _ _ [] = return [[]]
asReplaceWords st fugly msg = do
  mapM (\x -> asReplace st fugly x) msg

asReplace :: (MVar ()) -> Fugly -> String -> IO String
asReplace _ _ [] = return []
asReplace st (Fugly dict' _ wne' aspell' _ _) word' = do
  n  <- asIsName st aspell' word'
  ac <- asIsAcronym st aspell' word'
  if (elem ' ' word') || (elem '\'' word') || word' == (toUpperWord word') || n || ac then return word'
    else do
    a <- evalStateT (asSuggest aspell' word') st
    p <- wnPartPOS wne' word'
    let w = Map.lookup word' dict'
    let rw = words a
    rr <- Random.getStdRandom (Random.randomR (0, (length rw) - 1))
    if null rw || p /= UnknownEPos || isJust w then return word' else
      if head rw == word' then return word' else return $ map toLower (rw!!rr)

findNextWord :: Fugly -> Int -> Int -> Bool -> String -> IO [String]
findNextWord _ _ _ _ [] = return []
findNextWord (Fugly {dict=dict'}) i randoms prev word' = do
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
  let out = if randoms > 89 then words f1 else
              if rr < randoms - 25 then words f2 else
                if rr < randoms + 35 then words f3 else
                  if rr < randoms + 65 then words f4 else
                    words f5
  return out
    where
      w          = Map.lookup word' dict'
      wordGet'   = if prev then wordGetBefore else wordGetAfter
      neigh      = listNeigh $ wordGet' (fromJust w)
      neighmax   = listNeighMax $ wordGet' (fromJust w)
      neighleast = listNeighLeast $ wordGet' (fromJust w)

findRelated :: WordNetEnv -> String -> IO String
findRelated wne' word' = do
  pp <- wnPartPOS wne' word'
  out <- do if pp /= UnknownEPos then do
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
  if null out then return word' else return out

preSentence :: Fugly -> [String] -> IO String
preSentence _ [] = return []
preSentence (Fugly {ban=ban', FuglyLib.match=match'}) msg@(x : _) = do
    r <- Random.getStdRandom (Random.randomR (0, 60)) :: IO Int
    if elem x qWords then
      return (case r of
        1  -> "yes, and "
        2  -> "no way "
        3  -> "no, the "
        4  -> "yes, but "
        5  -> "perhaps the "
        6  -> "it is possible, and "
        7  -> "why is "
        8  -> "maybe, though "
        9  -> "certainly, "
        10 -> "certainly but "
        11 -> "never, "
        12 -> "no, but "
        13 -> "sure, however "
        14 -> "perhaps you are right "
        15 -> "maybe this "
        16 -> "of course "
        17 -> "sometimes it "
        18 -> "only when the "
        19 -> "it is weird that "
        20 -> "exactly, "
        _  -> [])
      else if (msg \\ ban') /= msg then
          return (case r of
            1  -> "that is disgusting, "
            2  -> "you are disgusting and "
            3  -> "foul language and "
            4  -> "you are disturbing me, "
            5  -> "maybe you should not "
            6  -> "please do not "
            7  -> "that is "
            8  -> "bad thoughts and "
            9  -> "do not say such "
            10 -> "do not "
            11 -> "stop that, "
            12 -> "ban this "
            13 -> "stop swearing about "
            14 -> "oh really "
            15 -> "don't be rude, "
            16 -> "that's filthy and "
            17 -> "shameful behavior never "
            18 -> "it's disgraceful "
            19 -> "please be nice, "
            _  -> [])
      else if (msg \\ match') /= msg then
             return (case r of
               1  -> "great stuff, "
               2  -> "I like that, "
               3  -> "keep going, "
               4  -> "please continue, "
               5  -> "very enlightening, please "
               6  -> "fascinating, I "
               7  -> "this is intriguing, "
               8  -> "simply wonderful "
               9  -> "yes indeed, "
               10 -> "if you like "
               11 -> "yeah it's nice "
               12 -> "undoubtedly, "
               13 -> "it is wonderful and "
               14 -> "so you like some "
               15 -> "I also like "
               16 -> "everybody loves "
               17 -> "this is great news, "
               18 -> "it's rather special "
               19 -> "absolutely, "
               20 -> "hey fabulous "
               21 -> "somewhat yes, but"
               22 -> "people agree that "
               _  -> [])
           else return []

hPutStrLock :: Handle -> String -> StateT (MVar ()) IO ()
hPutStrLock s m = do
  l <- get :: StateT (MVar ()) IO (MVar ())
  lock <- lift $ takeMVar l
  lift (do hPutStr s m ; putMVar l lock)

hPutStrLnLock :: Handle -> String -> StateT (MVar ()) IO ()
hPutStrLnLock s m = do
  l <- get :: StateT (MVar ()) IO (MVar ())
  lock <- lift $ takeMVar l
  lift (do hPutStrLn s m ; putMVar l lock)
