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
         dropTopic,
         dropTopicWords,
         ageWord,
         numWords,
         listWords,
         listWordFull,
         listWordsCountSort,
         listTopics,
         wordIs,
         replace,
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
         isName,
         isAcronym,
         gfLin,
         gfShowExpr,
         gfParseBool,
         gfParseShow,
         gfCategories,
         gfRandom,
         nnInsert,
         nnReply,
         sentenceA,
         sentenceB,
         sentenceB',
         insertCommas,
         findRelated,
         joinWords,
         toUpperSentence,
         endSentence,
         dePlenk,
         fHead,
         fLast,
         fTail,
         Word (..),
         DType (..),
         Default,
         Fugly (..)
       )
       where

import           Control.Concurrent             (MVar, putMVar, takeMVar)
import           Control.Exception
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.State.Lazy (StateT, evalStateT, get)
import qualified Data.ByteString.Char8          as ByteString
import           Data.Char
import           Data.Either
import           Data.List
import qualified Data.Map.Strict                as Map
import           Data.Maybe
import           Data.Tree                      (flatten)
import           Data.Word                      (Word16)
import qualified System.Random                  as Random
import           System.IO
import qualified Text.Regex.Posix               as Regex

import qualified Language.Aspell                as Aspell
import qualified Language.Aspell.Options        as Aspell.Options

import           NLP.WordNet                    hiding (Word)
import           NLP.WordNet.Prims              (indexLookup, senseCount, getSynset, getWords, getGloss)
import           NLP.WordNet.PrimTypes

import           AI.NeuralNetworks.Simple
import           PGF
import           Text.EditDistance              as EditDistance

type Dict    = Map.Map String Word
type Default = (DType, String)
type NSet    = [([Double], [Double])]
type NMap    = Map.Map Int String

data Fugly = Fugly {
              dict    :: Dict,
              defs    :: [Default],
              pgf     :: Maybe PGF,
              wne     :: WordNetEnv,
              aspell  :: Aspell.SpellChecker,
              ban     :: [String],
              match   :: [String],
              nset    :: NSet,
              nmap    :: NMap
              }

data Word = Word {
              word     :: String,
              count    :: Int,
              before   :: Map.Map String Int,
              after    :: Map.Map String Int,
              banafter :: [String],
              topic    :: [String],
              related  :: [String],
              pos      :: EPOS
              } |
            Name {
              name     :: String,
              count    :: Int,
              before   :: Map.Map String Int,
              after    :: Map.Map String Int,
              banafter :: [String],
              topic    :: [String]
              } |
            Acronym {
              acronym    :: String,
              count      :: Int,
              before     :: Map.Map String Int,
              after      :: Map.Map String Int,
              banafter   :: [String],
              topic      :: [String],
              definition :: String
              }

class Word_ a where
  wordIs          :: a -> String
  wordGetWord     :: a -> String
  wordGetCount    :: a -> Int
  wordGetAfter    :: a -> Map.Map String Int
  wordGetBefore   :: a -> Map.Map String Int
  wordGetBanAfter :: a -> [String]
  wordGetTopic    :: a -> [String]
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
  wordGetAfter Word{after=a}          = a
  wordGetAfter Name{after=a}          = a
  wordGetAfter Acronym{after=a}       = a
  wordGetBefore Word{before=b}        = b
  wordGetBefore Name{before=b}        = b
  wordGetBefore Acronym{before=b}     = b
  wordGetBanAfter Word{banafter=a}    = a
  wordGetBanAfter Name{banafter=a}    = a
  wordGetBanAfter Acronym{banafter=a} = a
  wordGetTopic Word{topic=t}          = t
  wordGetTopic Name{topic=t}          = t
  wordGetTopic Acronym{topic=t}       = t
  wordGetRelated Word{related=r}      = r
  wordGetRelated _                    = []
  wordGetPos Word{FuglyLib.pos=p}     = p
  wordGetPos _                        = UnknownEPos
  wordGetwc (Word w c _ _ _ _ _ _)    = (c, w)
  wordGetwc (Name n c _ _ _ _)        = (c, n)
  wordGetwc (Acronym a c _ _ _ _ _)   = (c, a)

data DType = Normal | Action | GreetAction | Greeting | Enter deriving (Eq, Read, Show)

emptyWord :: Word
emptyWord = Word [] 0 Map.empty Map.empty [] [] [] UnknownEPos

initFugly :: FilePath -> FilePath -> FilePath -> String -> IO (Fugly, [String])
initFugly fuglydir wndir gfdir dfile = do
    (dict', defs', ban', match', params') <- catch (loadDict fuglydir dfile)
                                             (\e -> do let err = show (e :: SomeException)
                                                       hPutStrLn stderr ("Exception in initFugly: " ++ err)
                                                       return (Map.empty, [], [], [], []))
    pgf' <- if gfdir == "nogf" then return Nothing else do pg <- readPGF (gfdir ++ "/ParseEng.pgf")
                                                           return $ Just pg
    wne' <- NLP.WordNet.initializeWordNetWithOptions
            (return wndir :: Maybe FilePath)
            (Just (\e f -> hPutStrLn stderr (e ++ show (f :: SomeException))))
    a <- Aspell.spellCheckerWithOptions [Aspell.Options.Lang (ByteString.pack "en_US"),
                                         Aspell.Options.IgnoreCase False, Aspell.Options.Size Aspell.Options.Large,
                                         Aspell.Options.SuggestMode Aspell.Options.Normal, Aspell.Options.Ignore 2]
    let aspell' = head $ rights [a]
    let nset'   = []
    let nmap'   = Map.empty
    return ((Fugly dict' defs' pgf' wne' aspell' ban' match' nset' nmap'), params')

stopFugly :: (MVar ()) -> FilePath -> Fugly -> String -> [String] -> IO ()
stopFugly st fuglydir fugly@Fugly{wne=wne'} dfile params = do
    catch (saveDict st fugly fuglydir dfile params)
      (\e -> do let err = show (e :: SomeException)
                evalStateT (hPutStrLnLock stderr ("Exception in stopFugly: " ++ err)) st
                return ())
    closeWordNet wne'

saveDict :: (MVar ()) -> Fugly -> FilePath -> String -> [String] -> IO ()
saveDict st Fugly{dict=dict', defs=defs', ban=ban', match=match'} fuglydir dfile params = do
    let d = Map.toList dict'
    if null d then evalStateT (hPutStrLnLock stderr "> Empty dict!") st
      else do
        h <- openFile (fuglydir ++ "/" ++ dfile ++ "-dict.txt") WriteMode
        hSetBuffering h LineBuffering
        evalStateT (hPutStrLnLock stdout "Saving dict file...") st
        saveDict' h d
        evalStateT (hPutStrLnLock h ">END<") st
        _ <- evalStateT (mapM (\(t, m) -> hPutStrLnLock h (show t ++ " " ++ m)) defs') st
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
    format' (Word w c b a ba t r p)
      | null w    = []
      | otherwise = unwords [("word: " ++ w ++ "\n"),
                             ("count: " ++ (show c) ++ "\n"),
                             ("before: " ++ (unwords $ listNeighShow b) ++ "\n"),
                             ("after: " ++ (unwords $ listNeighShow a) ++ "\n"),
                             ("ban-after: " ++ (unwords ba) ++ "\n"),
                             ("topic: " ++ (unwords t) ++ "\n"),
                             ("related: " ++ (unwords r) ++ "\n"),
                             ("pos: " ++ (show p) ++ "\n"),
                             ("end: \n")]
    format' (Name w c b a ba t)
      | null w    = []
      | otherwise = unwords [("name: " ++ w ++ "\n"),
                             ("count: " ++ (show c) ++ "\n"),
                             ("before: " ++ (unwords $ listNeighShow b) ++ "\n"),
                             ("after: " ++ (unwords $ listNeighShow a) ++ "\n"),
                             ("ban-after: " ++ (unwords ba) ++ "\n"),
                             ("topic: " ++ (unwords t) ++ "\n"),
                             ("end: \n")]
    format' (Acronym w c b a ba t d)
      | null w    = []
      | otherwise = unwords [("acronym: " ++ w ++ "\n"),
                             ("count: " ++ (show c) ++ "\n"),
                             ("before: " ++ (unwords $ listNeighShow b) ++ "\n"),
                             ("after: " ++ (unwords $ listNeighShow a) ++ "\n"),
                             ("ban-after: " ++ (unwords ba) ++ "\n"),
                             ("topic: " ++ (unwords t) ++ "\n"),
                             ("definition: " ++ d ++ "\n"),
                             ("end: \n")]

loadDict :: FilePath -> String -> IO (Dict, [Default], [String], [String], [String])
loadDict fuglydir dfile = do
    h <- openFile (fuglydir ++ "/" ++ dfile ++ "-dict.txt") ReadMode
    eof <- hIsEOF h
    if eof then
      return (Map.empty, [], [], [], [])
      else do
      hSetBuffering h LineBuffering
      hPutStrLn stdout "Loading dict file..."
      dict'   <- ff h emptyWord [([], emptyWord)]
      defs'   <- fd [] h
      ban'    <- hGetLine h
      match'  <- hGetLine h
      params' <- hGetLine h
      let out = (Map.fromList $ tail dict', defs', words ban', words match', words params')
      hClose h
      return out
  where
    getNeigh :: [String] -> Map.Map String Int
    getNeigh a = Map.fromList $ getNeigh' a []
    getNeigh' :: [String] -> [(String, Int)] -> [(String, Int)]
    getNeigh' []        l = l
    getNeigh' (x:y:xs) [] = getNeigh' xs [(x, read y :: Int)]
    getNeigh' (x:y:xs)  l = getNeigh' xs (l ++ (x, read y :: Int) : [])
    getNeigh' _         l = l
    fd :: [Default] -> Handle -> IO [Default]
    fd a h = do
      l <- hGetLine h
      if l == ">END<" then
        return a
        else
        fd (a ++ [fd' $ words l]) h
      where
        fd' :: [String] -> Default
        fd' l = ((read $ head l) :: DType, unwords $ tail l)
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
                           "word:"       -> Word    (unwords ll) 0 Map.empty Map.empty [] [] [] UnknownEPos
                           "name:"       -> Name    (unwords ll) 0 Map.empty Map.empty [] []
                           "acronym:"    -> Acronym (unwords ll) 0 Map.empty Map.empty [] [] []
                           "count:"      -> word'{count=read (unwords $ ll) :: Int}
                           "before:"     -> word'{before=getNeigh ll}
                           "after:"      -> word'{after=getNeigh ll}
                           "ban-after:"  -> word'{banafter=ll}
                           "topic:"      -> word'{topic=ll}
                           "related:"    -> word'{related=joinWords '"' ll}
                           "pos:"        -> word'{FuglyLib.pos=readEPOS $ unwords ll}
                           "definition:" -> word'{definition=unwords ll}
                           "end:"        -> word'
                           _             -> word'
               else word'
      if l4 == False then do hPutStrLn stderr ("Oops: " ++ l) >> return nm
        else if head wl == "end:" then
               ff h ww (nm ++ ((wordGetWord ww), ww) : [])
             else if head wl == ">END<" then
                    return nm
                  else
                    ff h ww nm

qWords :: [String]
qWords = ["am", "are", "can", "could", "did", "do", "does", "have", "if", "is", "should", "want", "was", "were", "what", "when", "where", "who", "why", "will"]

badEndWords :: [String]
badEndWords = ["a", "about", "am", "an", "and", "are", "as", "at", "but", "by", "do", "every", "for", "from", "gave", "go", "got", "had", "has", "he", "her", "he's", "his", "i", "i'd", "if", "i'll", "i'm", "in", "into", "is", "it", "its", "it's", "i've", "just", "make", "makes", "mr", "mrs", "my", "no", "of", "oh", "on", "or", "our", "person's", "she", "she's", "so", "than", "that", "that's", "the", "their", "there's", "they", "they're", "to", "us", "very", "was", "we", "were", "what", "when", "where", "which", "why", "with", "who", "whose", "yes", "you", "your", "you're", "you've"]

sWords :: [String]
sWords = ["a", "am", "an", "as", "at", "by", "do", "go", "he", "i", "if", "in", "is", "it", "me", "my", "no", "of", "oh", "on", "or", "so", "to", "us", "we", "yo"]

insertWords :: (MVar ()) -> Fugly -> Bool -> String -> [String] -> IO Dict
insertWords _  Fugly{dict=d}  _ _    []   = return d
insertWords st fugly autoname topic' [x]  = insertWord st fugly autoname topic' x [] [] []
insertWords st fugly@Fugly{dict=dict'} autoname topic' msg =
  let mx = fHead [] dmsg
      my = if len > 1 then dmsg!!1 else [] in
  case (len) of
    0 -> return dict'
    1 -> return dict'
    2 -> do ff <- insertWord st fugly autoname topic' mx [] my []
            insertWord st fugly{dict=ff} autoname topic' my mx [] []
    _ -> insertWords' st fugly autoname topic' 0 len dmsg
  where
    dmsg = dedup msg
    len  = length dmsg
    insertWords' _ (Fugly{dict=d}) _ _ _ _ [] = return d
    insertWords' st' f@(Fugly{dict=d}) a t i l m
      | i == 0     = do ff <- insertWord st' f a t (m!!i) [] (m!!(i+1)) []
                        insertWords' st' f{dict=ff} a t (i+1) l m
      | i > l - 1  = return d
      | i == l - 1 = do ff <- insertWord st' f a t (m!!i) (m!!(i-1)) [] []
                        insertWords' st' f{dict=ff} a t (i+1) l m
      | otherwise  = do ff <- insertWord st' f a t (m!!i) (m!!(i-1)) (m!!(i+1)) []
                        insertWords' st' f{dict=ff} a t (i+1) l m

insertWord :: (MVar ()) -> Fugly -> Bool -> String -> String -> String -> String -> String -> IO Dict
insertWord _  Fugly{dict=d}                                     _        _      []    _       _      _    = return d
insertWord st fugly@Fugly{dict=dict', aspell=aspell', ban=ban'} autoname topic' word' before' after' pos' = do
    n   <- asIsName st aspell' word'
    nb  <- asIsName st aspell' before'
    na  <- asIsName st aspell' after'
    ac  <- asIsAcronym st aspell' word'
    acb <- asIsAcronym st aspell' before'
    aca <- asIsAcronym st aspell' after'
    if (length word' < 3) && (not $ elem (map toLower word') sWords) then return dict'
      else if isJust w then f st nb na acb aca $ fromJust w
        else if ac && autoname then
                if elem (acroword word') ban' then return dict'
                else insertAcroRaw' st fugly wa (acroword word') (bi nb acb) (ai na aca) topic' []
          else if n && autoname then
                  if elem (upperword word') ban' then return dict'
                  else insertNameRaw' st fugly False wn word' (bi nb acb) (ai na aca) topic'
            else if isJust ww then insertWordRaw' st fugly False ww (lowerword word') (bi nb acb) (ai na aca) topic' pos'
              else insertWordRaw' st fugly False w (lowerword word') (bi nb acb) (ai na aca) topic' pos'
  where
    lowerword = map toLower . cleanString
    upperword = toUpperWord . cleanString
    acroword  = map toUpper . cleanString
    w   = Map.lookup word'     dict'
    wa  = Map.lookup (acroword  word') dict'
    wn  = Map.lookup (upperword word') dict'
    ww  = Map.lookup (lowerword word') dict'
    b   = Map.lookup before'   dict'
    ba' = Map.lookup (acroword  before') dict'
    bn' = Map.lookup (upperword before') dict'
    bw' = Map.lookup (lowerword before') dict'
    ai an aa = if isJust w && elem after' (wordGetBanAfter $ fromJust w) then []
               else if (length after' < 3) && (not $ elem (map toLower after') sWords) then []
                    else if aa && autoname then
                           if elem (acroword after') ban' then []
                           else if isJust wa && elem (acroword after') (wordGetBanAfter $ fromJust wa) then []
                                else acroword after'
                         else if an && autoname then
                                if elem (upperword after') ban' then []
                                else if isJust wn && elem (upperword after') (wordGetBanAfter $ fromJust wn) then []
                                     else upperword after'
                              else if isJust ww then if elem (lowerword after') (wordGetBanAfter $ fromJust ww) then []
                                                     else after'
                                   else lowerword after'
    bi bn ba = if isJust b && elem word' (wordGetBanAfter $ fromJust b) then []
               else if (length before' < 3) && (not $ elem (map toLower before') sWords) then []
                    else if ba && autoname then
                           if elem (acroword before') ban' then []
                           else if isJust ba' && elem (acroword word') (wordGetBanAfter $ fromJust ba') then []
                                else acroword before'
                         else if bn && autoname then
                                if elem (upperword before') ban' then []
                                else if isJust bn' && elem (upperword word') (wordGetBanAfter $ fromJust bn') then []
                                     else upperword before'
                              else if isJust bw' then if elem (lowerword word') (wordGetBanAfter $ fromJust bw') then []
                                                      else before'
                                   else lowerword before'
    f st' bn an ba aa Word{}    = insertWordRaw' st' fugly False w  word'             (bi bn ba) (ai an aa) topic' pos'
    f st' bn an ba aa Name{}    = insertNameRaw' st' fugly True  w  word'             (bi bn ba) (ai an aa) topic'
    f st' bn an ba aa Acronym{} = insertAcroRaw' st' fugly       wa (acroword word')  (bi bn ba) (ai an aa) topic' []

insertWordRaw :: (MVar ()) -> Fugly -> Bool -> String -> String -> String -> String -> String -> IO Dict
insertWordRaw st f@Fugly{dict=d} s w b a t p = insertWordRaw' st f s (Map.lookup w d) w b a t p

insertWordRaw' :: (MVar ()) -> Fugly -> Bool -> Maybe Word -> String -> String
                 -> String -> String -> String -> IO Dict
insertWordRaw' _  Fugly{dict=d}                               _      _ []    _       _      _      _    = return d
insertWordRaw' st Fugly{dict=dict', wne=wne', aspell=aspell'} strict w word' before' after' topic' pos' = do
    pp <- (if null pos' then wnPartPOS wne' word' else return $ readEPOS pos')
    pa <- wnPartPOS wne' after'
    pb <- wnPartPOS wne' before'
    rel <- wnRelated' wne' word' "Hypernym" pp
    as <- evalStateT (asSuggest aspell' word') st
    let asw = words as
    let nn x y  = if isJust $ Map.lookup x dict' then x
                  else if y == UnknownEPos && Aspell.check aspell'
                          (ByteString.pack x) == False then [] else x
    let insert' x = Map.insert x (Word x 1 (e (nn before' pb)) (e (nn after' pa)) [] [topic'] rel pp) dict'
    let msg w' = evalStateT (hPutStrLnLock stdout ("> inserted new word: " ++ w')) st
    if isJust w then let w' = fromJust w in return $ Map.insert word' w'{count=c, before=nb before' pb,
                                              after=na after' pa, topic=sort $ nub (topic' : wordGetTopic w')} dict'
      else if strict then msg word' >> return (insert' word')
           else if pp /= UnknownEPos || Aspell.check aspell' (ByteString.pack word') then msg word' >> return (insert' word')
                else if (length asw) > 0 then let hasw = head asw in
                if (length hasw < 3 && (not $ elem (map toLower hasw) sWords)) || (isJust $ Map.lookup hasw dict')
                then return dict' else msg hasw >> return (insert' hasw)
                     else return dict'
    where
      e [] = Map.empty
      e x  = Map.singleton x 1
      c = incCount' (fromJust w) 1
      na x y = if isJust $ Map.lookup x dict' then incAfter' (fromJust w) x 1
               else if y /= UnknownEPos || Aspell.check aspell'
                       (ByteString.pack x) then incAfter' (fromJust w) x 1
                    else wordGetAfter (fromJust w)
      nb x y = if isJust $ Map.lookup x dict' then incBefore' (fromJust w) x 1
               else if y /= UnknownEPos || Aspell.check aspell'
                       (ByteString.pack x) then incBefore' (fromJust w) x 1
                    else wordGetBefore (fromJust w)

insertNameRaw :: (MVar ()) -> Fugly -> Bool -> String -> String -> String -> String -> IO Dict
insertNameRaw st f@Fugly{dict=d} s w b a t = insertNameRaw' st f s (Map.lookup w d) w b a t

insertNameRaw' :: (MVar ()) -> Fugly -> Bool -> Maybe Word -> String -> String
                  -> String -> String -> IO Dict
insertNameRaw' _  Fugly{dict=d}                               _      _ []    _       _      _      = return d
insertNameRaw' st Fugly{dict=dict', wne=wne', aspell=aspell'} strict w name' before' after' topic' = do
    pa <- wnPartPOS wne' after'
    pb <- wnPartPOS wne' before'
    let n = if strict then name' else toUpperWord $ map toLower name'
    let msg w' = evalStateT (hPutStrLnLock stdout ("> inserted new name: " ++ w')) st
    if isJust w then let w' = fromJust w in return $ Map.insert name' w'{count=c, before=nb before' pb,
                                              after=na after' pa, topic=sort $ nub (topic' : wordGetTopic w')} dict'
      else msg n >> return (Map.insert n (Name n 1 (e (nn before' pb)) (e (nn after' pa)) [] [topic']) dict')
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

insertAcroRaw :: (MVar ()) -> Fugly -> String -> String -> String -> String -> String -> IO Dict
insertAcroRaw st f@Fugly{dict=d} w b a t def = insertAcroRaw' st f (Map.lookup w d) w b a t def

insertAcroRaw' :: (MVar ()) -> Fugly -> Maybe Word -> String -> String
                  -> String -> String -> String -> IO Dict
insertAcroRaw' _  Fugly{dict=d}                               _ []    _       _      _      _   = return d
insertAcroRaw' st Fugly{dict=dict', wne=wne', aspell=aspell'} w acro' before' after' topic' def = do
    pa <- wnPartPOS wne' after'
    pb <- wnPartPOS wne' before'
    let msg w' = evalStateT (hPutStrLnLock stdout ("> inserted new acronym: " ++ w')) st
    if isJust w then let w' = fromJust w in return $ Map.insert acro' w'{count=c, before=nb before' pb,
                                              after=na after' pa, topic=sort $ nub (topic' : wordGetTopic w')} dict'
      else msg acro' >> return (Map.insert acro' (Acronym acro' 1 (e (nn before' pb)) (e (nn after' pa)) [] [topic'] def) dict')
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

dropWord :: Dict -> String -> Dict
dropWord m word' = Map.map del' $ Map.delete word' m
    where
      del' w = w{before=Map.delete word' (wordGetBefore w), after=Map.delete word' (wordGetAfter w)}

dropAfter :: Dict -> String -> String -> Dict
dropAfter m word' after' = Map.adjust del' word' m
    where
      del' w = w{after=Map.delete after' (wordGetAfter w)}

dropAllAfter :: Dict -> String -> Dict
dropAllAfter m word' = Map.adjust del' word' m
    where
      del' w = w{after=Map.empty}

dropBefore :: Dict -> String -> String -> Dict
dropBefore m word' before' = Map.adjust del' word' m
    where
      del' w = w{before=Map.delete before' (wordGetBefore w)}

dropTopic :: Dict -> String -> Dict
dropTopic m t = Map.map del' m
    where
      del' w = w{topic=sort $ nub ("default" : (delete t $ wordGetTopic w))}

dropTopicWords :: Dict -> String -> Dict
dropTopicWords m t = del' (filter (\w -> [t] == (delete "default" $ wordGetTopic w)) $ Map.elems m) m
    where
      del' []     m' = m'
      del' (x:xs) m' = del' xs (dropWord m' $ wordGetWord x)

ageWord :: Dict -> String -> Int -> Dict
ageWord m word' num = Map.map age m
    where
      age w = let
        w' = wordGetWord  w
        c  = wordGetCount w
        in if w' == word' then w{count=if c - num < 1 then 1 else c - num,
                                 before=incBefore' w word' (-num), after=incAfter' w word' (-num)}
           else w{before=incBefore' w word' (-num), after=incAfter' w word' (-num)}

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

numWords :: Word_ a => Map.Map k a -> String -> String -> Int
numWords m typ top = length $ filter (\x -> wordIs x == typ && (elem top (wordGetTopic x) || null top)) $ Map.elems m

listNeigh :: Map.Map String Int -> [String]
listNeigh m = [w | (w, _) <- Map.toList m]

listNeighMax :: Map.Map String Int -> [String]
listNeighMax m = [w | (w, c) <- Map.toList m, c == maximum [c' | (_, c') <- Map.toList m]]

listNeighLeast :: Map.Map String Int -> [String]
listNeighLeast m = [w | (w, c) <- Map.toList m, c == minimum [c' | (_, c') <- Map.toList m]]

listNeighTopic :: Dict -> String -> [String] -> [String]
listNeighTopic d t n = map wordGetWord $ filter (\x -> elem t $ wordGetTopic x) $ map (\w -> fromMaybe emptyWord $ Map.lookup w d) n

listNeighShow :: Map.Map String Int -> [String]
listNeighShow m = concat [[w, show c] | (w, c) <- Map.toList m]

listWords :: Dict -> [String]
listWords m = map wordGetWord $ Map.elems m

listWordsCountSort :: Dict -> Int -> String -> String -> [String]
listWordsCountSort m num typ top = concat [[w, show c, ";"] | (c, w) <- take num $ reverse $
                                   sort $ map wordGetwc $ filter (\x -> wordIs x == typ &&
                                   (elem top (wordGetTopic x) || null top)) $ Map.elems m]

listWordFull :: Dict -> String -> String
listWordFull m word' = if isJust ww then
                         unwords $ f (fromJust ww)
                       else
                         "Nothing!"
  where
    ww = Map.lookup word' m
    f (Word w c b a ba t r p) = ["word:", w, "count:", show c, " before:",
                 unwords $ listNeighShow b, " after:", unwords $ listNeighShow a,
                 " ban after:", unwords ba, " pos:", (show p), " topic:", unwords t, " related:", unwords r]
    f (Name w c b a ba t) = ["name:", w, "count:", show c, " before:",
                 unwords $ listNeighShow b, " after:", unwords $ listNeighShow a,
                 " ban after:", unwords ba, " topic:", unwords t]
    f (Acronym w c b a ba t d) = ["acronym:", w, "count:", show c, " before:",
                 unwords $ listNeighShow b, " after:", unwords $ listNeighShow a,
                 " ban after:", unwords ba, " topic:", unwords t, " definition:", d]

listTopics :: Dict -> [String]
listTopics m = sort $ nub $ concat $ map wordGetTopic $ Map.elems m

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
cleanString []  = []
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

dedup :: Eq a => [a] -> [a]
dedup []  = []
dedup [x] = [x]
dedup (x:xs)
    | x == head xs = x : dedup (drop 1 xs)
    | otherwise    = x : dedup xs

joinWords :: Char -> [String] -> [String]
joinWords _ [] = []
joinWords a s  = joinWords' a $ filter (not . null) s
  where
    joinWords' _ [] = []
    joinWords' a' (x:xs)
      | (fHead ' ' x) == a' = unwords (x : (take num xs)) : joinWords a' (drop num xs)
      | otherwise           = x : joinWords a' xs
      where
        num = (fromMaybe 0 (elemIndex a' $ map (\y -> fLast '!' y) xs)) + 1

fixUnderscore :: String -> String
fixUnderscore = strip '"' . replace ' ' '_'

toUpperWord :: String -> String
toUpperWord [] = []
toUpperWord w  = (toUpper $ head w) : tail w

toUpperLast :: String -> String
toUpperLast [] = []
toUpperLast w  = init w ++ [toUpper $ last w]

toUpperSentence :: [String] -> [String]
toUpperSentence []     = []
toUpperSentence [x]    = [toUpperWord x]
toUpperSentence (x:xs) = toUpperWord x : xs

endSentence :: [String] -> [String]
endSentence []  = []
endSentence msg
    | l == '?'  = msg
    | l == '.'  = if r == 0 then (init msg) ++ ((last msg) ++ "..") : []
                  else if r == 1 then (init msg) ++ ((cleanString $ last msg) ++ "!") : []
                    else msg
    | otherwise = (init msg) ++ ((last msg) ++ if elem (head msg) qWords then "?" else ".") : []
  where l = last $ last msg
        r = mod (length $ concat msg) 6

fHead :: a -> [a] -> a
{-# INLINE fHead #-}
fHead b [] = b
fHead _ c  = head c

fLast :: a -> [a] -> a
{-# INLINE fLast #-}
fLast b [] = b
fLast _ c  = last c

fTail :: [a] -> [a] -> [a]
{-# INLINE fTail #-}
fTail b [] = b
fTail _ c  = tail c

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
    type' []  = "Other"
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
    type' []  = UnknownEPos
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
    m  <- runs w $ meet emptyQueue (head s1) (head s2)
    if not (null s1) && not (null s2) then do
        let result = m
        if isNothing result then return [] else
            return $ replace '_' ' ' $ unwords $ map (++ "\"") $ map ('"' :) $
                    getWords $ getSynset $ fromJust result
        else return []

sentenceMeet :: WordNetEnv -> String -> [String] -> StateT (MVar ()) IO (Bool, String)
sentenceMeet _ _ [] = return (False, [])
sentenceMeet w s m  = do
    l    <- get :: StateT (MVar ()) IO (MVar ())
    lock <- lift $ takeMVar l
    out  <- lift $ sentenceMeet' s m
    lift $ putMVar l lock
    return out
  where
    sentenceMeet' :: String -> [String] -> IO (Bool, String)
    sentenceMeet' _  []     = return (False, [])
    sentenceMeet' s' (x:xs) = do
        a <- catch (meet' s' x) (\e -> return (e :: SomeException) >> return [])
        if null a then sentenceMeet' s' xs
          else if fHead [] a == s' && length x > 2 then return (True, x)
               else sentenceMeet' s' xs
    meet' c d = do
      s1 <- runs w $ search c Noun 1
      s2 <- runs w $ search d Noun 1
      if null s1 || null s2 then return []
        else do
          m' <- runs w $ meet emptyQueue (head s1) (head s2)
          if isNothing m' then return []
            else return $ getWords $ getSynset $ fromJust m'

asIsName :: (MVar ()) -> Aspell.SpellChecker -> String -> IO Bool
asIsName _ _ []    = return False
asIsName _ _ "i"   = return True
asIsName _ _ "I"   = return True
asIsName st aspell' word' = do
    let n = ["Can", "He", "Male"]
    let l = map toLower word'
    let u = toUpperWord l
    let b = toUpperLast l
    nl <- evalStateT (asSuggest aspell' l) st
    nb <- evalStateT (asSuggest aspell' b) st
    return $ if length word' < 3 || elem word' n then False
             else if (length $ words nb) < 3 then False
                  else if word' == (words nb)!!1 then False
                       else if (word' == (words nb)!!0 || u == (words nb)!!0) && (not $ null nl) then True
                            else False

isName :: (MVar ()) -> Aspell.SpellChecker -> Dict -> String -> IO Bool
isName _  _       _     [] = return False
isName st aspell' dict' w  = do
      n <- asIsName st aspell' w
      let ww = Map.lookup w dict'
      return $ if isJust ww then wordIs (fromJust ww) == "name" else n

asIsAcronym :: (MVar ()) -> Aspell.SpellChecker -> String -> IO Bool
asIsAcronym _  _       []    = return False
asIsAcronym _  _      (_:[]) = return False
asIsAcronym st aspell' word' = do
    let n = ["who"]
    let a = [toLower $ head word'] ++ (tail $ map toUpper word')
    let u = map toUpper word'
    let l = map toLower word'
    na <- evalStateT (asSuggest aspell' a) st
    return $ if (elem l n || elem l sWords) && word' /= u then False
             else if u == word' && null na then True
                  else if (length $ words na) > 2 && word' == (words na)!!1 then False
                       else if (not $ null na) && u == (words na)!!0 then True
                            else False

isAcronym :: (MVar ()) -> Aspell.SpellChecker -> Dict -> String -> IO Bool
isAcronym _  _       _     [] = return False
isAcronym st aspell' dict' w  = do
      a <- asIsAcronym st aspell' w
      let ww = Map.lookup w dict'
      return $ if isJust ww then wordIs (fromJust ww) == "acronym" else a

dictLookup :: (MVar ()) -> Fugly -> String -> String -> IO String
dictLookup st Fugly{wne=wne', aspell=aspell'} word' pos' = do
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

gfLin :: Maybe PGF -> String -> String
gfLin _    []   = []
gfLin pgf' msg
    | isJust pgf' && isJust expr = linearize (fromJust pgf') (head $ languages $ fromJust pgf') (fromJust expr)
    | otherwise = []
  where
    expr = readExpr msg

gfParseBool :: Maybe PGF -> Int -> String -> Bool
gfParseBool pgf' len msg
    | elem (map toLower lw) badEndWords = False
    | elem '\'' (map toLower lw)        = False
    | not $ isJust pgf'                 = True
    | len == 0       = True
    | length w > len = (gfParseBool' ipgf' $ take len w) && (gfParseBool pgf' len (unwords $ drop len w))
    | otherwise      = gfParseBool' ipgf' w
  where
    ipgf' = fromJust pgf'
    w  = words msg
    lw = strip '?' $ strip '.' $ last w

gfParseBool' :: PGF -> [String] -> Bool
gfParseBool' _    []                           = False
gfParseBool' pgf' msg
    | null $ parse pgf' lang (startCat pgf') m = False
    | otherwise                                = True
  where
    m = unwords msg
    lang = head $ languages pgf'

gfParseShow :: Maybe PGF -> String -> [String]
gfParseShow pgf' msg = if isJust pgf' then let ipgf' = fromJust pgf' in
                         lin ipgf' lang (parse_ ipgf' lang (startCat ipgf') Nothing msg)
                         else return "No PGF file..."
  where
    lin p l (ParseOk tl, _)      = map (lin' p l) tl
    lin _ _ (ParseFailed a, b)   = ["parse failed at " ++ show a ++
                                    " tokens: " ++ showBracketedString b]
    lin _ _ (ParseIncomplete, b) = ["parse incomplete: " ++ showBracketedString b]
    lin _ _ _                    = ["No parse!"]
    lin' p l t                   = "parse: " ++ showBracketedString (head $ bracketedLinearize p l t)
    lang = head $ languages $ fromJust pgf'

gfCategories :: PGF -> [String]
gfCategories pgf' = map showCId (categories pgf')

gfRandom :: Maybe PGF -> String -> IO String
gfRandom pgf' [] = do
    if isJust pgf' then do
      r <- gfRandom' $ fromJust pgf'
      let rr = filter (\x -> x Regex.=~ "NP") $ words r
      gfRandom pgf' (if rr == (\\) rr (words r) then r else [])
      else return "I'm not really sure what to make of that."
gfRandom _ msg' = return msg'

gfRandom' :: PGF -> IO String
gfRandom' pgf' = do
    num <- Random.getStdRandom (Random.randomR (0, 9999))
    return $ dePlenk $ unwords $ toUpperSentence $ endSentence $
      filter (not . null) $ map cleanString $ take 12 $ words $ gfRand num
  where
    gfRand n = linearize pgf' (head $ languages pgf') $ head $
               generateRandomDepth (Random.mkStdGen n) pgf' (startCat pgf') (Just n)

gfShowExpr :: PGF -> String -> Int -> String
gfShowExpr pgf' type' num = if isJust $ readType type' then
    let c = fromJust $ readType type' in
    head $ filter (not . null) $ map (\x -> fromMaybe [] (unStr x))
      (generateRandomDepth (Random.mkStdGen num) pgf' c (Just num))
                            else "Not a GF type."

sentenceA :: MVar () -> Fugly -> Int -> Bool -> Bool -> Bool -> Int -> Int
             -> Int -> String -> [String] -> IO String
sentenceA _ _ _ _ _ _ _ _ _ _ [] = return []
sentenceA st fugly@Fugly{pgf=pgf', aspell=aspell', wne=wne', nset=nset', nmap=nmap'}
  r debug rwords stopic randoms stries slen topic' msg = do
    rr <- Random.getStdRandom (Random.randomR (0, 99)) :: IO Int
    s1a rr (length msg) msg
  where
    s1a :: Int -> Int -> [String] -> IO String
    s1a _ _ [] = return []
    s1a r' l w
      | l < 16 && r < 75 = do
        nnReply nset' nmap' 5000 w
      | l < 7 && s2l w Regex.=~ "hi$|hello|greetings|welcome" = return (case mod r' 6 of
         0 -> "Hello my friend."
         1 -> "Hi there."
         2 -> "Thanks."
         3 -> "Hey there pal."
         4 -> "Good morning."
         5 -> "Hey, what's going on?"
         _ -> [])
      | l == 1 && r < 70 = do { mm <- fixIt st debug (sentenceB' st fugly r debug True rwords stopic 0 10 5 5 topic' w) [] 1 0 0 50 ; return $ unwords mm }
      | l < 4 && r < 60 || r + r' < 20 = case mod r' 4 of
         0 -> do { mm <- fixIt st debug (return ("Do you like " ++ topic' ++ "?") : sentenceB' st fugly r debug True rwords stopic 5 10 4 4 topic' [topic']) [] 2 0 0 40 ; return $ unwords mm }
         1 -> do { mm <- fixIt st debug (return ("Not really.") : sentenceB' st fugly r debug True rwords stopic randoms 10 6 6 topic' (words "perhaps you")) [] 2 0 0 60 ; return $ unwords mm }
         2 -> do { mm <- fixIt st debug (sentenceB' st fugly r debug True rwords stopic 75 10 7 7 topic' $ words "can you") [] 1 0 0 70 ; return $ unwords mm }
         3 -> do { mm <- fixIt st debug (sentenceB' st fugly r debug True rwords stopic randoms 10 6 6 topic' $ words "I think that") [] 1 0 0 60 ; return $ unwords mm }
         _ -> return []
      | l < 7 && r' < 31 && s2l w == "test" = return (case mod r' 4 of
         0 -> "What are we testing?"
         1 -> "But I don't want to test..."
         2 -> "Is this just a test?"
         3 -> "Test it yourself."
         _ -> [])
      | l < 8 && r' < 17 && (map toLower $ unwords w) Regex.=~ "really" = return (case mod r' 3 of
         0 -> "Not really."
         1 -> "Yes really."
         2 -> "Oh really."
         _ -> [])
      | l < 9 && r' < 65 && (map toLower $ unwords w) Regex.=~ "lol|haha|hehe|rofl|lmao|funny|humorous" = return (case mod r' 7 of
         0 -> "Very funny."
         1 -> "What's so funny?"
         2 -> "It's not funny."
         3 -> "Please don't laugh."
         4 -> "Oh really."
         5 -> "I'm glad you think this is funny."
         6 -> "Seriously."
         _ -> [])
      | l > 2 && s2l w == "hey" = do
          m' <- fixIt st debug (sentenceB' st fugly r debug False rwords stopic randoms stries slen 5 topic' (drop 2 w)
                                ++ [gfRandom pgf' []]) [] 1 0 0 (stries * slen)
          w' <- s1r r' m'
          x' <- asReplaceWords st fugly w'
          return $ unwords $ toUpperSentence $ endSentence x'
      | (map toLower $ unwords w) Regex.=~ "rhyme|rhymes|sing.*|song|songs" || r' > 87 = do
          m' <- fixIt st debug (sentenceB' st fugly r debug False rwords stopic randoms stries slen 5 topic' w
                                ++ [gfRandom pgf' []]) [] 1 0 0 (stries * slen)
          w' <- s1r r' m'
          x' <- asReplaceWords st fugly w'
          return $ unwords $ toUpperSentence $ endSentence x'
      | l > 3 && (s1l $ take 3 w) == ["do", "you", w!!2 Regex.=~ "like|hate|love|have|want|need"] = do
        noun <- getNoun wne' r' w
        let nouns' = nouns noun
        let s1b rr ww = ww!!2 Regex.=~ "like|hate|love|have|want|need" ++ " " ++
                        if rr < 20 then "it" else if rr < 40 then "that" else nouns' in
          return $ unwords $ toUpperSentence $ endSentence $ words (case mod r' 4 of
            0 -> "I don't " ++ s1b r' w
            1 -> "yeah, I " ++ s1b r' w
            2 -> "sometimes I " ++ s1b r' w
            3 -> (noun ++ "s") ++ " are " ++ if r' < 50 then "not" else "" ++ " something I " ++ w!!2 Regex.=~ "like|hate|love|have|want|need"
            _ -> [])
      | l > 2 && (s1l $ take 2 w) == ["can", "you"] = return (case mod r' 5 of
         0 -> "No I can't."
         1 -> "Sure, I can do that."
         2 -> "It depends..."
         3 -> "Why would I want to do something like that?"
         4 -> "It is " ++ if r' < 20 then "boring" else if r' < 50 then "fun" else "certainly possible."
         _ -> [])
      | l > 2 && r' > 25 && elem (s2l w) qWords || r' > 65 && (map toLower $ unwords w) Regex.=~ intercalate "|" qWords = do
         noun <- getNoun wne' r' w
         let nouns' = nouns noun
         case mod r' 9 of
           0 -> do { mm <- fixIt st debug (return ("Yes, the " ++ noun ++ " is okay.") : sentenceB' st fugly r debug True rwords stopic 30 5 5 5 topic' [noun]) [] 2 0 0 25 ; return $ unwords mm }
           1 -> do { mm <- fixIt st debug (return "No, not really." : sentenceB' st fugly r debug True rwords stopic 30 5 7 7 topic' [noun]) [] 2 0 0 35 ; return $ unwords mm }
           2 -> do { mm <- fixIt st debug (sentenceB' st fugly r debug True rwords stopic 75 5 6 6 topic' [noun]) [] 1 0 0 30 ; return $ unwords mm }
           3 -> do { mm <- fixIt st debug (sentenceB' st fugly r debug True rwords stopic randoms 5 5 5 topic' ["sometimes"]) [] 1 0 0 25 ; return $ unwords mm }
           4 -> do { mm <- fixIt st debug (return "Yes." : sentenceB' st fugly r debug True rwords stopic 10 5 7 7 topic' ["the", noun, "is"]) [] 2 0 0 35 ; return $ unwords mm }
           5 -> return ("Maybe, but I don't really like " ++ nouns' ++ ".")
           6 -> return ("I'm not really sure about " ++ nouns' ++ ".")
           7 -> return ("But " ++ nouns' ++ " are boring.")
           8 -> return ("Let's discuss " ++ nouns' ++ " later.")
           _ -> return []
      | l > 3 && l < 15 = do
          let ra = mod r 4
          let rb = mod r' 5
          let t  = case ra of
                0 -> "food"
                1 -> "animal"
                2 -> "tool"
                3 -> "abstraction"
                _ -> []
          (a, b) <- evalStateT (sentenceMeet wne' t w) st
          if a then case ra of
                0 -> case rb of
                  0 -> return ("Food is delicious, especially " ++ b ++ "!")
                  1 -> return ("I like to eat the " ++ b ++ ".")
                  _ -> do { mm <- fixIt st debug (return ("Oh yes, " ++ b ++ ".") : sentenceB' st fugly r debug True rwords stopic randoms 5 9 9 topic' [b, "is", "tasty", "and"]) [] 2 0 0 45 ; return $ unwords mm }
                1 -> case rb of
                  0 -> return ("Animals are cute, I really like the " ++ b ++ ".")
                  1 -> return ("The " ++ b ++ " is an adorable animal.")
                  _ -> do { mm <- fixIt st debug (sentenceB' st fugly r debug True rwords stopic randoms 8 5 5 topic' [b, "is", "adorable"]) [] 1 0 0 40 ; return $ unwords mm }
                2 -> case rb of
                  0 -> return "Tools are very useful things."
                  1 -> return ("Can you please lend me your " ++ b ++ "?")
                  _ -> do { mm <- fixIt st debug (sentenceB' st fugly r debug True rwords stopic randoms 5 6 6 topic' ["tools"]) [] 1 0 0 30 ; return $ unwords mm }
                3 -> case rb of
                  0 -> return "This is too abstract for me."
                  1 -> return ("I find " ++ b ++ " to be a rather esoteric subject for discussion.")
                  _ -> do { mm <- fixIt st debug (sentenceB' st fugly r debug True rwords stopic randoms 5 9 9 topic' ["abstract", b, "is"]) [] 1 0 0 45 ; return $ unwords mm }
                _ -> return []
            else return []
      | otherwise = return []
    s1l = map (\x -> map toLower x)
    s2l x = map toLower $ head x
    s1r rr mm = mapM (\x -> do ry <- rhymesWith st aspell' x
                               if null ry then return []
                                 else return $ ry!!(mod rr (length ry))) mm

sentenceB :: (MVar ()) -> Fugly -> Int -> Bool -> Bool -> Bool -> Int -> Int -> Int
             -> Int -> String -> Int -> [String] -> IO [String]
sentenceB st fugly@Fugly{pgf=pgf'} r debug rwords stopic randoms stries slen plen topic' num msg = do
    m <- chooseWord msg
    let mm = if length msg < 4 || mod (length $ concat msg) 3 == 0 then msg else m
    fixIt st debug (sentenceB' st fugly r debug False rwords stopic randoms stries slen plen topic' mm ++
                    [gfRandom pgf' []]) [] num 0 0 (stries * slen)

sentenceB' :: (MVar ()) -> Fugly -> Int -> Bool -> Bool -> Bool -> Bool -> Int
               -> Int -> Int -> Int -> String -> [String] -> [IO String]
sentenceB' _ _ _ _ _ _ _ _ _ _ _ _ [] = [return []] :: [IO String]
sentenceB' st fugly@Fugly{dict=dict', pgf=pgf', wne=wne', aspell=aspell'}
  r debug first rwords stopic randoms stries slen plen topic' msg = do
    let s1h n a x = let out = if a then map toUpper x else if n then x else map toLower x in
          if isJust $ Map.lookup out dict' then out else []
    let s1a x = do
          a <- isAcronym st aspell' dict' x
          n <- isName st aspell' dict' x
          b <- getNoun wne' r msg
          z <- findNextWord fugly 0 randoms True stopic topic' b x
          let zz = fHead [] z
          y <- findNextWord fugly 1 randoms True stopic topic' b zz
          let yy = fHead [] y
          let c  = if null zz && null yy then 2 else if null zz || null yy then 3 else 4
          w  <- s1b fugly slen c b $ findNextWord fugly 1 randoms False stopic topic' b x
          ww <- s1b fugly slen 0 b $ return msg
          let d  = if first then ww else [yy] ++ [zz] ++ [s1h n a x] ++ w
          wnReplaceWords fugly rwords randoms $ filter (not . null) $ take (stries * slen) d
    let s1d x = do
          w <- x
          if null w then return []
            else return (init w ++ (cleanString (last w) ++ if elem (map toLower $ head w) qWords then "?" else ".") : [])
    let s1e x = do
          w <- x
          n <- isName st aspell' dict' $ fHead [] w
          if null w || n then return []
            else return ([s1c w] ++ tail w)
    let s1g = map (\x -> do y <- insertCommas wne' 0 x ; return $ dePlenk $ unwords y) (map (s1e . s1d . s1a) (msg ++ sWords))
    s1f 0 s1t s1g
  where
    s1b :: Fugly -> Int -> Int -> String -> IO [String] -> IO [String]
    s1b f n i noun msg' = do
      ww <- msg'
      if null $ concat ww then return []
        else if i >= n then return $ dedup ww else do
               www <- findNextWord f i randoms False stopic topic' noun $ fLast [] ww
               s1b f n (i + 1) noun (return $ ww ++ www)
    s1c :: [String] -> String
    s1c [] = []
    s1c w  = [toUpper $ head $ head w] ++ (fTail [] $ head w)
    s1t :: Int -> IO String -> IO String
    s1t i x = do
      y <- x
      p <- wnPartPOS wne' $ cleanString $ fLast [] $ words y
      let plen' = if ((realToFrac i) :: Float) > (((realToFrac stries) :: Float) / 2) then 0 else plen
      if null y then return []
        else if gfParseBool pgf' plen' y && length (words y) > 2 && p /= POS Adj then return y
             else if debug then evalStateT (hPutStrLnLock stdout ("> debug: sentence try: " ++ y)) st >> return []
                  else return []
    s1f _ _ []     = []
    s1f i f (x:xs) = f i x : s1f (i + 1) f xs

fixIt :: (MVar ()) -> Bool -> [IO String] -> [String] -> Int -> Int
         -> Int -> Int -> IO [String]
fixIt _  _ []     a _ _ _ _ = return a
fixIt st d (x:xs) a n i j s = do
    xx <- x
    _  <- if d then evalStateT (hPutStrLnLock stdout ("> debug: fixIt try: " ++ show j)) st else return ()
    if i >= n || j > s * n then return a
      else if null xx then fixIt st d xs a n i (j + 1) s
      else fixIt st d xs (a ++ [(if null a then [] else " ") ++ xx]) n (i + 1) j s

insertCommas :: WordNetEnv -> Int -> IO [String] -> IO [String]
insertCommas wne' i w = do
    w' <- w
    r  <- Random.getStdRandom (Random.randomR (0, 7)) :: IO Int
    let x  = fHead [] w'
    let xs = fTail [] w'
    let y  = fHead [] xs
    let l  = length xs
    let bad = ["a", "am", "an", "and", "as", "but", "by", "for", "from", "had", "has", "have", "I", "in", "is", "of", "on", "or", "that", "the", "this", "to", "very", "was", "with"]
    let match' = ["a", "but", "however", "the", "then", "though"]
    px <- wnPartPOS wne' x
    py <- wnPartPOS wne' y
    if l < 1 then w
      else if elem x bad || elem '\'' x || i < 1 || l < 4 then do
        xs' <- insertCommas wne' (i + 1) $ return xs
        return (x : xs')
           else if (elem y match') && r < 4 then do
             xs' <- insertCommas wne' 0 $ return xs
             return ((x ++ if r < 1 then ";" else ",") : xs')
                else if px == POS Noun && (py == POS Noun || py == POS Adj) && r < 3 && y /= "or" && y /= "and" then do
                  xs' <- insertCommas wne' 0 $ return xs
                  return ((x ++ if r < 2 then ", or" else ", and") : xs')
                     else if px == POS Adj && py == POS Adj && r < 2 then do
                       xs' <- insertCommas wne' 0 $ return xs
                       return ((x ++ " and") : xs')
                          else do
                            xs' <- insertCommas wne' (i + 1) $ return xs
                            return (x : xs')

chooseWord :: [String] -> IO [String]
chooseWord []  = return []
chooseWord msg = do
    cc <- c1 msg
    c2 cc []
  where
    c1 m = do
      r <- Random.getStdRandom (Random.randomR (0, 4)) :: IO Int
      if r < 3 then return m
        else return $ reverse m
    c2 [] m  = return m
    c2 [x] m = return (m ++ [x])
    c2 (x:xs) m = do
      r <- Random.getStdRandom (Random.randomR (0, 1)) :: IO Int
      if r == 0 then c2 xs (m ++ [x])
        else c2 ([x] ++ tail xs) (m ++ [head xs])

filterWordPOS :: WordNetEnv -> EPOS -> [String] -> IO [String]
filterWordPOS _    _    []  = return []
filterWordPOS wne' pos' msg = fpos [] msg
  where
    fpos a []     = return a
    fpos a (x:xs) = do
      p <- wnPartPOS wne' x
      if p == pos' then fpos (x : a) xs
        else fpos a xs

getNoun :: WordNetEnv -> Int -> [String] -> IO String
getNoun wne' r w = do
    ww <- filterWordPOS wne' (POS Noun) w
    let ww' = filter (\x -> length x > 2 && (not $ elem x qWords)) ww
    let n   = case mod r 9 of
          0 -> "thing"
          1 -> "stuff"
          2 -> "idea"
          3 -> "belief"
          4 -> "debate"
          5 -> "conversation"
          6 -> "cat"
          7 -> "bacon"
          8 -> "pony"
          _ -> []
    let n'  = if null ww' then n else ww'!!(mod r $ length ww')
    return $ if last n' == 's' then init n' else if length n' < 3 then n else n'

nouns :: String -> String
nouns []      = "stuff"
nouns "stuff" = "stuff"
nouns n       = if last n == 'y' then (init n) ++ "ies" else n ++ "s"

wnReplaceWords :: Fugly -> Bool -> Int -> [String] -> IO [String]
wnReplaceWords _                               _     _       []  = return []
wnReplaceWords _                               False _       msg = return msg
wnReplaceWords fugly@Fugly{wne=wne'} True  randoms msg = do
    cw <- chooseWord msg
    cr <- Random.getStdRandom (Random.randomR (0, (length cw) - 1))
    rr <- Random.getStdRandom (Random.randomR (0, 99))
    w  <- findRelated wne' (cw!!cr)
    let ww = if elem (map toLower w) sWords then cw!!cr else w
    if randoms == 0 then
      return msg
      else if randoms == 100 then
             mapM (\x -> findRelated wne' x) msg
           else if rr + 20 < randoms then
                  wnReplaceWords fugly True randoms $ filter (not . null)
                  ((takeWhile (/= (cw!!cr)) msg) ++ [ww] ++ (tail $ dropWhile (/= (cw!!cr)) msg))
                else
                  return msg

asReplaceWords :: (MVar ()) -> Fugly -> [String] -> IO [String]
asReplaceWords _  _     []  = return [[]]
asReplaceWords st fugly msg = do
    mapM (\x -> asReplace st fugly x) msg

asReplace :: (MVar ()) -> Fugly -> String -> IO String
asReplace _  _                                           []    = return []
asReplace st Fugly{dict=dict', wne=wne', aspell=aspell'} word' = do
    n  <- asIsName st aspell' word'
    ac <- asIsAcronym st aspell' word'
    if (elem ' ' word') || (elem '\'' word') || word' == (toUpperWord word') || n || ac then return word'
      else do
      a <- evalStateT (asSuggest aspell' word') st
      p <- wnPartPOS wne' word'
      let w  = Map.lookup word' dict'
      let rw = filter (\x -> (not $ elem '\'' x) && levenshteinDistance defaultEditCosts word' x < 4) $ words a
      rr <- Random.getStdRandom (Random.randomR (0, (length rw) - 1))
      if null rw || p /= UnknownEPos || isJust w then return word' else
        if head rw == word' then return word' else return $ map toLower (rw!!rr)

rhymesWith :: MVar () -> Aspell.SpellChecker -> String -> IO [String]
rhymesWith _  _       []    = return []
rhymesWith st aspell' word' = do
    let l = map toLower word'
    let b = toUpperLast l
    as <- evalStateT (asSuggest aspell' b) st
    let asw = words as
    let end = case length l of
          1 -> l
          2 -> l
          3 -> drop 1 l
          4 -> drop 2 l
          x -> drop (x-3) l
    let out = filter (\x -> (not $ elem '\'' x) && length x > 2 && length l > 2 &&
                       x Regex.=~ (end ++ "$") && levenshteinDistance defaultEditCosts l x < 4) asw
    return $ if null out then [word'] else out

findNextWord :: Fugly -> Int -> Int -> Bool -> Bool -> String -> String -> String -> IO [String]
findNextWord _                 _ _       _    _      _      _    []    = return []
findNextWord Fugly{dict=dict'} i randoms prev stopic topic' noun word' = do
    let ln = if isJust w then length neigh else 0
    let lm = if isJust w then length neighmax else 0
    let ll = if isJust w then length neighleast else 0
    nr <- Random.getStdRandom (Random.randomR (0, ln - 1))
    mr <- Random.getStdRandom (Random.randomR (0, lm - 1))
    lr <- Random.getStdRandom (Random.randomR (0, ll - 1))
    rr <- Random.getStdRandom (Random.randomR (0, 99))
    let f1 = if isJust w && ll > 0 then neighleast!!lr else []
    let f2 = if isJust w then case mod i 3 of
          0 -> if ln > 0 then neigh!!nr else []
          1 -> if ll > 0 then neighleast!!lr else []
          2 -> if ll > 0 then neighleast!!lr else []
          _ -> []
             else []
    let f3 = if isJust w then case mod i 5 of
          0 -> if ll > 0 then neighleast!!lr else []
          1 -> if ln > 0 then neigh!!nr else []
          2 -> if lm > 0 then neighmax!!mr else []
          3 -> if ln > 0 then neigh!!nr else []
          4 -> if ln > 0 then neigh!!nr else []
          _ -> []
             else []
    let f4 = if isJust w then case mod i 3 of
          0 -> if lm > 0 then neighmax!!mr else []
          1 -> if ln > 0 then neigh!!nr else []
          2 -> if lm > 0 then neighmax!!mr else []
          _ -> []
             else []
    let f5 = if isJust w && lm > 0 then neighmax!!mr else []
    let out = if randoms > 89 then words f1 else
                if rr < randoms - 25 then words f2 else
                  if rr < randoms + 35 then words f3 else
                    if rr < randoms + 65 then words f4 else
                      words f5
    return out
    where
      w        = Map.lookup word' dict'
      wordGet' = if prev then wordGetBefore else wordGetAfter
      neigh_all'       = listNeigh $ wordGet' (fromJust w)
      neigh_all        = if elem noun neigh_all' then [noun] else neigh_all'
      neigh_topic'     = listNeighTopic dict' topic' neigh_all'
      neigh_topic      = if elem noun neigh_topic' then [noun] else neigh_topic'
      neighmax_all'    = listNeighMax $ wordGet' (fromJust w)
      neighmax_all     = if elem noun neighmax_all' then [noun] else neighmax_all'
      neighmax_topic'  = listNeighTopic dict' topic' neighmax_all'
      neighmax_topic   = if elem noun neighmax_topic' then [noun] else neighmax_topic'
      neighleast_all   = listNeighLeast $ wordGet' (fromJust w)
      neighleast_topic = listNeighTopic dict' topic' neighleast_all
      neigh      = if stopic then neigh_topic else if null neigh_topic then neigh_all else neigh_topic
      neighmax   = if stopic then neighmax_topic else if null neighmax_topic then neighmax_all else neighmax_topic
      neighleast = if stopic then neighleast_topic else if null neighleast_topic then neighleast_all else neighleast_topic

findRelated :: WordNetEnv -> String -> IO String
findRelated wne' word' = do
    pp <- wnPartPOS wne' word'
    out <- do if pp /= UnknownEPos then do
              hyper <- wnRelated' wne' word' "Hypernym" pp
              hypo  <- wnRelated' wne' word' "Hyponym" pp
              anto  <- wnRelated' wne' word' "Antonym" pp
              let hyper' = filter (\x -> (not $ elem ' ' x) && length x > 2) $ map (strip '"') hyper
              let hypo'  = filter (\x -> (not $ elem ' ' x) && length x > 2) $ map (strip '"') hypo
              let anto'  = filter (\x -> (not $ elem ' ' x) && length x > 2) $ map (strip '"') anto
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

wordToDouble :: String -> Double
wordToDouble w = c $ (sum $ stringToDL w) / (realToFrac $ length w :: Double)
  where
    c n
      | n > 1     = 1.0
      | n < 0     = 0.0
      | otherwise = n
    stringToDL w' = map (fun . realToFrac . ord) w' :: [Double]
    fun a = c $ (a - 97) / 25

doubleToWord :: NMap -> Double -> String
doubleToWord _ 1.0 = []
doubleToWord m d   = let r = Map.lookup (floor $ d * 1000) m in
    if isJust r then fromJust r else doubleToWord m $ d + 0.007

nnInsert :: NSet -> NMap -> [String] -> [String] -> (NSet, NMap)
nnInsert nset' nmap' []    _      = (nset', nmap')
nnInsert nset' nmap' _    []      = (nset', nmap')
nnInsert nset' nmap' input output =
  let a = (map wordToDouble $ low input, (map wordToDouble $ low output) ++ take 16 (cycle [0])) : nset'
      b = foldr (\x -> Map.insert (mKey x) x) nmap' input in
  (a, b)
  where
    low = map (map toLower)
    mKey w = floor $ (wordToDouble w) * 1000

nnReply :: NSet -> NMap -> Int -> [String] -> IO String
nnReply []    _     _    _   = return []
nnReply _     _     _    []  = return []
nnReply nset' nmap' numg msg = do
    g <- Random.newStdGen
    let nnet = fst $ randomNeuralNetwork g [fromIntegral (length msg) :: Word16,
                 8, 8, 16] [Tanh, Tanh, Logistic] 0.3
    n <- backpropagationBatchParallel nnet nset' 0.43 nstop :: IO (NeuralNetwork Double)
    return $ unwords $ toUpperSentence $ endSentence $ nnAnswer n
  where
    nstop _ num = do
      return $ num >= numg
    nnAnswer nnet' = map (\x -> doubleToWord nmap' x) $ runNeuralNetwork nnet' $ map wordToDouble msg

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
