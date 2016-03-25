module FuglyLib where

import           Control.Concurrent             (MVar, putMVar, takeMVar)
import           Control.Exception
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.State.Lazy (StateT, evalStateT, get)
import qualified Data.ByteString.Char8          as ByteString
import           Data.Char
import           Data.List
import qualified Data.Map.Lazy                  as Map
import           Data.Maybe
import           Data.Tree                      (flatten)
import           Fugly.Types                    hiding (fugly)
import qualified Language.Aspell                as Aspell
import           NLP.WordNet                    hiding (Word)
import           NLP.WordNet.Prims              (indexLookup, senseCount, getSynset, getWords, getGloss)
import           NLP.WordNet.PrimTypes          hiding (pos)
import           PGF
import           Prelude                        hiding (Word)
import qualified System.Random                  as Random
import           System.IO
import           Text.EditDistance              as EditDistance
import qualified Text.Regex.Posix               as Regex

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
    wordGetPos Word{pos=p}              = p
    wordGetPos _                        = UnknownEPos
    wordGetwc (Word w c _ _ _ _ _ _)    = (c, w)
    wordGetwc (Name n c _ _ _ _)        = (c, n)
    wordGetwc (Acronym a c _ _ _ _ _)   = (c, a)

emptyWord :: Word
emptyWord = Word [] 0 Map.empty Map.empty [] [] [] UnknownEPos

qWords :: [String]
qWords = ["am", "are", "can", "could", "did", "do", "does", "have", "if",
          "is", "should", "want", "was", "were", "what", "when", "where",
          "who", "why", "will"]

badEndWords :: [String]
badEndWords = ["a", "about", "am", "an", "and", "are", "as", "at", "but",
               "by", "do", "every", "for", "from", "gave", "go", "got",
               "had", "has", "he", "her", "he's", "his", "i", "i'd", "if",
               "i'll", "i'm", "in", "into", "is", "it", "its", "it's", "i've",
               "just", "make", "makes", "mr", "mrs", "my", "no", "of", "oh",
               "on", "or", "our", "person's", "she", "she's", "so", "than",
               "that", "that's", "the", "their", "there's", "they", "they're",
               "to", "us", "very", "was", "we", "were", "what", "when",
               "where", "which", "why", "with", "who", "whose", "yes", "you",
               "your", "you're", "you've"]

sWords :: [String]
sWords = ["a", "am", "an", "as", "at", "by", "do", "go", "he", "i", "if",
          "in", "is", "it", "me", "my", "no", "of", "oh", "on", "or", "so",
          "to", "us", "we", "yo"]

insertWords :: MVar () -> Fugly -> Bool -> String -> [String] -> IO Dict
insertWords _ Fugly{dict=dict'} _ _ [] = return dict'
insertWords lock fugly autoname topic' [x] = insertWord lock fugly autoname topic' x [] [] []
insertWords lock fugly@Fugly{dict=dict'} autoname topic' msg =
  let mx = fHead [] dmsg
      my = if len > 1 then dmsg!!1 else [] in
  case (len) of
    0 -> return dict'
    1 -> return dict'
    2 -> do ff <- insertWord lock fugly autoname topic' mx [] my []
            insertWord lock fugly{dict=ff} autoname topic' my mx [] []
    _ -> insertWords' fugly autoname topic' 0 len dmsg
  where
    dmsg = dedupD msg
    len  = length dmsg
    insertWords' :: Fugly -> Bool -> String -> Int -> Int -> [String] -> IO Dict
    insertWords' (Fugly{dict=d}) _ _ _ _ [] = return d
    insertWords' f@(Fugly{dict=d}) a t i l m
      | i == 0     = do ff <- insertWord lock f a t (m!!i) [] (m!!(i+1)) []
                        insertWords' f{dict=ff} a t (i+1) l m
      | i > l - 1  = return d
      | i == l - 1 = do ff <- insertWord lock f a t (m!!i) (m!!(i-1)) [] []
                        insertWords' f{dict=ff} a t (i+1) l m
      | otherwise  = do ff <- insertWord lock f a t (m!!i) (m!!(i-1)) (m!!(i+1)) []
                        insertWords' f{dict=ff} a t (i+1) l m

insertWordsN :: MVar () -> Fugly -> Bool -> String -> Int -> [String] -> IO Dict
insertWordsN _ Fugly{dict=dict'} _ _ _ [] = return dict'
insertWordsN lock f@(Fugly{dict=dict'}) an top num msg = f1 0 dict'
  where
    f1 n d = do
      nd <- insertWords lock f{dict=d} an top msg
      if n < num then f1 (n + 1) nd else return nd

insertWord :: MVar () -> Fugly -> Bool -> String -> String
              -> String -> String -> String -> IO Dict
insertWord _ Fugly{dict=dict'} _ _ [] _ _ _ = return dict'
insertWord lock fugly@Fugly{dict=dict', aspell=aspell', ban=ban'}
    autoname topic' word' before' after' pos' = do
    n   <- asIsName lock aspell' word'
    nb  <- asIsName lock aspell' before'
    na  <- asIsName lock aspell' after'
    ac  <- asIsAcronym lock aspell' word'
    acb <- asIsAcronym lock aspell' before'
    aca <- asIsAcronym lock aspell' after'
    insert' n nb na ac acb aca
  where
    lowerword = map toLower . cleanString
    upperword = toUpperWord . cleanString
    acroword  = map toUpper . cleanString
    w   = Map.lookup word' dict'
    wa  = Map.lookup (acroword  word') dict'
    wn  = Map.lookup (upperword word') dict'
    ww  = Map.lookup (lowerword word') dict'
    b   = Map.lookup before' dict'
    ba' = Map.lookup (acroword  before') dict'
    bn' = Map.lookup (upperword before') dict'
    bw' = Map.lookup (lowerword before') dict'
    insert' n nb na ac acb aca
      | (length word' < 3) && (notElem (map toLower word') sWords) = return dict'
      | isJust w = f nb na acb aca $ fromJust w
      | ac && autoname = if elem (acroword word') ban' then return dict' else
                           insertAcroRaw' lock fugly wa (acroword word') (bi nb acb) (ai na aca) topic' []
      | n && autoname = if elem (upperword word') ban' then return dict' else
                          insertNameRaw' lock fugly False wn word' (bi nb acb) (ai na aca) topic'
      | isJust ww = insertWordRaw' lock fugly False ww (lowerword word') (bi nb acb) (ai na aca) topic' pos'
      | otherwise = insertWordRaw' lock fugly False w (lowerword word') (bi nb acb) (ai na aca) topic' pos'
    ai an aa
      | isJust w && elem after' (wordGetBanAfter $ fromJust w) = []
      | (length after' < 3) && (notElem (map toLower after') sWords) = []
      | aa && autoname = if elem (acroword after') ban' then [] else
                           if isJust wa && elem (acroword after') (wordGetBanAfter $ fromJust wa) then []
                           else acroword after'
      | an && autoname = if elem (upperword after') ban' then [] else
                           if isJust wn && elem (upperword after') (wordGetBanAfter $ fromJust wn) then []
                           else upperword after'
      | isJust ww = if elem (lowerword after') (wordGetBanAfter $ fromJust ww) then []
                    else lowerword after'
      | otherwise = lowerword after'
    bi bn ba
      | isJust b && elem word' (wordGetBanAfter $ fromJust b) = []
      | (length before' < 3) && (notElem (map toLower before') sWords) = []
      | ba && autoname = if elem (acroword before') ban' then [] else
                           if isJust ba' && elem (acroword word') (wordGetBanAfter $ fromJust ba') then []
                           else acroword before'
      | bn && autoname = if elem (upperword before') ban' then [] else
                           if isJust bn' && elem (upperword word') (wordGetBanAfter $ fromJust bn') then []
                           else upperword before'
      | isJust bw' = if elem (lowerword word') (wordGetBanAfter $ fromJust bw') then []
                     else lowerword before'
      | otherwise = lowerword before'
    f bn an ba aa Word{}    = insertWordRaw' lock fugly False w  word'            (bi bn ba) (ai an aa) topic' pos'
    f bn an ba aa Name{}    = insertNameRaw' lock fugly True  w  word'            (bi bn ba) (ai an aa) topic'
    f bn an ba aa Acronym{} = insertAcroRaw' lock fugly       wa (acroword word') (bi bn ba) (ai an aa) topic' []

insertWordRaw :: MVar () -> Fugly -> Bool -> String -> String -> String
                 -> String -> String -> IO Dict
insertWordRaw lock f@Fugly{dict=d} s w b a t p =
    insertWordRaw' lock f s (Map.lookup w d) w b a t p

insertWordRaw' :: MVar () -> Fugly -> Bool -> Maybe Word -> String -> String
                  -> String -> String -> String -> IO Dict
insertWordRaw' _ Fugly{dict=dict'} _ _ [] _ _ _ _ = return dict'
insertWordRaw' lock Fugly{dict=dict', wne=wne', aspell=aspell'}
    strict w word' before' after' topic' pos' = do
    pp <- (if null pos' then wnPartPOS wne' word' else return $ readEPOS pos')
    pa <- wnPartPOS wne' after'
    pb <- wnPartPOS wne' before'
    rel <- wnRelated' wne' word' "Hypernym" pp
    as <- evalStateT (asSuggest aspell' word') lock
    let asw = words as
    let nn x y  = if isJust $ Map.lookup x dict' then x else
                    if y == UnknownEPos && Aspell.check aspell'
                       (ByteString.pack x) == False then [] else x
    let insert' x = Map.insert x (Word x 1 (e (nn before' pb)) (e (nn after' pa)) [] [topic'] rel pp) dict'
    let msg w' = evalStateT (hPutStrLnLock stdout ("> inserted new word: " ++ w')) lock
    if isJust w then let w' = fromJust w in
      return $ Map.insert word' w'{count=c, before=nb before' pb, after=na after' pa,
                                   topic=sort $ nub (topic' : wordGetTopic w')} dict'
      else if strict then msg word' >> return (insert' word') else
      if pp /= UnknownEPos || Aspell.check aspell' (ByteString.pack word') then msg word' >> return (insert' word')
      else if (length asw) > 0 then let hasw = head asw in
        if (length hasw < 3 && (notElem (map toLower hasw) sWords)) || (isJust $ Map.lookup hasw dict')
        then return dict' else msg hasw >> return (insert' hasw)
           else return dict'
    where
      e [] = Map.empty
      e x  = Map.singleton x 1
      c = incCount' (fromJust w) 1
      na x y = if isJust $ Map.lookup x dict' then incAfter' (fromJust w) x 1 else
                 if y /= UnknownEPos || Aspell.check aspell' (ByteString.pack x) then
                   incAfter' (fromJust w) x 1
                 else wordGetAfter (fromJust w)
      nb x y = if isJust $ Map.lookup x dict' then incBefore' (fromJust w) x 1 else
                 if y /= UnknownEPos || Aspell.check aspell' (ByteString.pack x) then
                   incBefore' (fromJust w) x 1
                 else wordGetBefore (fromJust w)

insertNameRaw :: MVar () -> Fugly -> Bool -> String -> String -> String
                 -> String -> IO Dict
insertNameRaw lock f@Fugly{dict=dict'} s w b a t =
    insertNameRaw' lock f s (Map.lookup w dict') w b a t

insertNameRaw' :: MVar () -> Fugly -> Bool -> Maybe Word -> String -> String
                  -> String -> String -> IO Dict
insertNameRaw' _ Fugly{dict=dict'} _ _ [] _ _ _ = return dict'
insertNameRaw' lock Fugly{dict=dict', wne=wne', aspell=aspell'}
    strict w name' before' after' topic' = do
    pa <- wnPartPOS wne' after'
    pb <- wnPartPOS wne' before'
    let n = if strict then name' else toUpperWord $ map toLower name'
    let msg w' = evalStateT (hPutStrLnLock stdout ("> inserted new name: " ++ w')) lock
    if isJust w then let w' = fromJust w in
      return $ Map.insert name' w'{count=c, before=nb before' pb, after=na after' pa,
                                   topic=sort $ nub (topic' : wordGetTopic w')} dict'
      else msg n >> return (Map.insert n (Name n 1 (e (nn before' pb))
                      (e (nn after' pa)) [] [topic']) dict')
    where
      e [] = Map.empty
      e x = Map.singleton x 1
      c = incCount' (fromJust w) 1
      na x y = if isJust $ Map.lookup x dict' then incAfter' (fromJust w) x 1 else
                 if y /= UnknownEPos || Aspell.check aspell' (ByteString.pack x) then
                   incAfter' (fromJust w) x 1
                 else wordGetAfter (fromJust w)
      nb x y = if isJust $ Map.lookup x dict' then incBefore' (fromJust w) x 1 else
                 if y /= UnknownEPos || Aspell.check aspell' (ByteString.pack x) then
                   incBefore' (fromJust w) x 1
                 else wordGetBefore (fromJust w)
      nn x y  = if isJust $ Map.lookup x dict' then x else
                  if y == UnknownEPos && Aspell.check aspell' (ByteString.pack x) == False
                  then [] else x

insertAcroRaw :: MVar () -> Fugly -> String -> String -> String -> String
                 -> String -> IO Dict
insertAcroRaw lock f@Fugly{dict=dict'} w b a t def =
    insertAcroRaw' lock f (Map.lookup w dict') w b a t def

insertAcroRaw' :: MVar () -> Fugly -> Maybe Word -> String -> String
                  -> String -> String -> String -> IO Dict
insertAcroRaw' _ Fugly{dict=dict'} _ [] _ _ _ _ = return dict'
insertAcroRaw' lock Fugly{dict=dict', wne=wne', aspell=aspell'}
    w acro' before' after' topic' def = do
    pa <- wnPartPOS wne' after'
    pb <- wnPartPOS wne' before'
    let msg w' = evalStateT (hPutStrLnLock stdout ("> inserted new acronym: " ++ w')) lock
    if isJust w then let w' = fromJust w in
      return $ Map.insert acro' w'{count=c, before=nb before' pb,
        after=na after' pa, topic=sort $ nub (topic' : wordGetTopic w')} dict'
      else msg acro' >> return (Map.insert acro' (Acronym acro' 1 (e (nn before' pb))
                          (e (nn after' pa)) [] [topic'] def) dict')
    where
      e [] = Map.empty
      e x = Map.singleton x 1
      c = incCount' (fromJust w) 1
      na x y = if isJust $ Map.lookup x dict' then incAfter' (fromJust w) x 1 else
                 if y /= UnknownEPos || Aspell.check aspell' (ByteString.pack x) then
                   incAfter' (fromJust w) x 1
                 else wordGetAfter (fromJust w)
      nb x y = if isJust $ Map.lookup x dict' then incBefore' (fromJust w) x 1 else
                 if y /= UnknownEPos || Aspell.check aspell' (ByteString.pack x) then
                   incBefore' (fromJust w) x 1
                 else wordGetBefore (fromJust w)
      nn x y  = if isJust $ Map.lookup x dict' then x else
                  if y == UnknownEPos && Aspell.check aspell' (ByteString.pack x) == False
                  then [] else x

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
      del' w = w{topic=sort $ nub ("stuff" : (delete t $ wordGetTopic w))}

dropTopicWords :: Dict -> String -> Dict
dropTopicWords m t = del' (filter (\w -> [t] == (delete "stuff" $ wordGetTopic w)) $ Map.elems m) m
    where
      del' []     m' = m'
      del' (x:xs) m' = del' xs (dropWord m' $ wordGetWord x)

ageWord :: Dict -> String -> Int -> Dict
ageWord m word' num = Map.map age m
    where
      age w = let
        w' = wordGetWord  w
        c  = wordGetCount w in
        if w' == word' then
           w{count=if c - num < 1 then 1 else
           c - num, before=incBefore' w word' (-num), after=incAfter' w word' (-num)}
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
numWords m typ top = length $ filter (\x -> wordIs x == typ &&
                       (elem top (wordGetTopic x) || null top)) $ Map.elems m

listNeigh :: Map.Map String Int -> [String]
listNeigh m = [w | (w, _) <- Map.toList m]

listNeighMax :: Map.Map String Int -> [String]
listNeighMax m = [w | (w, c) <- Map.toList m, c == maximum
                   [c' | (_, c') <- Map.toList m]]

listNeighLeast :: Map.Map String Int -> [String]
listNeighLeast m = [w | (w, c) <- Map.toList m, c == minimum
                     [c' | (_, c') <- Map.toList m]]

listNeighTopic :: Dict -> String -> [String] -> [String]
listNeighTopic d t n = map wordGetWord $ filter (\x -> elem t $ wordGetTopic x)
                       $ map (\w -> fromMaybe emptyWord $ Map.lookup w d) n

listNeighShow :: Map.Map String Int -> [String]
listNeighShow m = concat [[w, show c] | (w, c) <- Map.toList m]

listWords :: Dict -> [String]
listWords m = map wordGetWord $ Map.elems m

listWordsCountSort :: Dict -> Int -> String -> String -> [String]
listWordsCountSort m num typ top = concat [[w, show c, ";"] | (c, w) <- take num $ reverse $
                                   sort $ map wordGetwc $ filter (\x -> wordIs x == typ &&
                                   (elem top (wordGetTopic x) || null top)) $ Map.elems m]

listWordFull :: Dict -> String -> String
listWordFull m word' =
    if isJust ww then unwords $ f (fromJust ww) else "Nothing!"
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
dePlenk [] = []
dePlenk s  = dePlenk' s []
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
    | x == head xs = x : dedup (dropWhile (\y -> y == x) xs)
    | otherwise    = x : dedup xs

dedupD :: [String] -> [String]
dedupD m = let n  = dedup m
               f1 = filter (not . null) . concat . map (\i -> [fst i,snd i]) . dedup
               x1 = f1 $ fz [] n
               x2 = f1 $ fz [] ("" : n) in
           if length x1 <= length x2 then x1 else x2
  where
    fz a []       = a
    fz a [x]      = a ++ [(x, [])]
    fz a (x:y:xs) = fz (a ++ [(x, y)]) xs

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
endSentence [] = []
endSentence msg
    | l == '?' || l == '!' = msg
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
wnRelated _    []    _    _  = return []
wnRelated wne' word' []   _  = wnRelated wne' word' "Hypernym" []
wnRelated wne' word' form [] = do
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
    s  <- runs wne' $ search (fixUnderscore word') (fromEPOS pos') AllSenses
    r  <- runs wne' $ relatedByList wnForm s
    ra <- runs wne' $ relatedByListAllForms s
    let result = if (map toLower form) == "all" then
                   concat $ map (fromMaybe [[]]) (runs wne' ra)
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
      return $ unwords $ map (\x -> if isNothing x then return '?' else
        (replace '_' ' ' $ unwords $ map (++ "\"") $ map ('"' :) $
        nub $ concat $ map (getWords . getSynset) (flatten (fromJust x)))) result

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
      if null a then sentenceMeet' s' xs else
        if fHead [] a == s' && length x > 2 then return (True, x)
        else sentenceMeet' s' xs
    meet' c d = do
      s1 <- runs w $ search c Noun 1
      s2 <- runs w $ search d Noun 1
      if null s1 || null s2 then return []
        else do
          m' <- runs w $ meet emptyQueue (head s1) (head s2)
          if isNothing m' then return [] else
            return $ getWords $ getSynset $ fromJust m'

asIsName :: MVar () -> Aspell.SpellChecker -> String -> IO Bool
asIsName _ _ []  = return False
asIsName _ _ "i" = return True
asIsName _ _ "I" = return True
asIsName lock aspell' word' = do
    let n = ["Can", "Fool", "He", "Male"]
    let l = map toLower word'
    let u = toUpperWord l
    let b = toUpperLast l
    nl <- evalStateT (asSuggest aspell' l) lock
    nb <- evalStateT (asSuggest aspell' b) lock
    return $ if length word' < 3 || elem word' n then False else
               if (length $ words nb) < 3 then False else
                 if word' == (words nb)!!1 then False else
                   if (word' == (words nb)!!0 || u == (words nb)!!0) && (not $ null nl) then True
                   else False

isName :: MVar () -> Aspell.SpellChecker -> Dict -> String -> IO Bool
isName _ _ _ [] = return False
isName lock aspell' dict' w = do
      n <- asIsName lock aspell' w
      let ww = Map.lookup w dict'
      return $ if isJust ww then wordIs (fromJust ww) == "name" else n

asIsAcronym :: MVar () -> Aspell.SpellChecker -> String -> IO Bool
asIsAcronym _ _ []     = return False
asIsAcronym _ _ (_:[]) = return False
asIsAcronym lock aspell' word' = do
    let n = ["now", "us", "who"]
    let a = [toLower $ head word'] ++ (tail $ map toUpper word')
    let u = map toUpper word'
    let l = map toLower word'
    na <- evalStateT (asSuggest aspell' a) lock
    return $ if (elem l n || elem l sWords) && word' /= u then False else
               if u == word' && null na then True else
                 if (length $ words na) > 2 && word' == (words na)!!1 then False else
                   if (not $ null na) && u == (words na)!!0 then True
                   else False

isAcronym :: MVar () -> Aspell.SpellChecker -> Dict -> String -> IO Bool
isAcronym _ _ _ [] = return False
isAcronym lock aspell' dict' w  = do
      a <- asIsAcronym lock aspell' w
      let ww = Map.lookup w dict'
      return $ if isJust ww then wordIs (fromJust ww) == "acronym" else a

dictLookup :: MVar () -> Fugly -> String -> String -> IO String
dictLookup _ _ [] _ = return []
dictLookup lock Fugly{wne=wne', aspell=aspell'} word' pos' = do
    gloss <- wnGloss wne' word' pos'
    if gloss == "Nothing!" then
       do a <- evalStateT (asSuggest aspell' word') lock
          return (gloss ++ " Perhaps you meant: " ++
                  (unwords (filter (\x -> x /= word') (words a))))
      else return gloss

asSuggest :: Aspell.SpellChecker -> String -> StateT (MVar ()) IO String
asSuggest _ [] = return []
asSuggest aspell' word' = do
    l <- get :: StateT (MVar ()) IO (MVar ())
    lock <- lift $ takeMVar l
    w <- lift $ Aspell.suggest aspell' (ByteString.pack word')
    let ww = map ByteString.unpack w
    lift $ if null ww then putMVar l lock >> return [] else
             if word' == head ww then putMVar l lock >> return []
             else putMVar l lock >> return (unwords ww)

gfLin :: Maybe PGF -> String -> String
gfLin _ [] = []
gfLin pgf' msg
    | isJust pgf' && isJust expr = linearize (fromJust pgf') (head $ languages $ fromJust pgf') (fromJust expr)
    | otherwise = []
  where
    expr = readExpr msg

gfParseBool :: Maybe PGF -> Int -> String -> Bool
gfParseBool _ _ [] = False
gfParseBool pgf' len msg
    | elem (map toLower lw) badEndWords = False
    | elem '\'' (map toLower lw)        = False
    | not $ isJust pgf'                 = True
    | len == 0       = True
    | length w > len = (gfParseBool' ipgf $ take len w) && (gfParseBool pgf' len (unwords $ drop len w))
    | otherwise      = gfParseBool' ipgf w
  where
    ipgf = fromJust pgf'
    w    = words msg
    lw   = strip '?' $ strip '.' $ last w

gfParseBool' :: PGF -> [String] -> Bool
gfParseBool' _ [] = False
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
      gfRandom pgf' $ if rr == (\\) rr (words r) then r else []
      else return []
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

insertPunc :: WordNetEnv -> Int -> [String] -> IO [String]
insertPunc wne' i w = do
    r  <- Random.getStdRandom (Random.randomR (0, 9)) :: IO Int
    let x  = fHead [] w
        xs = fTail [] w
        y  = fHead [] xs
        l  = length xs
        bad = ["a", "am", "an", "and", "as", "but", "by", "for", "from", "had", "has",
               "have", "I", "in", "is", "of", "on", "or", "that", "the", "this", "to",
               "very", "was", "with"]
        match' = ["a", "also", "but", "however", "our", "the", "then", "though", "will"]
    px <- wnPartPOS wne' x
    py <- wnPartPOS wne' y
    out r l x xs y px py bad match'
  where
    out r l x xs y px py bad match'
      | l < 1 = return w
      | elem x bad || elem '\'' x || i < 1 || l < 4 = do
          xs' <- insertPunc wne' (i + 1) xs
          return (x : xs')
      | (elem y match') && r < 7 = do
          xs' <- insertPunc wne' 0 xs
          let o' = if r > 4 then (x ++ ".  ") : (toUpperSentence xs') else
                     (x ++ ",") : xs'
          return o'
      | y == "is" && r > 3 && l > 2 && i > 2 = do
          xs' <- insertPunc wne' 0 xs
          return ((x ++ ", this") : xs')
      | px == POS Noun && (py == POS Noun || py == POS Adj)
        && r < 8 && y /= "or" && y /= "and" = do
          xs' <- insertPunc wne' 0 xs
          return ((x ++ if r < 2 then ", or" else ", and") : xs')
      | px == POS Adj && py == POS Adj && r < 7 = do
          xs' <- insertPunc wne' 0 xs
          return ((x ++ " and") : xs')
      | otherwise = do
          xs' <- insertPunc wne' (i + 1) xs
          return (x : xs')

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
    let bad = words "the thing this"
        ww' = sortOn length $ filter (\x -> length x > 2 && (notElem x $ bad ++ qWords)) ww
        n   = case mod r 9 of
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
    let n' = fHead n $ reverse ww'
    return $ if last n' == 's' then init n' else if length n' < 3 then n else n'

nouns :: String -> String
nouns []      = "stuff"
nouns "guy"   = "guys"
nouns "okay"  = "okays"
nouns "stuff" = "stuff"
nouns n
    | last n == 'y' = (init n) ++ "ies"
    | last n == 'h' = n ++ "es"
    | otherwise     = n ++ "s"

wnReplaceWords :: Fugly -> Bool -> Int -> [String] -> IO [String]
wnReplaceWords _ _     _ []  = return []
wnReplaceWords _ False _ msg = return msg
wnReplaceWords fugly@Fugly{wne=wne', ban=ban'} True  randoms msg = do
    cr <- Random.getStdRandom (Random.randomR (0, (length msg) - 1))
    rr <- Random.getStdRandom (Random.randomR (0, 99))
    let w' = msg!!cr
    w  <- findRelated wne' w'
    let ww = if elem (map toLower w) sWords || elem (map toLower w) ban' then w' else w
    if randoms == 0 then return msg else
      if randoms == 100 then mapM (\x -> findRelated wne' x) msg else
        if rr + 20 < randoms then
          wnReplaceWords fugly True randoms $ filter (not . null)
          ((takeWhile (/= w') msg) ++ [ww] ++ (tail $ dropWhile (/= w') msg))
        else return msg

asReplaceWords :: MVar () -> Fugly -> [String] -> IO [String]
asReplaceWords _ _ [] = return [[]]
asReplaceWords lock fugly msg = mapM (\x -> asReplace lock fugly x) msg

asReplace :: MVar () -> Fugly -> String -> IO String
asReplace _ _ [] = return []
asReplace lock Fugly{dict=dict', wne=wne', aspell=aspell'} word' = do
    n  <- asIsName lock aspell' word'
    ac <- asIsAcronym lock aspell' word'
    if (elem ' ' word') || (elem '\'' word') || word' == (toUpperWord word') || n || ac then return word'
      else do
      a <- evalStateT (asSuggest aspell' word') lock
      p <- wnPartPOS wne' word'
      let w  = Map.lookup word' dict'
      let rw = filter (\x -> (notElem '\'' x) && levenshteinDistance defaultEditCosts word' x < 4) $ words a
      rr <- Random.getStdRandom (Random.randomR (0, (length rw) - 1))
      if null rw || p /= UnknownEPos || isJust w then return word' else
        if head rw == word' then return word' else return $ map toLower (rw!!rr)

rhymesWith :: MVar () -> Aspell.SpellChecker -> String -> IO [String]
rhymesWith _ _ [] = return []
rhymesWith lock aspell' word' = do
    let l = map toLower word'
    let b = toUpperLast l
    as <- evalStateT (asSuggest aspell' b) lock
    let asw = words as
    let end = case length l of
          1 -> l
          2 -> l
          3 -> drop 1 l
          4 -> drop 2 l
          x -> drop (x-3) l
    let out = filter (\x -> (notElem '\'' x) && length x > 2 && length l > 2 &&
                x Regex.=~ (end ++ "$") && levenshteinDistance defaultEditCosts l x < 4) asw
    return $ if null out then [word'] else out

findNextWord :: Fugly -> Int -> Int -> Int -> Bool -> Bool -> String
                -> String -> String -> [String]
findNextWord _ _ _ _ _ _ _ _ [] = []
findNextWord Fugly{dict=dict'} r i rand prev stopic top noun w =
    let ln = if isJust ww then length neigh else 0
        lm = if isJust ww then length neighmax else 0
        ll = if isJust ww then length neighleast else 0
        i' = i + length w
        nr = mod (i' + r + 1) ln
        mr = mod (i' + r + 7) lm
        lr = mod (i' + r + 5) ll
        rr = mod (i' + r) 99
        f1 = if isJust ww && ll > 0 then neighleast!!lr else []
        f2 = if isJust ww then case mod i 3 of
          0 -> if ln > 0 then neigh!!nr else []
          1 -> if ll > 0 then neighleast!!lr else []
          2 -> if ll > 0 then neighleast!!lr else []
          _ -> []
             else []
        f3 = if isJust ww then case mod i 5 of
          0 -> if ll > 0 then neighleast!!lr else []
          1 -> if ln > 0 then neigh!!nr else []
          2 -> if lm > 0 then neighmax!!mr else []
          3 -> if ln > 0 then neigh!!nr else []
          4 -> if ln > 0 then neigh!!nr else []
          _ -> []
             else []
        f4 = if isJust ww then case mod i 3 of
          0 -> if lm > 0 then neighmax!!mr else []
          1 -> if ln > 0 then neigh!!nr else []
          2 -> if lm > 0 then neighmax!!mr else []
          _ -> []
             else []
        f5 = if isJust ww && lm > 0 then neighmax!!mr else [] in
    if rand > 89 then words f1 else
      if rr < rand - 25 then words f2 else
        if rr < rand + 35 then words f3 else
          if rr < rand + 65 then words f4 else
            words f5
    where
      ww       = Map.lookup w dict'
      wordGet' = if prev then wordGetBefore else wordGetAfter
      neigh_all'       = listNeigh $ wordGet' (fromJust ww)
      neigh_all        = if elem noun neigh_all' then [noun] else neigh_all'
      neigh_topic'     = listNeighTopic dict' top neigh_all'
      neigh_topic      = if elem noun neigh_topic' then [noun] else neigh_topic'
      neighmax_all'    = listNeighMax $ wordGet' (fromJust ww)
      neighmax_all     = if elem noun neighmax_all' then [noun] else neighmax_all'
      neighmax_topic'  = listNeighTopic dict' top neighmax_all'
      neighmax_topic   = if elem noun neighmax_topic' then [noun] else neighmax_topic'
      neighleast_all   = listNeighLeast $ wordGet' (fromJust ww)
      neighleast_topic = listNeighTopic dict' top neighleast_all
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
              let hyper' = filter (\x -> (notElem ' ' x) && length x > 2) $ map (strip '"') hyper
                  hypo'  = filter (\x -> (notElem ' ' x) && length x > 2) $ map (strip '"') hypo
                  anto'  = filter (\x -> (notElem ' ' x) && length x > 2) $ map (strip '"') anto
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
