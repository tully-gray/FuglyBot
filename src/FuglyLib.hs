module FuglyLib
       (
         initFugly,
         stopFugly,
         saveDict,
         insertWord,
         insertWords,
         listWords,
         listWordFull,
         cleanString,
         Dict,
         Fugly
       )
       where

import Control.Exception
import Data.Char (isAlpha, toLower)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.IO
import System.IO.Unsafe (unsafePerformIO)

import NLP.WordNet
import NLP.WordNet.Prims (indexLookup, senseCount, getSynset, getWords, getGloss)
import NLP.WordNet.PrimTypes

type Fugly = (Map.Map String Dict, WordNetEnv)

data Dict = Word {
              word    :: String,
              before  :: Map.Map String Int,
              after   :: Map.Map String Int,
              related :: [String],
              pos     :: EPOS
              } |
            Name {
              word    :: String,
              before  :: Map.Map String Int,
              after   :: Map.Map String Int,
              related :: [String]
              } |
            Place {
              word    :: String,
              before  :: Map.Map String Int,
              after   :: Map.Map String Int,
              related :: [String]
              } |
            Phrase {
              word    :: String,
              before  :: Map.Map String Int,
              after   :: Map.Map String Int,
              related :: [String]
              }

initFugly :: FilePath -> FilePath -> IO Fugly
initFugly fuglydir wndir = do
    wne <- NLP.WordNet.initializeWordNetWithOptions
           (return wndir :: Maybe FilePath)
           (Just (\e f -> putStrLn (e ++ show (f :: SomeException))))
    return (Map.empty :: Map.Map String Dict, wne)
    --h <- openFile (fuglydir ++ "/words.txt") ReadMode

stopFugly :: Fugly -> IO ()
stopFugly fugly@(dict, wne) = do
    saveDict dict "/home/lonewolf/fugly/dict.txt"
    closeWordNet wne

saveDict :: Map.Map String Dict -> FilePath -> IO ()
saveDict dict dictfile = do
    let d = Map.toList dict
    if null d then putStrLn "Empty dict!"
      else do
        h <- openFile dictfile WriteMode
        hSetBuffering h LineBuffering
        putStrLn "Saving dict file..."
        saveDict' h d
        hFlush h
        hClose h
  where
    saveDict' :: Handle -> [(String, Dict)] -> IO ()
    saveDict' _ [] = return ()
    saveDict' h (x:xs) = do
      hPutStr h (format' (snd x))
      saveDict' h xs
    format' m@(Word w b a r p) = unwords [("word: " ++ w ++ "\n"), ("before: " ++ (unwords $ listNeigh b) ++ "\n"), ("after: " ++ (unwords $ listNeigh a) ++ "\n"), ("related: " ++ (unwords r) ++ "\n"), ("pos: " ++ (show p) ++ "\n")]

insertWords :: Fugly -> [String] -> Map.Map String Dict
insertWords f@(dict, wne) [] = dict
insertWords f [x] = insertWord f x [] [] []
insertWords f@(dict, wne) msg@(x:y:xs) =
      case (len) of
        2 -> insertWord (insertWord f x [] y [], wne) y x [] []
        _ -> insertWords' f 0 len msg
  where
    len = length msg
    insertWords' f@(d,w) _ _ [] = d
    insertWords' f@(d,w) a l msg
      | a == 0     = insertWords' (insertWord f (msg!!a) []
                                   (msg!!(a+1)) [], wne) (a+1) l msg
      | a > l - 1  = d
      | a == l - 1 = insertWords' (insertWord f (msg!!a) (msg!!(a-1))
                                   [] [], wne) (a+1) l msg
      | otherwise  = insertWords' (insertWord f (msg!!a) (msg!!(a-1))
                                  (msg!!(a+1)) [], wne) (a+1) l msg

{-# NOINLINE insertWord #-}
insertWord :: Fugly -> String -> String -> String -> String -> Map.Map String Dict
insertWord f@(dict, wne) [] _ _ _ = dict
insertWord f@(dict, wne) word before after pos =
  if pp == UnknownEPos then
           dict
    else if isJust w then
           Map.insert ww (Word ww b a ((\x@(Word _ _ _ r _) -> r) (fromJust w)) pp) dict
         else
           Map.insert ww (Word ww (e bbb) (e aaa) rel pp) dict
  where
    w = Map.lookup ww dict
    e [] = Map.empty
    e x = Map.singleton x 1
    aa = cleanString isAlpha after
    bb = cleanString isAlpha before
    ww = cleanString isAlpha word
    a = incAfter' (fromJust w) aa
    b = incBefore' (fromJust w) bb
    aaa = if pa == UnknownEPos then [] else aa
    bbb = if pb == UnknownEPos then [] else bb
    pp = unsafePerformIO (if null pos then wnPartPOS wne ww else return $ readEPOS pos)
    pa = unsafePerformIO (if null pos then wnPartPOS wne aa else return $ readEPOS pos)
    pb = unsafePerformIO (if null pos then wnPartPOS wne bb else return $ readEPOS pos)
    rel = unsafePerformIO (wnRelated wne ww "Hypernym" pp)

incBefore' :: Dict -> String -> Map.Map String Int
incBefore' word@(Word _ b _ _ _) []     = b
incBefore' word@(Word _ b _ _ _) before =
  if isJust w then
    Map.insert before ((fromJust w) + 1) b
    else
    Map.insert before 1 b
  where
    w = Map.lookup before b

incBefore :: Map.Map String Dict -> String -> String -> Map.Map String Int
incBefore m word before = do
  let w = Map.lookup word m
  if isJust w then incBefore' (fromJust w) before
    else Map.empty

incAfter' :: Dict -> String -> Map.Map String Int
incAfter' word@(Word _ _ a _ _) []     = a
incAfter' word@(Word _ _ a _ _) after  =
  if isJust w then
    Map.insert after ((fromJust w) + 1) a
    else
    Map.insert after 1 a
  where
    w = Map.lookup after a

incAfter :: Map.Map String Dict -> String -> String -> Map.Map String Int
incAfter m word after = do
  let w = Map.lookup word m
  if isJust w then incAfter' (fromJust w) after
    else Map.empty

listNeigh :: Map.Map String Int -> [String]
listNeigh m = concat [[w, show c] | (w, c) <- Map.toList m]

listWords :: Map.Map String Dict -> [String]
listWords m = map (\x@(Word w _ _ _ _) -> w) $ Map.elems m

listWordFull :: Map.Map String Dict -> String -> String
listWordFull m word =
  if isJust ww then
    unwords $ (\x@(Word w b a r p) ->
                ["word:", w, " before:", unwords $ listNeigh b,
                 " after:", unwords $ listNeigh a, " PoS:", (show p),
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
    if (null result) || (null $ concat result) then return ["Nothing!"] else
      return $ map (\x -> replace '_' ' ' $ unwords $ map (++ "\"") $
                    map ('"' :) $ concat $ map (getWords . getSynset) x) result
