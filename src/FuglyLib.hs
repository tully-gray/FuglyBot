module FuglyLib
       (
         initFugly,
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
import Data.Char (isAlpha)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.IO (FilePath)
import System.IO.Unsafe

import NLP.WordNet
import NLP.WordNet.Prims (indexLookup, senseCount, getSynset, getWords, getGloss)
import NLP.WordNet.PrimTypes

type Fugly = (Map.Map String Dict, WordNetEnv)

--data PoS = Noun | Verb | Adjective | Adverb | Pronoun | Conjuction | Unknown
--         deriving (Eq, Ord, Show)

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

initFugly :: FilePath -> IO Fugly
initFugly wndir = do
    wne <- NLP.WordNet.initializeWordNetWithOptions
           (return wndir :: Maybe FilePath)
           (Just (\e f -> putStrLn (e ++ show (f :: SomeException))))
    return (Map.empty :: Map.Map String Dict, wne)
    --h <- openFile (fuglydir ++ "/words.txt") ReadMode

-- initFugly :: FilePath -> IO (Map.Map String Dict)
-- initFugly wndir = do
--     f <- initFugly' wndir
--     return $ fst f

insertWords :: Fugly -> [String] -> Map.Map String Dict
insertWords f@(dict, wne) [] = dict
insertWords f [x] = insertWord f x [] [] []
insertWords f@(dict ,wne) msg@(x:y:xs) =
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

-- insertWord :: Fugly -> String -> String -> String -> String -> Map.Map String Dict
-- insertWord f@(dict, wne) [] _ _ _ = dict
-- insertWord f@(dict, wne) word before after pos =
--   if isJust w then
--     Map.insert ww (Word ww b a ((\x@(Word _ _ _ r _) -> r) (fromJust w)) pp) dict
--     else
--     Map.insert ww (Word ww (e bb) (e aa) [] pp) dict
--   where
--     -- e [] = Map.singleton [] (-1)
--     e [] = Map.empty
--     e x = Map.singleton x 1
--     aa = cleanString isAlpha after
--     bb = cleanString isAlpha before
--     ww = cleanString isAlpha word
--     w = Map.lookup ww dict
--     b = incBefore' (fromJust w) bb
--     a = incAfter' (fromJust w) aa
--     pp = if null pos then UnknownEPos else readEPOS pos

{-# NOINLINE insertWord #-}
insertWord :: Fugly -> String -> String -> String -> String -> Map.Map String Dict
insertWord f@(dict, wne) [] _ _ _ = dict
insertWord f@(dict, wne) word before after pos =
  if isJust w then
    Map.insert ww (Word ww b a ((\x@(Word _ _ _ r _) -> r) (fromJust w)) pp) dict
    else
    Map.insert ww (Word ww (e bb) (e aa) [] pp) dict
  where
    -- e [] = Map.singleton [] (-1)
    e [] = Map.empty
    e x = Map.singleton x 1
    aa = cleanString isAlpha after
    bb = cleanString isAlpha before
    ww = cleanString isAlpha word
    w = Map.lookup ww dict
    b = incBefore' (fromJust w) bb
    a = incAfter' (fromJust w) aa
    pp = unsafePerformIO (if null pos then wnPartPOS wne ww else return $ readEPOS pos)

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
listWordFull m word = if isJust ww then
                        unwords $ (\x@(Word w b a _ p) ->
                        ["word:", w, " before:", unwords $ listNeigh b,
                         " after:", unwords $ listNeigh a, " PoS:", (show p)]) (fromJust ww)
                      else
                        "Nothing!"
  where
    ww = Map.lookup word m

cleanString :: (Char -> Bool) -> String -> String
cleanString _ [] = []
cleanString f (x:xs)
        | not $ f x = cleanString f xs
        | otherwise = x : cleanString f xs

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
      | fromMaybe (-1) (elemIndex (maximum a) a) == 0 = POS Noun
      | fromMaybe (-1) (elemIndex (maximum a) a) == 1 = POS Verb
      | fromMaybe (-1) (elemIndex (maximum a) a) == 2 = POS Adj
      | fromMaybe (-1) (elemIndex (maximum a) a) == 3 = POS Adv
      | otherwise                                     = UnknownEPos
