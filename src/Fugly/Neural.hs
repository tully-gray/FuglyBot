module Fugly.Neural where

import           AI.NeuralNetworks.Simple
import           Control.Concurrent             (MVar)
import           Control.Monad.Trans.State.Lazy (evalStateT)
import qualified Data.ByteString.Char8          as ByteString (pack)
import           Data.Char                      (toLower, toUpper)
import           Data.List                      (nub)
import qualified Data.Map                       as Map (insert, lookup)
import           Data.Maybe
import           Fugly.Types
import           FuglyLib
import qualified Language.Aspell                as Aspell (check)
import           NLP.WordNet.PrimTypes
import           System.IO                      (stdout)
import qualified System.Random                  as Random

floatToWord :: NMap -> Bool -> Int -> Int -> Float -> String
floatToWord nmap' b j k d = let i = (j * u) + (floor $ d * msize)
                                s = not b
                                t = if b then 1 else 0
                                u = if b then 1 else (-1)
                                r = Map.lookup i nmap' in
    if isJust r then fromJust r else if j < k then
      floatToWord nmap' s (j + t) k d else []

nnInsert :: Fugly -> Int -> [String] -> [String] -> IO (NeuralNetwork Float, NSet, NMap)
nnInsert Fugly{nnet=nnet', nset=nset', nmap=nmap'} _ [] _ = return (nnet', nset', nmap')
nnInsert Fugly{nnet=nnet', nset=nset', nmap=nmap'} _ _ [] = return (nnet', nset', nmap')
nnInsert Fugly{wne=wne', aspell=aspell', ban=ban', nnet=nnet', nset=nset', nmap=nmap'} nsets input output = do
  i' <- fix (low input) []
  o' <- fix (low output) []
  let new = (dList i', dList o')
      ok  = checkNSet [new]
  if null ok then
    return (nnet', nset', nmap') else do
      g <- Random.newStdGen
      let nr = randNN g 0.5
          nn = if nnTest nnet' > 0.7 then nr else nnet'
          ns = take nsets $ nub $ new : nset'
          nm = foldr (\x -> Map.insert (mKey x) x) nmap' $ i' ++ o'
      newN <- backpropagationBatchParallel nn ns 0.007 nnStop :: IO (NeuralNetwork Float)
      return (newN, ns, nm)
  where
    dList x = take (fromIntegral nsize :: Int) $ map wordToFloat $ x ++ nnPad
    fix :: [String] -> [String] -> IO [String]
    fix []     a = return $ dedupD $ filter (not . null) $ reverse a
    fix (x:xs) a = do
      p <- wnPartPOS wne' x
      if (length x < 3) && (notElem (map toLower x) sWords) then fix xs a
        else if elem (map toLower x) ban' then fix xs a
             else if p /= UnknownEPos || Aspell.check aspell' (ByteString.pack x) then fix xs (x : a)
                  else fix xs a
    low [] = []
    low a  = map (map toLower) a
    mKey w = floor $ (wordToFloat w) * msize

nnReply :: MVar () -> Fugly -> Int -> Bool -> [String] -> IO String
nnReply _  Fugly{nset=[]} _ _ _  = return []
nnReply _  _              _ _ [] = return []
nnReply st Fugly{dict=dict', pgf=pgf', wne=wne', aspell=aspell',
                 ban=ban', nnet=nnet', nmap=nmap'} plen debug msg = do
    a <- nnReply'
    c <- acroName [] $ check a
    if null $ concat c then return [] else do
      out <- insertCommas wne' 0 $ toUpperSentence $ endSentence $ dedupD c
      if debug then
        evalStateT (hPutStrLnLock stdout ("> debug: nnReply: " ++ unwords out)) st
        else return ()
      return $ unwords out
  where
    sw = 1 -- search width
    nnReply' :: IO [String]
    nnReply' = do
      let out = runNeuralNetwork nnet' $ map wordToFloat $ take (fromIntegral nsize :: Int) msg
      if debug then
        evalStateT (hPutStrLnLock stdout ("> debug: nnReply: " ++ unwords (map show out))) st
        else return ()
      return $ map (\x -> floatToWord nmap' True 0 sw x) out
    check :: [String] -> IO [String]
    check [] = return []
    check x = do
      p <- wnPartPOS wne' $ cleanString $ last x
      let x' = filter (\y -> (not $ null y) && notElem y ban') $ replace "i" "I" x
      if gfParseBool pgf' plen (unwords x') && p /= POS Adj && bannedS x' then return x'
        else return []
    banned :: String -> String -> Bool
    banned w a = let w' = Map.lookup w dict' in
      if isJust w' then
         elem a (wordGetBanAfter $ fromJust w')
         else False
    bannedS :: [String] -> Bool
    bannedS []     = False
    bannedS [_]    = True
    bannedS (x:xs) = if banned x $ head xs then False
                     else bannedS xs
    acroName :: [String] -> IO [String] -> IO [String]
    acroName o w = do
      m <- w
      if null m then return $ reverse o else do
        let x  = head m
            xs = fTail [] m
        ac <- isAcronym st aspell' dict' x
        n  <- isName st aspell' dict' x
        let w' = if ac then map toUpper x else if n then toUpperWord x else map toLower x
        acroName (w' : o) $ return xs
