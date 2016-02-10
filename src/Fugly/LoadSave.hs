module Fugly.LoadSave where

import           AI.NeuralNetworks.Simple
import           Control.Concurrent             (MVar)
import           Control.Exception
import           Control.Monad.Trans.State.Lazy (evalStateT)
import qualified Data.ByteString.Char8          as ByteString (pack)
import           Data.Either
import           Data.List                      (elemIndex, nub, sort)
import qualified Data.Map.Lazy                  as Map
import           Data.Maybe
import           Fugly.Neural
import           Fugly.Types
import           FuglyLib
import qualified Language.Aspell                as Aspell
import qualified Language.Aspell.Options        as Aspell.Options
import           NLP.WordNet                    hiding (Word)
import           NLP.WordNet.PrimTypes          hiding (pos)
import           PGF                            (readPGF)
import           Prelude                        hiding (Word)
import           System.IO
import qualified System.Random                  as Random

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
    (nset', nmap') <- catch (loadNeural fuglydir $ read (params'!!4))
                      (\e -> do let err = show (e :: SomeException)
                                hPutStrLn stderr ("Exception in initFugly: " ++ err)
                                return ([], Map.empty))
    g <- Random.newStdGen
    let newnet = randNN g 0.7
    if null $ checkNSet nset' then
      return ((Fugly dict' defs' pgf' wne' aspell' ban' match' newnet nset' nmap'), params')
      else do
        nnet' <- backpropagationBatchParallel newnet nset' 0.02 nnStop :: IO (NeuralNetwork Float)
        return ((Fugly dict' defs' pgf' wne' aspell' ban' match' nnet' nset' nmap'), params')

stopFugly :: MVar () -> FilePath -> Fugly -> String -> [String] -> IO ()
stopFugly st fuglydir fugly@Fugly{wne=wne'} dfile params = do
    catch (do
             saveDict st fugly fuglydir dfile params
             saveNeural st fugly fuglydir)
      (\e -> do let err = show (e :: SomeException)
                evalStateT (hPutStrLnLock stderr ("Exception in stopFugly: " ++ err)) st
                return ())
    closeWordNet wne'

saveDict :: MVar () -> Fugly -> FilePath -> String
            -> [String] -> IO ()
saveDict st Fugly{dict=dict', defs=defs', ban=ban', match=match'}
  fuglydir dfile params = do
    let d = Map.toList dict'
    h <- openFile (fuglydir ++ "/" ++ dfile ++ "-dict.txt") WriteMode
    hSetBuffering h LineBuffering
    evalStateT (hPutStrLnLock stdout "Saving dict file...") st
    if null d then return () else saveDict' h d
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
                           "pos:"        -> word'{pos=readEPOS $ unwords ll}
                           "definition:" -> word'{definition=unwords ll}
                           "end:"        -> word'
                           _             -> word'
               else word'
      if l4 == False then do return nm
        else if head wl == "end:" then
               ff h ww (nm ++ ((wordGetWord ww), ww) : [])
             else if head wl == ">END<" then
                    return nm
                  else
                    ff h ww nm

saveNeural :: MVar () -> Fugly -> FilePath -> IO ()
saveNeural st Fugly{nset=nset', nmap=nmap'} fuglydir = do
    h <- openFile (fuglydir ++ "/neural.txt") WriteMode
    hSetBuffering h LineBuffering
    evalStateT (hPutStrLnLock stdout "Saving neural file...") st
    _ <- evalStateT (mapM (\(i, o) -> hPutStrLnLock h ((unwords $ map show i) ++
           " / " ++ (unwords $ map show o))) $ checkNSet $ nub nset') st
    evalStateT (hPutStrLnLock h ">END<") st
    _ <- evalStateT (mapM (\x -> hPutStrLnLock h x)
                     [unwords [show n, s] | (n, s) <- Map.toList nmap']) st
    evalStateT (hPutStrLnLock h ">END<") st
    hClose h

loadNeural :: FilePath -> Int -> IO (NSet, NMap)
loadNeural fuglydir nsets = do
    h <- openFile (fuglydir ++ "/neural.txt") ReadMode
    eof <- hIsEOF h
    if eof then
      return ([], Map.empty)
      else do
      hSetBuffering h LineBuffering
      hPutStrLn stdout "Loading neural file..."
      nset' <- fn h [([], [])]
      nmap' <- fm h Map.empty
      return (nset', nmap')
  where
    fn :: Handle -> NSet -> IO NSet
    fn h a = do
      l <- hGetLine h
      if l == ">END<" then return $ take nsets $ checkNSet a else
        let j = elemIndex '/' l in
          if isJust j then let
            (i, o) = splitAt (fromJust j) l
            i' = map (clamp . read) $ words $ init i
            o' = map (clamp . read) $ words $ drop 2 o in
            fn h ((i', o') : a)
          else fn h $ checkNSet a
    fm :: Handle -> NMap -> IO NMap
    fm h a = do
      l <- hGetLine h
      if l == ">END<" then return a else
        let ll = words l
            n  = read $ head ll :: Int
            s  = unwords $ tail ll in
        fm h $ Map.insert n s a
