module Fugly.Reply (
    defsReplace,
    replyDefault,
    replyResponse,
    replyRegex,
    replyMixed,
    replyRandom
    ) where

import           Control.Concurrent             (MVar)
import           Control.Monad.Trans.State.Lazy (evalStateT)
import           Data.Char                      (isDigit, toLower, toUpper)
import           Data.Map.Strict                as Map (lookup)
import           Data.Maybe
import           Data.List                      (intercalate)
import           Fugly.Parameter
import           Fugly.Types                    hiding (fugly, topic)
import           FuglyLib
import           NLP.WordNet.PrimTypes
import           System.IO                      (stdout)
import qualified System.Random                  as Random
import           Text.EditDistance
import qualified Text.Regex.Posix               as Regex

replyResponse :: MVar () -> Fugly -> Parameter -> Int -> Float
                 -> String -> String -> IO String
replyResponse _ _ _ _ _ _ [] = return []
replyResponse lock fugly'@Fugly{defs=defs'}
    params'@Parameter{debug=debug'} r dist nick' msg = do
    if debug' then
      evalStateT (hPutStrLnLock stdout "> debug: replyResponse") lock
      else return ()
    let r' = [de | (t, de) <- defs', t == Response]
        l  = map splitDef r'
        md = dist + (realToFrac (length msg) / 4 :: Float)
        bl = bestLevenshtein l msg
        d  = if null bl then maxBound :: Int else (\((_, _), d') -> d') $ head bl
        o  = map (\((_, o'), _) -> o') bl
    if (realToFrac d :: Float) <= md then
      defsReplace lock fugly' params' True nick' $ o!!(mod r $ length o)
      else return []
replyResponse _ _ _ _ _ _ _ = return []

replyRegex :: MVar () -> Fugly -> Parameter -> Int -> String
              -> String -> IO String
replyRegex _ _ _ _ _ [] = return []
replyRegex lock fugly'@Fugly{defs=defs'}
    params'@Parameter{debug=debug'} r nick' msg = do
    if debug' then
      evalStateT (hPutStrLnLock stdout "> debug: replyRegex") lock
      else return ()
    let m = [de | (t, de) <- defs', t == Regex]
        l = map splitDef m
        o = [a | (q, a) <- l, msg Regex.=~ q]
    if not $ null o then
      defsReplace lock fugly' params' True nick' $ o!!(mod r $ length o)
      else return []
replyRegex _ _ _ _ _ _ = return []

replyMixed :: MVar () -> Fugly -> Parameter -> Int -> [String] -> IO String
replyMixed _ _ _ _ [] = return []
replyMixed lock fugly'@Fugly{dict=dict', wne=wne', match=match'}
    params'@Parameter{debug=debug', sTries=stries, topic=topic'}
    r msg = do
    if debug' then
      evalStateT (hPutStrLnLock stdout "> debug: replyMixed") lock
      else return ()
    rr <- Random.getStdRandom (Random.randomR (0, 99)) :: IO Int
    replyMixed' rr msg
  where
    l = length msg
    t = stries * 5
    repRand = replyRandom' lock fugly' params'{sLength=5} True True
    fixIt'  = fixIt lock debug'
    replyMixed' :: Int -> [String] -> IO String
    replyMixed' _  [] = return []
    replyMixed' r' w
      | mBool = do
        let s = repRand $ words mString
        m <- fixIt' s [] 1 0 0 t
        return $ unwords m
      | l > 3 && (map (\x -> map toLower x) $ take 3 w) == ["do", "you", w!!2 Regex.=~ "like|hate|love|have|want|need"] = do
        noun <- getNoun wne' r' w
        let nouns' = nouns noun
            s1b :: Int -> [String] -> String
            s1b rr ww = ww!!2 Regex.=~ "like|hate|love|have|want|need" ++ " " ++
              if rr < 20 then "it" else
                if rr < 40 then "that" else nouns' in
          return $ unwords $ toUpperSentence $ endSentence $ words
          (case mod r' 4 of
            0 -> "I don't " ++ s1b r' w
            1 -> "yeah, I " ++ s1b r' w
            2 -> "sometimes I " ++ s1b r' w
            3 -> "can you " ++ s1b r' w
            _ -> nouns' ++ " are " ++ if r' < 50 then "not" else
                   " something I " ++ w!!2 Regex.=~ "like|hate|love|have|want|need")
      | l > 3 && l < 15 = do
          let ra = mod r 3
              t' = case ra of
                0 -> "food"
                1 -> "animal"
                2 -> "tool"
                _ -> "abstraction"
          (a, b) <- evalStateT (sentenceMeet wne' t' w) lock
          if a then case ra of
            0 -> do
              let s = repRand [b, "is", "tasty", "and"]
              m <- fixIt' (return ("Oh yes, " ++ b ++ ".") : s) [] 2 0 0 t
              return $ unwords m
            1 -> do
              let s = repRand [b, "is"]
              m <- fixIt' s [] 1 0 0 t
              return $ unwords m
            2 -> do
              let s = repRand ["my"]
              m <- fixIt' s [] 1 0 0 t
              return $ unwords m
            _ -> do
              let s = repRand ["this", b, "is"]
              m <- fixIt' s [] 1 0 0 t
              return $ unwords m
            else return []
      | l < 6 && r + r' < 25 = case mod r' 3 of
         0 -> do
          let s = repRand [topic']
          m <- fixIt' (return ("Do you like " ++ topic' ++ "?") : s) [] 2 0 0 t
          return $ unwords m
         1 -> do
          let s = repRand $ words "perhaps you"
          m <- fixIt' s [] 2 0 0 t
          return $ unwords m
         2 -> do
          let s = repRand $ words "can you"
          m <- fixIt' s [] 1 0 0 t
          return $ unwords m
         _ -> do
          let s = repRand $ words "I think that"
          m <- fixIt' s [] 1 0 0 t
          return $ unwords m
      | l == 1 && r < 70 = do
          let s = repRand w
          m <- fixIt' s [] 1 0 0 t
          return $ unwords m
      | otherwise = do
        let ns = getNames dict' w
        evalStateT (hPutStrLnLock stdout ("> debug: replyMixed: names: " ++ unwords ns)) lock
        if null ns then do
          noun <- getNoun wne' r' w
          let s = repRand ["this", noun, "is"]
          m <- fixIt' s [] 1 0 0 t
          return $ unwords $ toUpperSentence $ endSentence m
          else do
          let n' = ns!!mod r (length ns)
              s' = case mod r 3 of
                     0 -> [n', "said", "that"]
                     1 -> ["yes", "but", n', "once", "said", "that"]
                     _ -> ["why", "did", n', "say", "that"]
              s  = repRand s'
          m <- fixIt' s [] 1 0 0 t
          return $ unwords $ toUpperSentence $ endSentence m
    mList   = map toLower (" " ++ intercalate " | " match' ++ " ")
    mMsg    = (" " ++ map toLower (unwords msg) ++ " ")
    mBool   = mMsg Regex.=~ mList :: Bool
    mString = mMsg Regex.=~ mList :: String
replyMixed _ _ _ _ _ = return []

replyRandom :: MVar () -> Fugly -> Parameter -> Int -> Int -> [String] -> IO String
replyRandom _ _ _ _ _ [] = return []
replyRandom lock fugly'
    params'@Parameter{debug=debug', sTries=stries, sLength=slen}
    r num msg = do
    if debug' then
      evalStateT (hPutStrLnLock stdout "> debug: replyRandom") lock
      else return ()
    let m = if length msg < 4 || mod (length $ concat msg) 3 == 0 then
                msg else [msg!!(mod r $ length msg)]
        s = replyRandom' lock fugly' params' False True m
    o <- fixIt lock debug' s [] num 0 0 (stries * slen)
    return $ unwords o
replyRandom _ _ _ _ _ _ = return []

replyRandom' :: MVar () -> Fugly -> Parameter -> Bool -> Bool -> [String] -> [IO String]
replyRandom' _ _ _ _ _ [] = [return []] :: [IO String]
replyRandom' lock fugly@Fugly{dict=dict', pgf=pgf', wne=wne', aspell=aspell'}
    Parameter{debug=debug', replaceWord=rw, strictTopic=st, randoms=randoms',
              sTries=stries, sLength=slen, pLength=plen, topic=topic'}
    first punc msg = do
    let s1h n a x = let out = if a then map toUpper x else
                                if n then x else map toLower x in
          if isJust $ Map.lookup out dict' then out else []
    let s1a x = do
          r <- Random.getStdRandom (Random.randomR (0, 999)) :: IO Int
          a <- isAcronym lock aspell' dict' x
          n <- isName lock aspell' dict' x
          b <- getNoun wne' r msg
          let z  = findNextWord fugly r 0 randoms' True st topic' b x
              zz = fHead [] z
              y  = findNextWord fugly r 1 randoms' True st topic' b zz
              yy = fHead [] y
              c  = if null zz && null yy then 2 else
                     if null zz || null yy then 3 else 4
              w  = s1b fugly r slen c b $ findNextWord fugly r 1 randoms' False
                   st topic' b x
              ww = s1b fugly r slen 0 b msg
              d  = if first then ww else [yy] ++ [zz] ++ [s1h n a x] ++ w
          o <- wnReplaceWords fugly rw randoms' $ filter (not . null) $ take (stries * slen) d
          return $ dedupD o
    let s1d x = do
          w <- x
          if null w then return []
            else if not punc then x
              else return (init w ++ (cleanString (fLast [] w) ++ if elem
                (map toLower $ head w) qWords then "?" else ".") : [])
    let s1e x = do
          w <- x
          n <- isName lock aspell' dict' $ fHead [] w
          if null w then return []
            else if not punc && not n then x
              else return ([s1c w] ++ tail w)
    let s1g = map (\x -> do y <- x ; z <- insertPunc wne' 0 y
                            return $ dePlenk $ unwords z)
              (map (s1e . s1d . s1a) (msg ++ sWords))
    s1f 0 s1t s1g
  where
    s1b :: Fugly -> Int -> Int -> Int -> String -> [String] -> [String]
    s1b f r n i noun w =
      if null $ concat w then return [] else
        if i >= n then dedupD w else
          let ww = findNextWord f r i randoms' False st topic' noun $ fLast [] w in
          s1b f (r + i) n (i + 1) noun (w ++ ww)
    s1c :: [String] -> String
    s1c [] = []
    s1c w  = [toUpper $ head $ head w] ++ (fTail [] $ head w)
    s1t :: Int -> IO String -> IO String
    s1t i x = do
      y    <- x
      pos' <- wnPartPOS wne' $ cleanString $ fLast [] $ words y
      let plen' = if ((realToFrac i) :: Float) >
                     (((realToFrac stries) :: Float) / 2) then 0 else plen
      if null y then return [] else
        if gfParseBoolS pgf' plen' y && length (words y) > 2 && pos' /= POS Adj then return y else
          if debug' then evalStateT (hPutStrLnLock stdout ("> debug: sentence try: " ++ y)) lock >> return []
          else return []
    s1f _ _ []     = []
    s1f i f (x:xs) = f i x : s1f (i + 1) f xs
replyRandom' _ _ _ _ _ _ = [return []] :: [IO String]

replyDefault :: MVar () -> Fugly -> Parameter -> Int -> String -> IO String
replyDefault lock fugly'@Fugly{defs=defs', pgf=pgf'}
    params'@Parameter{debug=debug'} r nick' = do
    if debug' then
      evalStateT (hPutStrLnLock stdout "> debug: replyDefault") lock
      else return ()
    let dList = [de | (t, de) <- defs', t == Default]
        len   = length dList
    if len == 0 then gfRandom pgf' []
      else defsReplace lock fugly' params'{replaceWord=False, strictTopic=False} True nick' $ dList!!mod r len
replyDefault _ _ _ _ _ = return []

bestLevenshtein :: [(String, String)] -> String -> [((String, String), Int)]
bestLevenshtein _   []    = [(([], []), maxBound :: Int)]
bestLevenshtein []  _     = [(([], []), maxBound :: Int)]
bestLevenshtein defs' msg = filter (\x -> snd x == min') dl
  where
    dists :: [((String, String), Int)] -> [(String, String)] -> [((String, String), Int)]
    dists o []     = o
    dists o (x:xs) = dists ((x, levenshteinDistance defaultEditCosts msg $ fst x) : o) xs
    dl   = dists [] defs'
    min' = minimum [d | (_, d) <- dl]

defsReplace :: MVar () -> Fugly -> Parameter -> Bool -> String -> String -> IO String
defsReplace _ _ _ _ _ [] = return []
defsReplace lock fugly'
    params'@Parameter{debug=debug', topic=topic'}
    format nick' msg = do
    let n = if null nick' then "somebody" else nick'
        t = if null topic' then "stuff" else topic'
        o = replace "#nick" n $ replace "#topic" t $ words msg
    if null $ dropWhile (\x -> x == ' ') $ concat o then return [] else do
      replace' [] o
  where
    replace' :: [String] -> [String] -> IO String
    replace' a []
      | format    = f $ toUpperSentence $ endSentence a
      | otherwise = f a
      where
        f = return . dePlenk . unwords . dedup
    replace' a m =
      if rl > 0 then do
        let w = if null a then topic' else last a
            s = rr [w]
        r <- fixIt lock debug' s [] 1 0 0 15
        if null r then return [] else
          replace' (f' $ a ++ r) $ f' xs
      else
        replace' (f' $ a ++ [x]) $ f' xs
      where
        x  = head m
        xs = fTail [] m
        re = reverse $ x Regex.=~ "#random[0-9]+" :: String
        rn = reverse $ takeWhile isDigit re
        rl = if x == "#random" then 3 :: Int else
               if null re then 0 else
                 read rn :: Int
        rr = replyRandom' lock fugly' params'{sTries=5, sLength=rl} True False
        f' = words . unwords
defsReplace _ _ _ _ _ _ = return []

splitDef :: String -> (String, String)
splitDef [] = ([], [])
splitDef x  = let q = reverse $ dropWhile (\c -> c == ' ') $ reverse $ takeWhile (\c -> c /= '=') x
                  a = dropWhile (\c -> c == ' ') $ tail $ dropWhile (\c -> c /= '=') x in
              (q, a)

fixIt :: MVar () -> Bool -> [IO String] -> [String] -> Int -> Int
         -> Int -> Int -> IO [String]
fixIt _  _ []     a _ _ _ _ = return a
fixIt lock d (x:xs) a n i j s = do
    xx <- x
    _  <- if d then evalStateT (hPutStrLnLock stdout ("> debug: fixIt try: " ++ show j)) lock else return ()
    if i >= n || j > s * n then return a
      else if null xx then fixIt lock d xs a n i (j + 1) s
      else fixIt lock d xs (a ++ [(if null a then [] else " ") ++ xx]) n (i + 1) j s
