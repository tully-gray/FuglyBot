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
import           Data.Char                      (toLower, toUpper)
import           Data.Map.Strict                as Map (lookup)
import           Data.Maybe
import           Data.List                      (intercalate)
import           Fugly.Parameter
import           Fugly.Types                    hiding (fugly, topic)
import           FuglyLib
import           NLP.WordNet.PrimTypes
import           System.IO                      (stdout)
import qualified System.Random                  as Random
import           Text.EditDistance              as EditDistance
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
      defsReplace lock fugly' params' nick' $ o!!(mod r $ length o)
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
      defsReplace lock fugly' params' nick' $ o!!(mod r $ length o)
      else return []
replyRegex _ _ _ _ _ _ = return []

replyMixed :: MVar () -> Fugly -> Parameter -> Int -> [String] -> IO String
replyMixed _ _ _ _ [] = return []
replyMixed lock fugly'@Fugly{wne=wne', match=match'}
    params'@Parameter{debug=debug', sTries=stries, sLength=slen, topic=topic'}
    r msg = do
    if debug' then
      evalStateT (hPutStrLnLock stdout "> debug: replyMixed") lock
      else return ()
    rr <- Random.getStdRandom (Random.randomR (0, 99)) :: IO Int
    replyMixed' rr msg
  where
    l = length msg
    repRand = replyRandom' lock fugly' params' True True
    fixIt'  = fixIt lock debug'
    replyMixed' :: Int -> [String] -> IO String
    replyMixed' _  [] = return []
    replyMixed' r' w
      | mBool = do
        let s = repRand $ words mString
        m <- fixIt' s [] 1 0 0 60
        return $ unwords m
      | l > 3 && (map (\x -> map toLower x) $ take 3 w) == ["do", "you", w!!2 Regex.=~ "like|hate|love|have|want|need"] = do
        noun <- getNoun wne' r' w
        let nouns' = nouns noun
            s1b :: Int -> [String] -> String
            s1b rr ww = ww!!2 Regex.=~ "like|hate|love|have|want|need" ++ " " ++
              if rr < 20 then "it" else
                if rr < 40 then "that" else nouns' in
          return $ unwords $ toUpperSentence $ endSentence $ words
          (case mod r' 5 of
            0 -> "I don't " ++ s1b r' w
            1 -> "yeah, I " ++ s1b r' w
            2 -> "sometimes I " ++ s1b r' w
            3 -> "can you " ++ s1b r' w
            4 -> nouns' ++ " are " ++ if r' < 50 then "not" else
                   " something I " ++ w!!2 Regex.=~ "like|hate|love|have|want|need"
            _ -> [])
      | l > 3 && l < 15 = do
          let ra = mod r 4
          let t  = case ra of
                0 -> "food"
                1 -> "animal"
                2 -> "tool"
                3 -> "abstraction"
                _ -> []
          (a, b) <- evalStateT (sentenceMeet wne' t w) lock
          if a then case ra of
            0 -> do
              let s = repRand [b, "is", "tasty", "and"]
              mm <- fixIt' (return ("Oh yes, " ++ b ++ ".") : s) [] 2 0 0 45
              return $ unwords mm
            1 -> do
              let s = repRand [b, "is"]
              mm <- fixIt' s [] 1 0 0 40
              return $ unwords mm
            2 -> do
              let s = repRand ["my"]
              mm <- fixIt' s [] 1 0 0 30
              return $ unwords mm
            3 -> do
              let s = repRand ["this", b, "is"]
              mm <- fixIt' s [] 1 0 0 45
              return $ unwords mm
            _ -> return []
            else return []
      | l < 6 && r + r' < 25 = case mod r' 4 of
         0 -> do
          let s = repRand [topic']
          mm <- fixIt' (return ("Do you like " ++ topic' ++ "?") : s) [] 2 0 0 40
          return $ unwords mm
         1 -> do
          let s = repRand $ words "perhaps you"
          mm <- fixIt' s [] 2 0 0 60
          return $ unwords mm
         2 -> do
          let s = repRand $ words "can you"
          mm <- fixIt' s [] 1 0 0 70
          return $ unwords mm
         3 -> do
          let s = repRand $ words "I think that"
          mm <- fixIt' s [] 1 0 0 60
          return $ unwords mm
         _ -> return []
      | l == 1 && r < 70 = do
          let s = repRand w
          mm <- fixIt' s [] 1 0 0 50
          return $ unwords mm
      | otherwise = do
        noun <- getNoun wne' r' w
        let s = repRand ["this", noun, "is"]
        m' <- fixIt' s [] 1 0 0 (stries * slen)
        return $ unwords $ toUpperSentence $ endSentence m'
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
    Parameter{debug=debug', replaceWord=rw, strictTopic=st', randoms=rand,
              sTries=stries, sLength=slen, pLength=plen, topic=top}
    first punc msg = do
    let s1h n a x = let out = if a then map toUpper x else
                                if n then x else map toLower x in
          if isJust $ Map.lookup out dict' then out else []
    let s1a x = do
          r <- Random.getStdRandom (Random.randomR (0, 999)) :: IO Int
          a <- isAcronym lock aspell' dict' x
          n <- isName lock aspell' dict' x
          b <- getNoun wne' r msg
          let z  = findNextWord fugly r 0 rand True st' top b x
              zz = fHead [] z
              y  = findNextWord fugly r 1 rand True st' top b zz
              yy = fHead [] y
              c  = if null zz && null yy then 2 else
                     if null zz || null yy then 3 else 4
              w  = s1b fugly r slen c b $ findNextWord fugly r 1 rand False
                   st' top b x
              ww = s1b fugly r slen 0 b msg
              d  = if first then ww else [yy] ++ [zz] ++ [s1h n a x] ++ w
          o <- wnReplaceWords fugly rw rand $ filter (not . null) $ take (stries * slen) d
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
    let s1g = map (\x -> do y <- x ; z <- insertCommas wne' 0 y
                            return $ dePlenk $ unwords z)
              (map (s1e . s1d . s1a) (msg ++ sWords))
    s1f 0 s1t s1g
  where
    s1b :: Fugly -> Int -> Int -> Int -> String -> [String] -> [String]
    s1b f r n i noun w =
      if null $ concat w then return [] else
        if i >= n then dedupD w else
          let ww = findNextWord f r i rand False st' top noun $ fLast [] w in
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
        if gfParseBool pgf' plen' y && length (words y) > 2 && pos' /= POS Adj then return y else
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
      else defsReplace lock fugly' params'{replaceWord=False, strictTopic=False} nick' $ dList!!mod r len
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

defsReplace :: MVar () -> Fugly -> Parameter -> String -> String -> IO String
defsReplace _ _ _ _ [] = return []
defsReplace lock fugly
    p@Parameter{debug=d, topic=top}
    nick' msg = do
    let n = if null nick' then "somebody" else nick'
        t = if null top then "stuff" else top
        o = replace "#nick" n $ replace "#topic" t $ words msg
    replace' [] o
  where
    repRand = replyRandom' lock fugly p{sTries=5, sLength=3, pLength=3} True False
    replace' :: [String] -> [String] -> IO String
    replace' a [] = return $ dePlenk $ unwords a
    replace' [] (x:xs) =
      if x == "#random" then do
        let s = repRand [top]
        m <- fixIt lock d s [] 1 0 0 15
        if null m then return [] else
          replace' m xs
      else replace' [x] xs
    replace' a [x] =
      if x == "#random" then do
        let s = repRand [top]
        m <- fixIt lock d s [] 1 0 0 15
        if null m then return [] else
          replace' (a ++ m) []
      else replace' (a ++ [x]) []
    replace' a (x:y:xs) =
      if y == "#random" then do
        let s = repRand [x]
        m <- fixIt lock d s [] 1 0 0 15
        if null m then return [] else
          replace' (a ++ m) xs
      else replace' (a ++ [x]) $ y : xs
defsReplace _ _ _ _ _ = return []

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
