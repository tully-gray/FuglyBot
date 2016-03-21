module Fugly.Reply where

import           Control.Concurrent             (MVar)
import           Control.Monad.Trans.State.Lazy (evalStateT)
import           Data.Char                      (toLower, toUpper)
import           Data.Map.Strict                as Map (lookup)
import           Data.Maybe
import           Data.List                      (intercalate)
import           Fugly.Neural
import           Fugly.Types                    hiding (fugly)
import           FuglyLib
import           NLP.WordNet.PrimTypes
import           System.IO                      (stdout)
import qualified System.Random                  as Random
import           Text.EditDistance              as EditDistance
import qualified Text.Regex.Posix               as Regex

replyResponse :: MVar () -> Fugly -> Int -> Bool -> Bool -> Bool -> Int
                 -> Float -> String -> String -> String -> IO String
replyResponse _ _ _ _ _ _ _ _ _ _ []  = return []
replyResponse st fugly@Fugly{defs=defs'} r debug rwords stopic randoms
                 dist nick' topic' msg = do
    if debug then
      evalStateT (hPutStrLnLock stdout "> debug: replyResponse") st
      else return ()
    let r' = [de | (t, de) <- defs', t == Response]
        l  = map splitDef r'
        md = dist + (realToFrac (length msg) / 4 :: Float)
        bl = bestLevenshtein msg l
        d  = if null bl then maxBound :: Int else (\((_, _), d') -> d') $ head bl
        o  = map (\((_, o'), _) -> o') bl
    if (realToFrac d :: Float) <= md then
      defsReplace st fugly r debug rwords stopic randoms topic' nick' $ o!!(mod r $ length o)
      else return []

replyRegex :: MVar () -> Fugly -> Int -> Bool -> Bool -> Bool -> Int
                 -> String -> String -> String -> IO String
replyRegex _ _ _ _ _ _ _ _ _ []  = return []
replyRegex st fugly@Fugly{defs=defs'} r debug rwords stopic randoms
                 nick' topic' msg = do
    if debug then
      evalStateT (hPutStrLnLock stdout "> debug: replyRegex") st
      else return ()
    let m' = [de | (t, de) <- defs', t == Regex]
        l  = map splitDef m'
        o  = [a | (q, a) <- l, msg Regex.=~ q]
    if not $ null o then
      defsReplace st fugly r debug rwords stopic randoms topic' nick' $ o!!(mod r $ length o)
      else return []

replyNeural :: MVar () -> Fugly -> Int -> Bool -> [String] -> IO String
replyNeural _  _     _    _     []  = return []
replyNeural st fugly plen debug msg =
    let pad = take (fromIntegral nsize :: Int) $ cycle [" "] in
    nnReply st fugly plen debug (msg ++ pad)

replyMixed :: MVar () -> Fugly -> Int -> Bool -> Bool -> Bool -> Int
             -> Int -> Int -> String -> [String] -> IO String
replyMixed _ _ _ _ _ _ _ _ _ _ [] = return []
replyMixed st fugly@Fugly{wne=wne', match=match'}
  r debug rwords stopic randoms stries slen topic' msg = do
    if debug then
      evalStateT (hPutStrLnLock stdout "> debug: replyMixed") st
      else return ()
    rr <- Random.getStdRandom (Random.randomR (0, 99)) :: IO Int
    replyMixed' rr msg
  where
    l = length msg
    replyMixed' :: Int -> [String] -> IO String
    replyMixed' _  [] = return []
    replyMixed' r' w
      | mBool = do
        let s = replyRandom' st fugly r debug True True rwords stopic randoms
                10 6 6 topic' $ words mString
        mm <- fixIt st debug s [] 1 0 0 60
        return $ unwords mm
      | l > 3 && (map (\x -> map toLower x) $ take 3 w) == ["do", "you", w!!2 Regex.=~
             "like|hate|love|have|want|need"] = do
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
            4 -> (noun ++ "s") ++ " are " ++ if r' < 50 then
                  "not" else "" ++ " something I " ++ w!!2 Regex.=~
                        "like|hate|love|have|want|need"
            _ -> [])
      | l > 3 && l < 15 = do
          let ra = mod r 4
          let t  = case ra of
                0 -> "food"
                1 -> "animal"
                2 -> "tool"
                3 -> "abstraction"
                _ -> []
          (a, b) <- evalStateT (sentenceMeet wne' t w) st
          if a then case ra of
            0 -> do
              let s = replyRandom' st fugly r debug True True rwords stopic randoms
                      5 9 9 topic' [b, "is", "tasty", "and"]
              mm <- fixIt st debug (return ("Oh yes, " ++ b ++ ".") : s)
                      [] 2 0 0 45
              return $ unwords mm
            1 -> do
              let s = replyRandom' st fugly r debug True True rwords stopic randoms
                      8 5 5 topic' [b, "is"]
              mm <- fixIt st debug s [] 1 0 0 40
              return $ unwords mm
            2 -> do
              let s = replyRandom' st fugly r debug True True rwords stopic randoms
                      5 6 6 topic' ["my"]
              mm <- fixIt st debug s [] 1 0 0 30
              return $ unwords mm
            3 -> do
              let s = replyRandom' st fugly r debug True True rwords stopic randoms
                      5 9 9 topic' ["this", b, "is"]
              mm <- fixIt st debug s [] 1 0 0 45
              return $ unwords mm
            _ -> return []
            else return []
      | l < 6 && r < 60 || r + r' < 50 = case mod r' 4 of
         0 -> do
          let s = replyRandom' st fugly r debug True True rwords stopic 5
                  10 4 4 topic' [topic']
          mm <- fixIt st debug (return ("Do you like " ++ topic' ++
                                        "?") : s) [] 2 0 0 40
          return $ unwords mm
         1 -> do
          let s = replyRandom' st fugly r debug True True rwords stopic randoms
                  10 6 6 topic' (words "perhaps you")
          mm <- fixIt st debug (return ("Not really.") : s) [] 2 0 0 60
          return $ unwords mm
         2 -> do
          let s = replyRandom' st fugly r debug True True rwords stopic 75 10
                  7 7 topic' $ words "can you"
          mm <- fixIt st debug s [] 1 0 0 70
          return $ unwords mm
         3 -> do
          let s = replyRandom' st fugly r debug True True rwords stopic randoms
                  10 6 6 topic' $ words "I think that"
          mm <- fixIt st debug s [] 1 0 0 60
          return $ unwords mm
         _ -> return []
      | l == 1 && r < 70 = do
          let s = replyRandom' st fugly r debug True True rwords
                                stopic 0 10 5 5 topic' w
          mm <- fixIt st debug s [] 1 0 0 50
          return $ unwords mm
      | otherwise = do
        noun <- getNoun wne' r' w
        let s = replyRandom' st fugly r debug True True rwords stopic
                randoms stries slen 5 topic' ["this", noun, "is"]
        m' <- fixIt st debug s [] 1 0 0 (stries * slen)
        return $ unwords $ toUpperSentence $ endSentence m'
    mList   = map toLower (" " ++ intercalate " | " match' ++ " ")
    mBool   = (" " ++ map toLower (unwords msg) ++ " ") Regex.=~ mList :: Bool
    mString = (" " ++ map toLower (unwords msg) ++ " ") Regex.=~ mList :: String

replyRandom :: MVar () -> Fugly -> Int -> Bool -> Bool -> Bool -> Int -> Int
             -> Int -> Int -> String -> Int -> [String] -> IO String
replyRandom st fugly r debug rwords stopic randoms stries
               slen plen topic' num msg = do
    if debug then
      evalStateT (hPutStrLnLock stdout "> debug: replyRandom") st
      else return ()
    let mm = if length msg < 4 || mod (length $ concat msg) 3 == 0 then
                msg else [msg!!(mod r $ length msg)]
    o <- fixIt st debug (replyRandom' st fugly r debug False True rwords stopic
           randoms stries slen plen topic' mm) [] num 0 0 (stries * slen)
    return $ unwords o

replyRandom' :: MVar () -> Fugly -> Int -> Bool -> Bool -> Bool -> Bool -> Bool -> Int
               -> Int -> Int -> Int -> String -> [String] -> [IO String]
replyRandom' _ _ _ _ _ _ _ _ _ _ _ _ _ [] = [return []] :: [IO String]
replyRandom' st fugly@Fugly{dict=dict', pgf=pgf', wne=wne', aspell=aspell'}
  r debug first punc rwords stopic randoms stries slen plen topic' msg = do
    let s1h n a x = let out = if a then map toUpper x else
                                if n then x else map toLower x in
          if isJust $ Map.lookup out dict' then out else []
    let s1a x = do
          a <- isAcronym st aspell' dict' x
          n <- isName st aspell' dict' x
          b <- getNoun wne' r msg
          z <- findNextWord fugly 0 randoms True stopic topic' b x
          let zz = fHead [] z
          y <- findNextWord fugly 1 randoms True stopic topic' b zz
          let yy = fHead [] y
          let c  = if null zz && null yy then 2 else
                     if null zz || null yy then 3 else 4
          w  <- s1b fugly slen c b $ findNextWord fugly 1 randoms False
                  stopic topic' b x
          ww <- s1b fugly slen 0 b $ return msg
          let d  = if first then ww else [yy] ++ [zz] ++ [s1h n a x] ++ w
          o <- wnReplaceWords fugly rwords randoms $ filter (not . null)
                 $ take (stries * slen) d
          return $ dedupD o
    let s1d x = do
          w <- x
          if null w then return []
            else if not punc then x
              else return (init w ++ (cleanString (fLast [] w) ++ if elem
                (map toLower $ head w) qWords then "?" else ".") : [])
    let s1e x = do
          w <- x
          n <- isName st aspell' dict' $ fHead [] w
          if null w then return []
            else if not punc && not n then x
              else return ([s1c w] ++ tail w)
    let s1g = map (\x -> do y <- x ; z <- insertCommas wne' 0 y
                            return $ dePlenk $ unwords z)
              (map (s1e . s1d . s1a) (msg ++ sWords))
    s1f 0 s1t s1g
  where
    s1b :: Fugly -> Int -> Int -> String -> IO [String] -> IO [String]
    s1b f n i noun msg' = do
      ww <- msg'
      if null $ concat ww then return []
        else if i >= n then return $ dedupD ww else do
               www <- findNextWord f i randoms False stopic topic' noun
                        $ fLast [] ww
               s1b f n (i + 1) noun (return $ ww ++ www)
    s1c :: [String] -> String
    s1c [] = []
    s1c w  = [toUpper $ head $ head w] ++ (fTail [] $ head w)
    s1t :: Int -> IO String -> IO String
    s1t i x = do
      y <- x
      p <- wnPartPOS wne' $ cleanString $ fLast [] $ words y
      let plen' = if ((realToFrac i) :: Float) >
                     (((realToFrac stries) :: Float) / 2) then 0 else plen
      if null y then return []
        else if gfParseBool pgf' plen' y && length (words y) > 2 &&
                    p /= POS Adj then return y
             else if debug then evalStateT (hPutStrLnLock stdout
               ("> debug: sentence try: " ++ y)) st >> return []
                  else return []
    s1f _ _ []     = []
    s1f i f (x:xs) = f i x : s1f (i + 1) f xs

replyDefault :: MVar () -> Fugly -> Int -> Bool -> String -> String -> IO String
replyDefault st f@Fugly{defs=defs', pgf=pgf'} r debug nick' topic' = do
    if debug then
      evalStateT (hPutStrLnLock stdout "> debug: replyDefault") st
      else return ()
    let d    = [de | (t, de) <- defs', t == Default]
        lenD = length d
    if lenD == 0 then gfRandom pgf' []
      else defsReplace st f 23 debug False False 17 topic' nick' $ d!!mod r lenD

bestLevenshtein :: String -> [(String, String)] -> [((String, String), Int)]
bestLevenshtein _   []    = [(([], []), maxBound :: Int)]
bestLevenshtein []  _     = [(([], []), maxBound :: Int)]
bestLevenshtein msg defs' = filter (\x -> snd x == min') dl
  where
    dists :: [((String, String), Int)] -> [(String, String)] -> [((String, String), Int)]
    dists o []     = o
    dists o (x:xs) = dists ((x, levenshteinDistance defaultEditCosts msg $ fst x) : o) xs
    dl   = dists [] defs'
    min' = minimum [d | (_, d) <- dl]

defsReplace :: MVar () -> Fugly -> Int -> Bool -> Bool -> Bool -> Int
              -> String -> String -> String -> IO String
defsReplace _  _     _ _     _      _      _       _      _    [] = return []
defsReplace st fugly r debug rwords stopic randoms topic' nick' m = do
    let s = if m Regex.=~ "#random" then
               replyRandom' st fugly r debug False False rwords stopic randoms
               5 3 3 topic' [topic']
            else []
    mm <- fixIt st debug s [] 1 0 0 15
    let m'  = unwords mm
        n'  = if null nick' then "somebody" else nick'
        tt' = if null topic' then "stuff" else topic'
        o'  = replace "#nick" n' $ replace "#topic" tt' $ words m
        f'  = return . dePlenk . unwords
    if null m' then f' o' else
      f' $ replace "#random" m' o'

splitDef :: String -> (String, String)
splitDef x = let q = reverse $ dropWhile (\c -> c == ' ') $ reverse $ takeWhile (\c -> c /= '=') x
                 a = dropWhile (\c -> c == ' ') $ tail $ dropWhile (\c -> c /= '=') x in
             (q, a)

fixIt :: MVar () -> Bool -> [IO String] -> [String] -> Int -> Int
         -> Int -> Int -> IO [String]
fixIt _  _ []     a _ _ _ _ = return a
fixIt st d (x:xs) a n i j s = do
    xx <- x
    _  <- if d then evalStateT (hPutStrLnLock stdout ("> debug: fixIt try: " ++ show j)) st else return ()
    if i >= n || j > s * n then return a
      else if null xx then fixIt st d xs a n i (j + 1) s
      else fixIt st d xs (a ++ [(if null a then [] else " ") ++ xx]) n (i + 1) j s
