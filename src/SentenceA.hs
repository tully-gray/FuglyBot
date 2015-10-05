module SentenceA where

import           Control.Concurrent             (MVar)
import           Control.Monad.Trans.State.Lazy (evalStateT)
import           Data.Char                      (toLower, toUpper)
import qualified Data.Map.Strict                as Map (lookup, size)
import           Data.Maybe
import           FuglyLib
import           NLP.WordNet                    hiding (Word)
import           NLP.WordNet.PrimTypes
import           System.IO                      (stdout)
import qualified System.Random                  as Random
import qualified Text.Regex.Posix               as Regex

sentenceA :: MVar () -> Fugly -> Int -> Bool -> Bool -> Bool -> Int
             -> Int -> Int -> String -> [String] -> IO String
sentenceA _ _ _ _ _ _ _ _ _ _ [] = return []
sentenceA st fugly@Fugly{pgf=pgf', aspell=aspell', wne=wne', dict=dict'}
  r debug rwords stopic randoms stries slen topic' msg = do
    rr <- Random.getStdRandom (Random.randomR (0, 99)) :: IO Int
    let len = length msg
    if len > 1 && len <= (fromIntegral nsize :: Int) && r < 50 then
      let pad = take (fromIntegral nsize :: Int) $ cycle [" "] in
      nnReply st fugly debug (msg ++ pad)
      else do
      sentenceA' rr len msg
  where
    sentenceA' :: Int -> Int -> [String] -> IO String
    sentenceA' _ _ [] = return []
    sentenceA' r' l w
      -- | l < 7 && s2l w Regex.=~ "hi$|hello|greetings|welcome" =
      --   return (case mod r' 6 of
      --    0 -> "Hello my friend."
      --    1 -> "Hi there."
      --    2 -> "Thanks."
      --    3 -> "Hey there pal."
      --    4 -> "Good morning."
      --    5 -> "Hey, what's going on?"
      --    _ -> [])
      | l == 1 && r < 70 = do
          let s = sentenceB' st fugly r debug True rwords
                                stopic 0 10 5 5 topic' w
          mm <- fixIt st debug s [] 1 0 0 50
          return $ unwords mm
      | l < 4 && r < 60 || r + r' < 20 = case mod r' 4 of
         0 -> do
          let s = sentenceB' st fugly r debug True rwords stopic 5
                  10 4 4 topic' [topic']
          mm <- fixIt st debug (return ("Do you like " ++ topic' ++
                                        "?") : s) [] 2 0 0 40
          return $ unwords mm
         1 -> do
          let s = sentenceB' st fugly r debug True rwords stopic randoms
                  10 6 6 topic' (words "perhaps you")
          mm <- fixIt st debug (return ("Not really.") : s) [] 2 0 0 60
          return $ unwords mm
         2 -> do
          let s = sentenceB' st fugly r debug True rwords stopic 75 10
                  7 7 topic' $ words "can you"
          mm <- fixIt st debug s [] 1 0 0 70
          return $ unwords mm
         3 -> do
          let s = sentenceB' st fugly r debug True rwords stopic randoms
                  10 6 6 topic' $ words "I think that"
          mm <- fixIt st debug s [] 1 0 0 60
          return $ unwords mm
         _ -> return []
      -- | l < 7 && r' < 31 && s2l w == "test" = return (case mod r' 4 of
      --    0 -> "What are we testing?"
      --    1 -> "But I don't want to test..."
      --    2 -> "Is this just a test?"
      --    3 -> "Test it yourself."
      --    _ -> [])
      -- | l < 8 && r' < 17 && (map toLower $ unwords w) Regex.=~ "really" =
      --   return (case mod r' 3 of
      --    0 -> "Not really."
      --    1 -> "Yes really."
      --    2 -> "Oh really."
      --    _ -> [])
      -- | l < 9 && r' < 65 && (map toLower $ unwords w) Regex.=~
      --   "lol|haha|hehe|rofl|lmao|funny|humorous" = return (case mod r' 7 of
      --    0 -> "Very funny."
      --    1 -> "What's so funny?"
      --    2 -> "It's not funny."
      --    3 -> "Please don't laugh."
      --    4 -> "Oh really."
      --    5 -> "I'm glad you think this is funny."
      --    6 -> "Seriously."
      --    _ -> [])
      | l > 2 && s2l w == "hey" = do
          let s = sentenceB' st fugly r debug False rwords stopic
                                randoms stries slen 5 topic' (drop 2 w)
                                ++ [gfRandom pgf' []]
          m' <- fixIt st debug s [] 1 0 0 $ stries * slen
          w' <- s1r r' m'
          x' <- asReplaceWords st fugly w'
          return $ unwords $ toUpperSentence $ endSentence x'
      | (map toLower $ unwords w) Regex.=~
         "rhyme|rhymes|sing.*|song|songs" || r' > 87 = do
          let s = sentenceB' st fugly r debug False rwords stopic
                                randoms stries slen 5 topic' w
                                ++ [gfRandom pgf' []]
          m' <- fixIt st debug s [] 1 0 0 (stries * slen)
          w' <- s1r r' m'
          x' <- asReplaceWords st fugly w'
          return $ unwords $ toUpperSentence $ endSentence x'
      -- | l > 3 && (s1l $ take 3 w) == ["do", "you", w!!2 Regex.=~
      --        "like|hate|love|have|want|need"] = do
      --   noun <- getNoun wne' r' w
      --   let nouns' = nouns noun
      --       s1b :: Int -> [String] -> String
      --       s1b rr ww = ww!!2 Regex.=~ "like|hate|love|have|want|need" ++ " " ++
      --                   if rr < 20 then "it" else
      --                     if rr < 40 then "that" else nouns' in
      --     return $ unwords $ toUpperSentence $ endSentence $ words
      --     (case mod r' 5 of
      --       0 -> "I don't " ++ s1b r' w
      --       1 -> "yeah, I " ++ s1b r' w
      --       2 -> "sometimes I " ++ s1b r' w
      --       3 -> "can you " ++ s1b r' w
      --       4 -> (noun ++ "s") ++ " are " ++ if r' < 50 then
      --             "not" else "" ++ " something I " ++ w!!2 Regex.=~
      --                   "like|hate|love|have|want|need"
      --       _ -> [])
      -- | l > 2 && (s1l $ take 2 w) == ["can", "you"] = return (case mod r' 6 of
      --    0 -> "No I can't."
      --    1 -> "Sure, I can do that."
      --    2 -> "It depends..."
      --    3 -> "Do you ever stop and ponder?"
      --    4 -> "Why would I want to do something like that?"
      --    5 -> "It is " ++ if r' < 20 then "boring." else if r' < 50 then
      --             "fun." else "certainly possible."
      --    _ -> [])
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
              let s = sentenceB' st fugly r debug True rwords stopic randoms
                      5 9 9 topic' [b, "is", "tasty", "and"]
              mm <- fixIt st debug (return ("Oh yes, " ++ b ++ ".") : s)
                      [] 2 0 0 45
              return $ unwords mm
            1 -> do
              let s = sentenceB' st fugly r debug True rwords stopic randoms
                      8 5 5 topic' [b, "is"]
              mm <- fixIt st debug s [] 1 0 0 40
              return $ unwords mm
            2 -> do
              let s = sentenceB' st fugly r debug True rwords stopic randoms
                      5 6 6 topic' ["my"]
              mm <- fixIt st debug s [] 1 0 0 30
              return $ unwords mm
            3 -> do
              let s = sentenceB' st fugly r debug True rwords stopic randoms
                      5 9 9 topic' ["this", b, "is"]
              mm <- fixIt st debug s [] 1 0 0 45
              return $ unwords mm
            _ -> return []
            else return []
      | r' < 70 && Map.size dict' < 50 = gfRandom pgf' []
      | otherwise = return []
    s1l = map (\x -> map toLower x)
    s2l x = map toLower $ head x
    s1r rr mm = mapM (\x -> do ry <- rhymesWith st aspell' x
                               if null ry then return []
                                 else return $ ry!!(mod rr (length ry))) mm

sentenceB :: MVar () -> Fugly -> Int -> Bool -> Bool -> Bool -> Int -> Int
             -> Int -> Int -> String -> Int -> [String] -> IO [String]
sentenceB st fugly@Fugly{pgf=pgf'} r debug rwords stopic randoms stries
               slen plen topic' num msg = do
    m <- chooseWord msg
    let mm = if length msg < 4 || mod (length $ concat msg) 3 == 0 then
                msg else m
    fixIt st debug (sentenceB' st fugly r debug False rwords stopic
                    randoms stries slen plen topic' mm ++
                    [gfRandom pgf' []]) [] num 0 0 (stries * slen)

sentenceB' :: MVar () -> Fugly -> Int -> Bool -> Bool -> Bool -> Bool -> Int
               -> Int -> Int -> Int -> String -> [String] -> [IO String]
sentenceB' _ _ _ _ _ _ _ _ _ _ _ _ [] = [return []] :: [IO String]
sentenceB' st fugly@Fugly{dict=dict', pgf=pgf', wne=wne', aspell=aspell'}
  r debug first rwords stopic randoms stries slen plen topic' msg = do
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
          wnReplaceWords fugly rwords randoms $ filter (not . null)
            $ take (stries * slen) d
    let s1d x = do
          w <- x
          if null w then return []
            else return (init w ++ (cleanString (last w) ++ if elem
              (map toLower $ head w) qWords then "?" else ".") : [])
    let s1e x = do
          w <- x
          n <- isName st aspell' dict' $ fHead [] w
          if null w || n then return []
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
