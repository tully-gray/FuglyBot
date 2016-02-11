module Fugly.Command where

import           Control.Concurrent             (MVar, readMVar, swapMVar)
import           Control.Exception              (catch, SomeException)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.State.Lazy
import           Data.Char                      (isAscii)
import qualified Data.Map.Lazy                  as Map
import           Fugly.Parameter                as P
import           Fugly.Types                    hiding (topic)
import           Fugly.LoadSave
import           FuglyLib

quit :: Bot -> FState -> Bool -> (String -> String -> StateT FState IO ())
        -> [String] -> IO Bot
quit b st o f m = if o then
    case length m of
      0 -> do evalStateT (f "QUIT" ":Bye") st >> return b
      _ -> do evalStateT (f "QUIT" (":" ++ unwords m)) st >> return b
    else return b

save :: Bot -> FState -> Bool -> (String -> IO ())
        -> (String -> StateT FState IO ()) -> IO Bot
save b@Bot{fugly=f, params=p@Parameter{fuglyDir=fdir, dictFile=dfile}}
           st o f1 f2 =
    let l = getLock st in
    if o then
      catch (do
             saveDict l f fdir dfile $ paramsToList p
             saveNeural l f fdir)
      (\e -> do let err = show (e :: SomeException)
                _ <- evalStateT (f2 ("Exception saving state: " ++ err)) st
                return ()) >> f1 "Saved bot state!" >> return b
    else return b
save b _ _ _ _ = return b

load :: Bot -> FState -> Bool -> (String -> IO ())
        -> (String -> StateT FState IO ()) -> IO Bot
load b@Bot{fugly=f@(Fugly dict' defs' pgf' wne' aspell' ban' match' _ _ _),
           params=p@Parameter{P.nick=bn, owner=owner', fuglyDir=fdir,
           dictFile=dfile, nSetSize=nsets}} st o f1 f2 =
    if o then do
      (nd, nde, nb, nm, np) <- catch (loadDict fdir dfile)
        (\e -> do let err = show (e :: SomeException)
                  _ <- evalStateT (f2 ("Exception in loadDict: " ++ err)) st
                  return (dict', defs', ban', match', paramsToList p))
      (ns, nm') <- catch (loadNeural fdir nsets)
        (\e -> do let err = show (e :: SomeException)
                  _ <- evalStateT (f2 ("Exception in loadNeural: " ++ err)) st
                  return ([], Map.empty))
      _ <- f1 "Loaded bot state!"
      return b{params=(readParamsFromList np){P.nick=bn, owner=owner',
        fuglyDir=fdir, dictFile=dfile}, fugly=f{dict=nd, defs=nde, pgf=pgf',
        wne=wne', aspell=aspell', ban=nb, match=nm, nset=ns, nmap=nm'}}
    else return b
load b _ _ _ _ = return b

part :: Bot -> FState -> Bool -> MVar (Map.Map String [String])
        -> (String -> [String] -> StateT FState IO ()) -> [String] -> IO Bot
part b st o cn f m = if o then evalStateT (do
    cn' <- lift $ readMVar cn
    f "PART" m
    lift $ swapMVar cn (Map.delete (unwords m) cn')) st >> return b
    else return b

nick :: Bot -> FState -> Bool -> (String -> IO ())
        -> (String -> String -> StateT FState IO ()) -> [String] -> IO Bot
nick b st o f1 f2 m = if o then
    case length m of
      1 -> evalStateT ((\x' -> f2 "NICK" $ cleanStringWhite isAscii x') (m!!0)) st >> return b
      _ -> f1 "Usage: !nick <nick>" >> return b
    else return b

showParams :: Bot -> Parameter -> Bool -> (String -> IO ()) -> [String] -> IO Bot
showParams b (Parameter nick' owner' _ dfile uCmd rkick maxcmsg numt nsets
              sTries' slen plen learn slearn stopic aName allowPM' debug' topic'
              randoms' rWord timer' delay' greets actions') o f m =
    if o then case length m of
      0 -> f ("nick: " ++ nick' ++ "  owner: " ++ owner' ++ "  usercommands: " ++ show uCmd
              ++ "  rejoinkick: " ++ show rkick ++ "  maxchanmsg: " ++ show maxcmsg
              ++ "  numthreads: " ++ show numt ++ "  nsetsize: " ++ show nsets
              ++ "  sentencetries: " ++ show sTries' ++ "  sentencelength: " ++ show slen
              ++ "  parselength: " ++ show plen ++ "  dictfile: " ++ dfile
              ++ "  learning: " ++ learn ++ "  strictlearn: " ++ show slearn
              ++ "  stricttopic: " ++ show stopic ++ "  debug: " ++ show debug'
              ++ "  autoname: " ++ show aName ++ "  allowpm: " ++ show allowPM'
              ++ "  topic: " ++ topic' ++ "  randoms: " ++ show randoms'
              ++ "  replacewords: " ++ show rWord
              ++ "  timer: " ++ show timer' ++ "  delay: " ++ show delay'
              ++ "  greetings: " ++ show greets ++ "  actions: " ++ show actions') >> return b
      _ -> f "Usage: !showparams" >> return b
    else return b
showParams b _ _ _ _ = return b
