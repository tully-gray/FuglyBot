module Fugly.Types where

import           AI.NeuralNetworks.Simple       (NeuralNetwork)
import           Control.Concurrent             (MVar, ThreadId)
import           Data.Map.Lazy                  (Map)
import           Fugly.Parameter                (Parameter)
import           Language.Aspell                (SpellChecker)
import           NLP.WordNet                    (WordNetEnv)
import           NLP.WordNet.PrimTypes          (EPOS)
import           PGF                            (PGF)
import           System.IO                      (Handle)

type ChanNicks = Map String [String]
type Default   = (DType, String)
type Dict      = Map String Fugly.Types.Word
type FState    = (MVar Bot, MVar (), MVar [ThreadId], MVar ChanNicks)
type NMap      = Map Int String
type NSet      = [([Float], [Float])]

data Bot = Bot {
    handle :: Handle,
    params :: Parameter,
    fugly  :: Fugly
    }

data Fugly = Fugly {
    dict    :: Dict,
    defs    :: [Default],
    pgf     :: Maybe PGF,
    wne     :: WordNetEnv,
    aspell  :: SpellChecker,
    ban     :: [String],
    match   :: [String],
    nnet    :: NeuralNetwork Float,
    nset    :: NSet,
    nmap    :: NMap
    }

data Word = Word {
    word     :: String,
    count    :: Int,
    before   :: Map String Int,
    after    :: Map String Int,
    banafter :: [String],
    topic    :: [String],
    related  :: [String],
    pos      :: EPOS
    } |
  Name {
    name     :: String,
    count    :: Int,
    before   :: Map String Int,
    after    :: Map String Int,
    banafter :: [String],
    topic    :: [String]
    } |
  Acronym {
    acronym    :: String,
    count      :: Int,
    before     :: Map String Int,
    after      :: Map String Int,
    banafter   :: [String],
    topic      :: [String],
    definition :: String
    }

data DType = Default | Normal | Response | Regex | Action |
             GreetAction | Greeting | Enter deriving (Eq, Read, Show)

getBot :: FState -> MVar Bot
getBot (b, _, _, _) = b

getLock :: FState -> MVar ()
getLock (_, l, _, _) = l

getTCount :: FState -> MVar [ThreadId]
getTCount (_, _, tc, _) = tc

getChanNicks :: FState -> MVar (Map String [String])
getChanNicks (_, _, _, cn) = cn
