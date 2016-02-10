module Fugly.Types where

import           AI.NeuralNetworks.Simple       (NeuralNetwork)
import           Data.Map.Lazy                  (Map)
import           Language.Aspell                (SpellChecker)
import           NLP.WordNet                    (WordNetEnv)
import           NLP.WordNet.PrimTypes          (EPOS)
import           PGF                            (PGF)

type Dict    = Map String Fugly.Types.Word
type Default = (DType, String)
type NSet    = [([Float], [Float])]
type NMap    = Map Int String

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

data DType = Default | Normal | Response | Action | GreetAction | Greeting | Enter deriving (Eq, Read, Show)
