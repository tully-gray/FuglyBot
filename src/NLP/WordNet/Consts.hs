module NLP.WordNet.Consts where

import System.FilePath (joinPath)

makePath :: [FilePath] -> FilePath
makePath = joinPath

dictDir :: FilePath
dictDir         = "/dict"
defaultPath :: FilePath
defaultPath     = "/usr/local/WordNet-2.0/dict"
defaultBin :: FilePath
defaultBin      = "/usr/local/WordNet-2.0/bin"
