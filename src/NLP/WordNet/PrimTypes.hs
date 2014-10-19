{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, ImplicitParams, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables #-} 

module NLP.WordNet.PrimTypes where

import Data.Array
import Data.Char (toLower)
import System.IO
import Control.Exception
import Data.Dynamic (Typeable)

type Offset = Integer

-- | The basic part of speech type, either a 'Noun', 'Verb', 'Adj'ective or 'Adv'erb.
data POS = Noun | Verb | Adj | Adv
         deriving (Eq, Ord, Show, Ix, Typeable)

allPOS :: [POS]
allPOS = [Noun ..]

data EPOS = POS POS | Satellite | AdjSatellite | IndirectAnt | DirectAnt | UnknownEPos | Pertainym
          deriving (Eq, Ord, Typeable)

fromEPOS :: EPOS -> POS
fromEPOS (POS p) = p
fromEPOS _ = Adj

allEPOS :: [EPOS]
allEPOS = [POS Noun ..]

instance Enum POS where
  toEnum 1 = Noun
  toEnum 2 = Verb
  toEnum 3 = Adj
  toEnum 4 = Adv
  toEnum _ = Noun
  fromEnum Noun = 1
  fromEnum Verb = 2
  fromEnum Adj  = 3
  fromEnum Adv  = 4
  enumFrom i       = enumFromTo i Adv
  enumFromThen i j = enumFromThenTo i j Adv

instance Enum EPOS where
  toEnum 1 = POS Noun
  toEnum 2 = POS Verb
  toEnum 3 = POS Adj
  toEnum 4 = POS Adv
  toEnum 5 = Satellite
  toEnum 6 = AdjSatellite
  toEnum 7 = DirectAnt
  toEnum 8 = IndirectAnt
  toEnum 9 = Pertainym
  toEnum _ = UnknownEPos
  fromEnum (POS Noun)   = 1
  fromEnum (POS Verb)   = 2
  fromEnum (POS Adj)    = 3
  fromEnum (POS Adv)    = 4
  fromEnum Satellite    = 5
  fromEnum AdjSatellite = 6
  fromEnum DirectAnt    = 7
  fromEnum IndirectAnt  = 8
  fromEnum Pertainym    = 9
  fromEnum UnknownEPos  = 10
  enumFrom i       = enumFromTo i AdjSatellite
  enumFromThen i j = enumFromThenTo i j AdjSatellite

instance Show EPOS where
  showsPrec i (POS p)        = showsPrec i p
  showsPrec _ (Satellite)    = showString "Satellite"
  showsPrec _ (AdjSatellite) = showString "AdjSatellite"
  showsPrec _ (IndirectAnt)  = showString "IndirectAnt"
  showsPrec _ (DirectAnt)    = showString "DirectAnt"
  showsPrec _ (Pertainym)    = showString "Pertainym"
  showsPrec _ (UnknownEPos)  = showString "UnknownEPos"

instance Ix EPOS where
  range (i, j)     = [i..j]
  index (i, _) a   = fromEnum a - fromEnum i
  inRange (i, j) a = a `elem` [i..j]

readEPOS :: String -> EPOS
readEPOS a | (map toLower a) == "n"           = POS Noun
readEPOS a | (map toLower a) == "v"           = POS Verb
readEPOS a | (map toLower a) == "a"           = POS Adj
readEPOS a | (map toLower a) == "r"           = POS Adv
readEPOS a | (map toLower a) == "s"           = Satellite
readEPOS a | (map toLower a) == "noun"        = POS Noun
readEPOS a | (map toLower a) == "verb"        = POS Verb
readEPOS a | (map toLower a) == "adj"         = POS Adj
readEPOS a | (map toLower a) == "adv"         = POS Adv
readEPOS a | (map toLower a) == "adjective"   = POS Adj
readEPOS a | (map toLower a) == "adverb"      = POS Adv
readEPOS a | (map toLower a) == "satellite"   = Satellite
readEPOS _                                    = UnknownEPos

data WordNetEnv =
     WordNetEnv {
       dataHandles :: Array POS (Handle, Handle), -- index, real
       excHandles  :: Array POS Handle, -- for morphology
       senseHandle, 
       countListHandle,
       keyIndexHandle,
       revKeyIndexHandle :: Maybe Handle,  -- these are all optional
       vSentHandle :: Maybe (Handle, Handle), -- index, real
       wnReleaseVersion :: Maybe String,
       dataDirectory :: FilePath,
       warnAbout :: String -> SomeException -> IO ()
     }

wordNetEnv0 :: WordNetEnv
wordNetEnv0 = WordNetEnv { 
                dataHandles       = undefined,
                excHandles        = undefined,
                senseHandle       = Nothing,
                countListHandle   = Nothing,
                keyIndexHandle    = Nothing,
                revKeyIndexHandle = Nothing,
                vSentHandle       = Nothing,
                wnReleaseVersion  = Nothing,
                dataDirectory     = "",
                warnAbout         = \_ _ -> return ()
              }

data SenseKey = 
     SenseKey {
       senseKeyPOS    :: POS,
       senseKeyString :: String,
       senseKeyWord   :: String
     } deriving (Show, Typeable)


-- | A 'Key' is a simple pointer into the database, which can be
-- followed using 'lookupKey'.
newtype Key = Key (Offset, POS) deriving (Eq, Typeable)

data Synset =
     Synset {
       hereIAm     :: Offset,
       ssType      :: EPOS,
       fnum        :: Int,
       pos         :: EPOS,
       ssWords     :: [(String, Int, SenseType)], -- (word, lex-id, sense)
       whichWord   :: Maybe Int,
       forms       :: [(Form, Offset, EPOS, Int, Int)],
       frames      :: [(Int, Int)],
       defn        :: String,
       key         :: Maybe Offset,
       searchType  :: Int,
       headWord    :: String,
       headSense   :: SenseType
     } deriving (Show)

synset0 :: Synset
synset0 = Synset 0 UnknownEPos (-1) undefined [] Nothing [] [] "" Nothing (-1) "" AllSenses

-- | The basic type which holds search results.  Its 'Show' instance simply
-- shows the string corresponding to the associated WordNet synset.
data SearchResult =
     SearchResult {
       srSenseKey  :: Maybe SenseKey,
       -- | This provides (maybe) the associated overview for a SearchResult.
       -- The 'Overview' is only available if this 'SearchResult' was
       -- derived from a real search, rather than 'lookupKey'.
       srOverview  :: Maybe Overview,
       srIndex     :: Maybe Index,
       -- | This provides (maybe) the associated sense number for a SearchResult.
       -- The 'SenseType' is only available if this 'SearchResult' was
       -- derived from a real search, rather than 'lookupKey'.
       srSenseNum  :: Maybe SenseType,
       srSynset    :: Synset
     } deriving (Typeable)

data Index = 
     Index {
       indexWord        :: String,
       indexPOS         :: EPOS,
       indexSenseCount  :: Int,
       indexForms       :: [Form],
       indexTaggedCount :: Int,
       indexOffsets     :: [Offset]
     } deriving (Eq, Ord, Show, Typeable)

index0 :: Index
index0 = Index "" (POS Noun) (-1) [] (-1) []

-- | The different types of relations which can hold between WordNet Synsets.
data Form = Antonym | Hypernym | Hyponym | Entailment | Similar
          | IsMember | IsStuff | IsPart
          | HasMember | HasStuff | HasPart
          | Meronym | Holonym | CauseTo | PPL | SeeAlso
          | Attribute | VerbGroup | Derivation | Classification | Class | Nominalization
          -- misc:
          | Syns | Freq | Frames | Coords | Relatives | HMeronym | HHolonym | WNGrep | OverviewForm
          | Unknown
          deriving (Eq, Ord, Show, Enum, Typeable)

allForm :: [Form]
allForm = [Antonym ..]

readForm :: String -> Form
readForm a | (map toLower a) == "antonym"         = Antonym
readForm a | (map toLower a) == "hypernym"        = Hypernym
readForm a | (map toLower a) == "hyponym"         = Hyponym
readForm a | (map toLower a) == "entailment"      = Entailment
readForm a | (map toLower a) == "similar"         = Similar
readForm a | (map toLower a) == "ismember"        = IsMember
readForm a | (map toLower a) == "isstuff"         = IsStuff
readForm a | (map toLower a) == "ispart"          = IsPart
readForm a | (map toLower a) == "hasmember"       = HasMember
readForm a | (map toLower a) == "hasstuff"        = HasStuff
readForm a | (map toLower a) == "haspart"         = HasPart
readForm a | (map toLower a) == "meronym"         = Meronym
readForm a | (map toLower a) == "holonym"         = Holonym
readForm a | (map toLower a) == "causeto"         = CauseTo
readForm a | (map toLower a) == "ppl"             = PPL
readForm a | (map toLower a) == "seealso"         = SeeAlso
readForm a | (map toLower a) == "attribute"       = Attribute
readForm a | (map toLower a) == "verbgroup"       = VerbGroup
readForm a | (map toLower a) == "derivation"      = Derivation
readForm a | (map toLower a) == "classification"  = Classification
readForm a | (map toLower a) == "class"           = Class
readForm a | (map toLower a) == "nominalization"  = Nominalization
readForm a | (map toLower a) == "syns"            = Syns
readForm a | (map toLower a) == "freq"            = Freq
readForm a | (map toLower a) == "frames"          = Frames
readForm a | (map toLower a) == "coords"          = Coords
readForm a | (map toLower a) == "relatives"       = Relatives
readForm a | (map toLower a) == "hmeronym"        = HMeronym
readForm a | (map toLower a) == "hholonym"        = HHolonym
readForm a | (map toLower a) == "wngrep"          = WNGrep
readForm a | (map toLower a) == "overviewform"    = OverviewForm
readForm _                                        = Unknown

-- | A 'SenseType' is a way of controlling search.  Either you specify
-- a certain sense (using @SenseNumber n@, or, since 'SenseType' is an
-- instance of 'Num', you can juse use @n@) or by searching using all
-- senses, through 'AllSenses'.  The 'Num' instance performs standard
-- arithmetic on 'SenseNumber's, and 'fromInteger' yields a 'SenseNumber' (always),
-- but any arithmetic involving 'AllSenses' returns 'AllSenses'.
data SenseType = AllSenses | SenseNumber Int deriving (Eq, Ord, Show, Typeable)

instance Num SenseType where
  fromInteger = SenseNumber . fromInteger
  SenseNumber n + SenseNumber m = SenseNumber $ n+m
  _ + _ = AllSenses
  SenseNumber n - SenseNumber m = SenseNumber $ n-m
  _ - _ = AllSenses
  SenseNumber n * SenseNumber m = SenseNumber $ n*m
  _ * _ = AllSenses
  negate (SenseNumber n) = SenseNumber $ negate n
  negate x = x
  abs (SenseNumber n) = SenseNumber $ abs n
  abs x = x
  signum (SenseNumber n) = SenseNumber $ signum n
  signum x = x
  

-- | The 'Overview' type is the return type which gives you an
-- overview of a word, for all sense and for all parts of speech.
data Overview =
     Overview {
       nounIndex,
       verbIndex,
       adjIndex,
       advIndex :: Maybe Index
     } deriving (Eq, Ord, Show, Typeable)
