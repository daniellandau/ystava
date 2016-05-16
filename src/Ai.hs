module Ai where

import qualified Data.Text as T
import AI.HNN.FF.Network
import Numeric.LinearAlgebra
import NLP.Stemmer
import NLP.Tokenize.String
import Text.Regex.Posix
import Data.List
import Data.Maybe
import Control.Exception as Ex
import Debug.Trace

data Choice = WhereNextPractice | NextPractice | StaticResponse T.Text | NoResponse deriving (Eq, Show)

analyze :: String -> [String]
analyze sentence =
  map (stem Finnish) $ filter (=~ ("^\\w" :: String)) $ tokenize sentence

rawSamples :: [(String, Choice)]
rawSamples =
  [ ("Milloin on seuraavat treenit?", NextPractice)
  , ("Koska on seuraavat harjoitukset?", NextPractice)
  , ("Koska seuraavat harkat?", NextPractice)
  , ("Koska seuraavat treenit?", NextPractice)
  , ("Milloin seuraavat treenit?", NextPractice)
  , ("koska meillä on taas treenejä", NextPractice)
  , ("Koska treenit?", NextPractice)
  , ("Mitä kuuluu?", StaticResponse "Olen robotti, minulla on kivaa")
  , ("Hyvää päivää", StaticResponse "No moro moro")
  , ("Millaista on olla robotti?", StaticResponse "Olen robotti, minulla on kivaa")
  , ("Kuka on hyvä pesemään ikkunoita?", StaticResponse "Kuulemma Selliina")
  , ("Koska pelataan jalkapalloa", StaticResponse "En tiedä tarkkaan, mutta tod. näk. ens keskiviikkona jos on kesä.")
  , ("Hyvää huomenta", StaticResponse "No moro moro")
  , ("Onko tänään kaunis ilma?", StaticResponse "Katso vaikka http://www.supersaa.fi/")
  , ("Lähdetkö uimaan?", StaticResponse "Mieluumin en")
  , ("Millainen sää on huomenna?", StaticResponse "Katso vaikka http://www.supersaa.fi/")
  , ("Millainen ilma on huomenna?", StaticResponse "Katso vaikka http://www.supersaa.fi/")
  , ("Mkä on päivän sää?", StaticResponse "Katso vaikka http://www.supersaa.fi/")
  , ("Millainen elämä on robotilla?", StaticResponse "Ihan kivaa")
  , ("missä harjoitukset?", WhereNextPractice)
  , ("missä treenit?", WhereNextPractice)
  , ("missä parhaat bileet?", StaticResponse "YStävien seurassa tietty!")
  , ("missä on paras meno?", StaticResponse "YStävien seurassa tietty!")
  ]

corpus :: [String]
corpus = nub $ concat $ map (analyze . fst) rawSamples

answerCorpus :: [Choice]
answerCorpus = nub $ map snd rawSamples

indexOf :: String -> Maybe Int
indexOf word = elemIndex word corpus

answerIndexOf :: Choice -> Maybe Int
answerIndexOf choice = elemIndex choice answerCorpus

toSample :: String -> Vector Double
toSample sentence =
  let indices = catMaybes . map indexOf . analyze $ sentence
      list = map (\x -> if elem x indices then 1.0 else 0.0) [0..(length corpus - 1)]
  in fromList list

answerToSample :: Choice -> Vector Double
answerToSample choice =
  let index = fromJust . answerIndexOf $ choice
      list = map (\x -> if x == index then 1.0 else 0.0) [0..(length answerCorpus - 1)]
  in fromList list

samples :: Samples Double
samples = map (\rawSample -> (toSample . fst $ rawSample, answerToSample . snd $ rawSample)) rawSamples

makeNetwork :: SomeException -> IO (Network Double)
makeNetwork _ = do
  net <- createNetwork (length corpus) [60] (length answerCorpus)
  return $ trainNTimes 5000 0.8 sigmoid sigmoid' net samples

getNetwork :: IO (Network Double)
getNetwork = Ex.catch (loadNetwork "network") makeNetwork

choose :: Maybe T.Text -> Network Double -> Choice
choose queryMaybe net =
  maybe NoResponse (choose' net) queryMaybe

choose' :: Network Double -> T.Text -> Choice
choose' net query =
  let weights = output net sigmoid (toSample $ T.unpack query)
  in if maxElement weights > 0.9
     then answerCorpus !! maxIndex weights
     else traceShow weights $ StaticResponse "No nyt en ihan ymmärtänyt 😞"
