module Calendar where

import Text.ICalendar
import Network.HTTP.Conduit
import Data.Time
import qualified Data.Map.Lazy as Map
import qualified Data.Text.Lazy as LT
import Data.Function           (on)
import Data.Monoid
import Data.Maybe
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding      as TE
import qualified Data.ByteString.Lazy as L
import Data.Default
import Data.Time.LocalTime.TimeZone.Series
import Data.Time.LocalTime.TimeZone.Olson
import Control.Monad (join)

data YsEvent = YsEvent { _start :: LocalTime, _summary :: LT.Text, _location :: Maybe LT.Text } deriving (Show, Eq)

instance Ord YsEvent where
  (<=) = (<=) `on` _start

utcToLocal :: String -> UTCTime -> IO LocalTime
utcToLocal tzName time = do
  timeZone <- timeZoneFor tzName time
  return $ utcToLocalTime timeZone time

timeZoneFor :: String -> UTCTime -> IO TimeZone
timeZoneFor tzName time = do
  tzSeries <- getTimeZoneSeriesFromOlsonFile $ "/usr/share/zoneinfo/" ++ tzName
  return $ timeZoneFromSeries tzSeries time

extractUtcTime :: DTStart -> Maybe UTCTime
extractUtcTime (DTStartDateTime { dtStartDateTimeValue = UTCDateTime { dateTimeUTC = t}}) =
  Just t
extractUtcTime _ = Nothing

whenNextPractice :: T.Text -> IO LT.Text
whenNextPractice url = do
  nextPracticeMaybe <- getNextPractice url
  return $ maybe (LT.pack "Ei löytynyt seuraavia treenejä")
    (\nextPractice ->
       "Seuraavat treenit, otsikolla \"" <>
       _summary nextPractice <> "\" aikana " <>
       (LT.pack . formatTime defaultTimeLocale "%d.%m. kello %H:%M." . _start $ nextPractice)) $
    nextPracticeMaybe

whereNextPractice :: T.Text -> IO LT.Text
whereNextPractice url = do
  nextPracticeMaybe <- getNextPractice url
  let locationMaybe = join (fmap _location nextPracticeMaybe)
  return $ maybe (LT.pack "Ei tietoa, joten varmaan Musiikkisalissa")
    ("Seuraavien harjoitusten sijainti on " <>) locationMaybe

getNextPractice :: T.Text -> IO (Maybe YsEvent)
getNextPractice url = do
  icals <- fetchIcal url
  let events = Map.elems . vcEvents . head $ icals
  nowUtc <- getCurrentTime
  now <-  utcToLocal "Europe/Helsinki" nowUtc
  timeZone <- timeZoneFor "Europe/Helsinki" nowUtc
  let ysEventsMaybes = map (\event -> if (isJust $ localTime timeZone event) then Just (YsEvent (fromJust $ localTime timeZone event) (summary event) (location event)) else Nothing) $ events
  let ysEvents = catMaybes ysEventsMaybes
  return . headMay $ filter (\x -> let summary' = _summary x in LT.isInfixOf "reeni" summary' || LT.isInfixOf "arjoitu" summary') $ dropWhile ((< now) . _start) $ sort $ ysEvents
    where summary = summaryValue . fromJust . veSummary
          localTime timeZone event =
                         let utcTimeMaybe = (extractUtcTime . fromJust . veDTStart $ event)
                             localTimeMaybe = if (isJust utcTimeMaybe)
                                           then
                                             let localTime = utcToLocalTime timeZone (fromJust utcTimeMaybe)
                                             in   Just localTime
                                           else Nothing
                         in localTimeMaybe
          location event = join $ fmap ((\loc -> if LT.length loc > 0 then Just loc else Nothing) . locationValue) $ veLocation event

headMay :: [a] -> Maybe a
headMay (x:_) = Just x
headMay _     = Nothing

fetchIcal :: T.Text -> IO [VCalendar]
fetchIcal url = do
  request <- parseUrl $ T.unpack url
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  let body = responseBody response
  let body' = L.fromStrict . TE.encodeUtf8 $ T.replace "\r\n\r\n" "\r\n" $ TE.decodeUtf8 . L.toStrict $ body
  case parseICalendar def "foo" body' of
    Left err -> error err
    Right (a, _) -> return a
