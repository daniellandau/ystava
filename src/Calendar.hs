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

data YsEvent = YsEvent { _start :: LocalTime, _summary :: LT.Text } deriving (Show, Eq)

instance Ord YsEvent where
  (<=) = (<=) `on` _start

utcToLocal :: String -> UTCTime -> IO LocalTime
utcToLocal tzName time = do
  tzSeries <- getTimeZoneSeriesFromOlsonFile $ "/usr/share/zoneinfo/" ++ tzName
  let timeZone = timeZoneFromSeries tzSeries time
  return $ utcToLocalTime timeZone time

extractUtcTime :: DTStart -> Maybe UTCTime
extractUtcTime (DTStartDateTime { dtStartDateTimeValue = UTCDateTime { dateTimeUTC = t}}) =
  Just t
extractUtcTime _ = Nothing

getNextPractice :: T.Text -> IO LT.Text
getNextPractice url = do
  icals <- fetchIcal url
  let events = Map.elems . vcEvents . head $ icals
  now <- getCurrentTime >>= utcToLocal "Europe/Helsinki"
  let summaries = map (summaryValue . fromJust . veSummary) events
  localTimes <- mapM (\event -> do
                         let utcTimeMaybe = (extractUtcTime . fromJust . veDTStart $ event)
                         localTimeMaybe <- if (isJust utcTimeMaybe)
                                           then
                                             do localTime <- utcToLocal "Europe/Helsinki" (fromJust utcTimeMaybe)
                                                return $ Just localTime
                                           else return Nothing
                         return localTimeMaybe
                         ) events
  let ysEventsMaybes = map (\tuple -> if (isJust $ fst tuple) then Just (YsEvent (fromJust $ fst tuple) (snd tuple)) else Nothing) $ localTimes `zip` summaries
  let ysEvents = catMaybes ysEventsMaybes
  let nextPractice = head $ filter (\x -> let summary = _summary x in LT.isInfixOf "reeni" summary || LT.isInfixOf "arjoitu" summary) $ dropWhile ((< now) . _start) $ sort $ ysEvents
  return $ "Seuraavat treenit, otsikolla \"" <> _summary nextPractice <> "\" aikana " <> (LT.pack . formatTime defaultTimeLocale "%d.%m. kello %H:%M." . _start $ nextPractice)

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
