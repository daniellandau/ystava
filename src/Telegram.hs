{-# LANGUAGE DeriveGeneric #-}
module Telegram where

import Calendar
import Ai

import Web.Telegram.API.Bot
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Data.Binary.Tagged
import Data.Binary (Binary)
import GHC.Generics (Generic)
import AI.HNN.FF.Network

data Conf = Conf { _token :: !Token, _icalUrl :: !T.Text, _username :: !T.Text, _net :: !(Network Double) }

type Mystack a = ReaderT Conf (StateT MyState IO) a

newtype ChatId = ChatId T.Text deriving (Eq, Show, Generic)
instance Binary ChatId
instance HasStructuralInfo ChatId
instance HasSemanticVersion ChatId

newtype UserId = UserId Int deriving (Eq, Show, Generic)
instance Binary UserId
instance HasStructuralInfo UserId
instance HasSemanticVersion UserId

toText :: ChatId -> T.Text
toText (ChatId t) = t

data Cache = Cache { _group :: Maybe ChatId, _individuals :: [UserId] } deriving (Eq, Show, Generic)
instance Binary Cache
instance HasStructuralInfo Cache
instance HasSemanticVersion Cache

data MyState = MyState { _cache :: Cache, _offset :: Int }

log_ :: String -> Mystack ()
log_ = liftIO . putStrLn

loop :: Mystack ()
loop = do
  Conf { _token = token } <- ask
  MyState { _offset = offset } <- get
  response <- liftIO $ getUpdates token (Just offset) (Just 1) (Just 5)
  case response of
    Left a -> do
      log_ . show $ a
      loop
    Right UpdatesResponse { update_result = updates } -> do
      liftIO $ mapM_ (putStrLn . show) updates
      if length updates > 0
        then do
          newOffset <- mapM doOne updates >>= return . (+1) . maximum
          modify (\x -> x { _offset = newOffset })
          loop
        else
          loop

doOne :: Update -> Mystack Int
doOne update = do
  Conf { _token = token, _icalUrl = icalUrl, _username = userName, _net = net} <- ask
  let userId = UserId . user_id . fromJust . from . fromJust . message $ update
  let chatId = ChatId . T.pack . show . chat_id . chat . fromJust . message $ update
  let chatType = chat_type . chat . fromJust . message $ update
  let textMaybe = text . fromJust . message $ update
  let maxSeenOffset = update_id update
  MyState { _cache = cache } <- get
  group <- if isNothing (_group cache)
       then do
         let newCache = cache { _group = Just chatId}
         modify (\x -> x { _cache = newCache})
         liftIO $ taggedEncodeFile "cache" newCache
         return chatId
       else return . fromJust . _group $ cache
  isGoodUser <- isKnownGoodUser userId
  let shouldListen' = shouldListen (isGroup chatType) (T.isInfixOf userName (maybe "" id textMaybe))
  let isGoodGroup = group == chatId
  _ <- if isGoodGroup || (not (isGroup chatType) && isGoodUser)
    then do
      _ <- if isGoodGroup && not isGoodUser then addUser userId else return ()
      let choice = choose textMaybe net
      response <- case choice of
        NoResponse -> return ""
        NextPractice -> liftIO $ LT.toStrict <$> whenNextPractice icalUrl
        StaticResponse t -> return t
        WhereNextPractice -> liftIO $ LT.toStrict <$> whereNextPractice icalUrl
      if T.length response > 0 && shouldListen'
        then liftIO $ sendMessage token (SendMessageRequest (toText chatId) response (Just Markdown) Nothing Nothing Nothing) >> return ()
        else liftIO . return $ ()
    else liftIO $ if shouldListen'
                  then sendMessage token (SendMessageRequest (toText chatId)
                                          "Tämä botti on vain YStävien käyttöön. Puhu minulle YS-kanavalla (ja kun tunnemme toisemme myös yksityischätissä)" (Just Markdown) Nothing Nothing Nothing) >> return ()
                  else return ()
  return maxSeenOffset

shouldListen :: Bool -> Bool -> Bool
shouldListen isGroup' mentionsBot =
  not isGroup' || mentionsBot

isGroup :: ChatType -> Bool
isGroup Group = True
isGroup _     = False

addUser :: UserId -> Mystack ()
addUser userId = do
  modify (\state' ->
            let cache = _cache state' in state' { _cache = cache {_individuals = (_individuals cache) ++ [userId]}})
  newCache <- _cache <$> get
  liftIO $ taggedEncodeFile "cache" newCache

isKnownGoodUser :: UserId -> Mystack Bool
isKnownGoodUser userId = do
  MyState { _cache = Cache { _individuals = users }} <- get
  return $ elem userId users
