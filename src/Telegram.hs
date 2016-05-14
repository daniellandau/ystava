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

data Conf = Conf { _token :: Token, _icalUrl :: T.Text }

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
      log_ (show . length $ updates)
      log_ (show offset)
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
  Conf { _token = token, _icalUrl = icalUrl } <- ask
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
  let isGoodGroup = group == chatId
  _ <- if isGoodGroup || (not (isGroup chatType) && isGoodUser)
    then do
      _ <- if isGoodGroup && not isGoodUser then addUser userId else return ()
      choice <- liftIO $ if (isGoodGroup && not (T.isInfixOf "@ystava" (maybe "" id textMaybe))) then return NoResponse else choose textMaybe
      response <- case choice of
        NoResponse -> return ""
        NextPractice -> liftIO $ LT.toStrict <$> getNextPractice icalUrl
        StaticResponse t -> return t
      if T.length response > 0
        then liftIO $ sendMessage token (SendMessageRequest (toText chatId) response (Just Markdown) Nothing Nothing Nothing) >> return ()
        else liftIO . return $ ()
    else liftIO $ sendMessage token (SendMessageRequest (toText chatId)
                                     "Tämä botti on vain YStävien käyttöön. Puhu minulle YS-kanavalla (ja kun tunnemme toisemme myös yksityischätissä)" (Just Markdown) Nothing Nothing Nothing) >> return ()
  return maxSeenOffset

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
