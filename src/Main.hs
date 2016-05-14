module Main where
import Telegram
import qualified Data.Text as T
import System.Environment
import Control.Monad.Reader
import Web.Telegram.API.Bot
import Data.Binary.Tagged
import Control.Exception as Ex
import Control.Monad.State

main :: IO ()
main = do
  putStrLn "start app"
  token <- maybe (error "Need TOKEN") (Token . T.pack) <$> lookupEnv "TOKEN"
  icalUrl <- maybe (error "Need ICAL_URL") T.pack <$> lookupEnv "ICAL_URL"
  cache <- Ex.catch (taggedDecodeFile "cache") handleDefaultCache
  _ <- runStateT (runReaderT loop $ Conf token icalUrl) (MyState cache 0)
  return ()

handleDefaultCache :: SomeException -> IO Cache
handleDefaultCache _ = return $ Cache Nothing []
