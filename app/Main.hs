module Main where

import Api (createRequest)
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT (runReaderT), asks)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Vector (Vector)
import Env (getOpenChargeApiKey, getOpenChargeApiUrl)
import Network.HTTP.Conduit (Request (requestBody), Response (responseBody))
import Network.HTTP.Simple (
  JSONException,
  getResponseBody,
  httpJSONEither,
 )
import Types

type App a = ReaderT Config IO a

data Config = Config {apiUrl :: ByteString, apiKey :: ByteString}

loadConfig :: IO Config
loadConfig = Config <$> getOpenChargeApiUrl <*> getOpenChargeApiKey

runApp :: App ()
runApp = do
  config <- ask
  let request = createRequest (apiUrl config) (apiKey config)
  response <- liftIO $ httpJSONEither request :: App (Response (Either JSONException (Vector Poi)))
  liftIO $ print $ getResponseBody response

main :: IO ()
main = do
  loadFile defaultConfig
  loadConfig >>= runReaderT runApp

-- response <- liftIO $ httpJSONEither request
-- liftIO $ print $ getResponseBody response

-- main :: IO ()
-- main = do
--   loadFile defaultConfig
--   apiUrl <- getOpenChargeApiUrl
--   apiKey <- getOpenChargeApiKey
--   let request = createRequest apiUrl apiKey
--   response <- httpJSONEither request :: IO (Response (Either JSONException (Vector Poi)))
--   print $ getResponseBody response
