module Main where

import Api (CreateRequestDeps (CreateRequestDeps), getPoi)
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.Reader (ReaderT (runReaderT))
import Env (getOpenChargeApiKey, getOpenChargeApiUrl)

loadConfig :: IO CreateRequestDeps
loadConfig = CreateRequestDeps <$> getOpenChargeApiUrl <*> getOpenChargeApiKey

main :: IO ()
main = do
  loadFile defaultConfig
  config <- loadConfig
  result <- runReaderT getPoi config
  print result
