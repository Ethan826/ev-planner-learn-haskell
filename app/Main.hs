{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api (createRequest)
import Configuration.Dotenv (defaultConfig, loadFile)
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

main :: IO ()
main = do
  loadFile defaultConfig
  apiUrl <- getOpenChargeApiUrl
  apiKey <- getOpenChargeApiKey
  let request = createRequest apiUrl apiKey
  response <- httpJSONEither request :: IO (Response (Either JSONException (Vector Poi)))
  print $ getResponseBody response
