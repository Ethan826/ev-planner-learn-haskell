{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Api where

import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import Network.HTTP.Client.Conduit (Request, Response)
import Network.HTTP.Simple (
  JSONException,
  defaultRequest,
  getResponseBody,
  httpJSONEither,
  setRequestHeader,
  setRequestHost,
  setRequestPath,
  setRequestPort,
  setRequestQueryString,
  setRequestSecure,
 )
import Types (Poi)

data CreateRequestDeps = CreateRequestDeps
  { apiUrl :: ByteString
  , apiKey :: ByteString
  }

createRequest :: ReaderT CreateRequestDeps IO Request
createRequest = do
  CreateRequestDeps{apiUrl, apiKey} <- ask
  return $
    setRequestSecure True $
      setRequestPort port $
        setRequestPath requestPath $
          setRequestQueryString queryString $
            setRequestHeader "X-API-Key" [apiKey] $
              setRequestHost apiUrl defaultRequest
 where
  port = 443
  requestPath = "/v3/poi/"
  queryString =
    [ ("output", Just "json")
    , ("countrycode", Just "US")
    , ("maxresults", Just "10")
    ]

performRequest ::
  ReaderT
    CreateRequestDeps
    IO
    (Response (Either JSONException (Vector Poi)))
performRequest = do
  request <- createRequest
  httpJSONEither request

getPoi ::
  ReaderT
    CreateRequestDeps
    IO
    (Either JSONException (Vector Poi))
getPoi = getResponseBody <$> performRequest
