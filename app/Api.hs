{-# LANGUAGE OverloadedStrings #-}

module Api where

import Data.ByteString (ByteString)
import Network.HTTP.Client.Conduit (Request)
import Network.HTTP.Simple (
  defaultRequest,
  setRequestHeader,
  setRequestHost,
  setRequestPath,
  setRequestPort,
  setRequestQueryString,
  setRequestSecure,
 )

createRequest :: ByteString -> ByteString -> Request
createRequest apiUrl apiKey =
  setRequestSecure True $
    setRequestPort port $
      setRequestPath requestPath $
        setRequestQueryString queryString $
          setRequestHeader "X-API-Key" [apiKey] $
            setRequestHost apiUrl defaultRequest
 where
  port = 443
  requestPath = "/v3/poi/"
  queryString = [("output", Just "json"), ("countrycode", Just "US"), ("maxresults", Just "10")]