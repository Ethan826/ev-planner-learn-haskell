{-# LANGUAGE OverloadedStrings #-}

module Env where

import Data.ByteString (ByteString)
import Data.String (IsString (fromString))
import System.Environment (getEnv)

getOpenChargeApiUrl :: IO ByteString
getOpenChargeApiUrl = fromString <$> getEnv "OPEN_CHARGE_API_URL"

getOpenChargeApiKey :: IO ByteString
getOpenChargeApiKey = fromString <$> getEnv "OPEN_CHARGE_API_KEY"
