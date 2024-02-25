{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Data.Aeson (FromJSON (parseJSON), Value, genericParseJSON)
import Data.Aeson.Casing (aesonPrefix, pascalCase)
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics
import Network.HTTP.Conduit (Request (requestBody), Response (responseBody))
import Network.HTTP.Simple (
  JSONException,
  defaultRequest,
  getResponseBody,
  httpJSON,
  httpJSONEither,
  setRequestHeader,
  setRequestHost,
  setRequestPath,
  setRequestPort,
  setRequestQueryString,
  setRequestSecure,
 )
import System.Environment (getEnv)

getOpenChargeApiUrl :: IO String
getOpenChargeApiUrl = getEnv "OPEN_CHARGE_API_URL"

getOpenChargeApiKey :: IO String
getOpenChargeApiKey = getEnv "OPEN_CHARGE_API_KEY"

main :: IO ()
main = do
  loadFile defaultConfig
  apiUrl <- fmap fromString getOpenChargeApiUrl
  apiKey <- fmap fromString getOpenChargeApiKey
  let request =
        setRequestSecure True $
          setRequestPort 443 $
            setRequestPath "/v3/poi/" $
              setRequestQueryString [("output", Just "json"), ("countrycode", Just "US"), ("maxresults", Just "10")] $
                setRequestHeader "X-API-Key" [apiKey] $
                  setRequestHost
                    apiUrl
                    defaultRequest
  response <- httpJSONEither request :: IO (Response (Either JSONException (Vector Poi)))
  print $ getResponseBody response

data Poi = Poi
  { poiUserComments :: Maybe Text
  , poiPercentageSimilarity :: Maybe Text
  , poiMediaItems :: Maybe Text
  , poiIsRecentlyVerified :: Bool
  , poiDateLastVerified :: Text
  , poiID :: Int
  , poiUUID :: Text
  , poiParentChargePointID :: Maybe Text
  , poiDataProviderID :: Int
  , poiDataProvidersReference :: Text
  , poiOperatorID :: Maybe Int
  , poiOperatorsReference :: Maybe Text
  , poiUsageTypeID :: Int
  , poiUsageCost :: Maybe Text
  , poiNumberOfPoints :: Maybe Text
  , poiGeneralComments :: Maybe Text
  , poiDatePlanned :: Maybe Text
  , poiDateLastConfirmed :: Maybe Text
  , poiStatusTypeID :: Int
  , poiDateLastStatusUpdate :: Text
  , poiMetadataValues :: Maybe Text
  , poiDataQualityLevel :: Int
  , poiDateCreated :: Text
  , poiSubmissionStatusTypeID :: Int
  -- , addressInfoPOIElement :: AddressInfo
  -- , connectionsPOIElement :: Vector Connection
  -- , dataProviderPOIElement :: DataProvider
  -- , operatorInfoPOIElement :: Maybe OperatorInfo
  -- , statusTypePOIElement :: StatusType
  -- , submissionStatusPOIElement :: SubmissionStatus
  -- , usageTypePOIElement :: UsageType
  }
  deriving (Generic, Show)

instance FromJSON Poi where
  parseJSON :: Value -> Parser Poi
  parseJSON = genericParseJSON $ aesonPrefix pascalCase
