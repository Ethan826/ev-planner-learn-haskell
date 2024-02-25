{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Data.Aeson (FromJSON, ToJSON, Value)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.String (IsString (fromString))
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Conduit (Request (requestBody), Response (responseBody))
import Network.HTTP.Simple
  ( JSONException,
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
  response <- httpJSONEither request :: IO (Response (Either JSONException Poi))
  print $ getResponseBody response

data Poi = Poi
  { -- dataProviderPoi :: DataProvider,
    operatorInfoPoi :: Maybe Text,
    -- usageTypePoi :: UsageType,
    -- statusTypePoi :: StatusType,
    -- submissionStatusPoi :: SubmissionStatus,
    userCommentsPoi :: Maybe Text,
    percentageSimilarityPoi :: Maybe Text,
    mediaItemsPoi :: Maybe Text,
    isRecentlyVerifiedPoi :: Bool,
    dateLastVerifiedPoi :: Text,
    poiIDPoi :: Int,
    uuidPoi :: Text,
    parentChargePointIDPoi :: Maybe Text,
    dataProviderIDPoi :: Int,
    dataProvidersReferencePoi :: Text,
    operatorIDPoi :: Maybe Text,
    operatorsReferencePoi :: Maybe Text,
    usageTypeIDPoi :: Int,
    usageCostPoi :: Maybe Text,
    addressInfoPoi :: AddressInfo,
    -- connectionsPoi :: Vector Connection,
    numberOfPointsPoi :: Maybe Text,
    generalCommentsPoi :: Maybe Text,
    datePlannedPoi :: Maybe Text,
    dateLastConfirmedPoi :: Maybe Text,
    statusTypeIDPoi :: Int,
    dateLastStatusUpdatePoi :: Text,
    metadataValuesPoi :: Maybe Text,
    dataQualityLevelPoi :: Int,
    dateCreatedPoi :: Text,
    submissionStatusTypeIDPoi :: Int
  }
  deriving (Generic, Show)

data AddressInfo = AddressInfo
  { addressInfoIDAddressInfo :: Int,
    titleAddressInfo :: Text,
    addressLine1AddressInfo :: Text,
    addressLine2AddressInfo :: Maybe Text,
    townAddressInfo :: Text,
    stateOrProvinceAddressInfo :: Text,
    postcodeAddressInfo :: Text,
    countryIDAddressInfo :: Int,
    -- countryAddressInfo :: Country,
    latitudeAddressInfo :: Float,
    longitudeAddressInfo :: Float,
    contactTelephone1AddressInfo :: Text,
    contactTelephone2AddressInfo :: Maybe Text,
    contactEmailAddressInfo :: Maybe Text,
    accessCommentsAddressInfo :: Text,
    relatedURLAddressInfo :: Text,
    distanceAddressInfo :: Maybe Text,
    distanceUnitAddressInfo :: Int
  }
  deriving (Generic, Show)

instance FromJSON Poi

instance FromJSON AddressInfo