{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Data.Aeson (FromJSON (parseJSON), ToJSON, Value (Object), decode, genericParseJSON, withText, (.:))
import Data.Aeson.Casing (aesonPrefix, pascalCase)
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
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

getOpenChargeApiUrl :: IO ByteString
getOpenChargeApiUrl = fromString <$> getEnv "OPEN_CHARGE_API_URL"

getOpenChargeApiKey :: IO ByteString
getOpenChargeApiKey = fromString <$> getEnv "OPEN_CHARGE_API_KEY"

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

main :: IO ()
main = do
  loadFile defaultConfig
  apiUrl <- getOpenChargeApiUrl
  apiKey <- getOpenChargeApiKey
  let request = createRequest apiUrl apiKey
  response <- httpJSONEither request :: IO (Response (Either JSONException (Vector Poi)))
  print $ getResponseBody response

data Poi = Poi
  { dataProviderPoi :: DataProvider
  , operatorInfoPoi :: Maybe OperatorInfo
  , usageTypePoi :: UsageType
  , statusTypePoi :: StatusType
  , submissionStatusPoi :: SubmissionStatus
  , userCommentsPoi :: Maybe Text
  , percentageSimilarityPoi :: Maybe Text
  , mediaItemsPoi :: Maybe Text
  , isRecentlyVerifiedPoi :: Bool
  , dateLastVerifiedPoi :: Text
  , poiIDPoi :: Int
  , uuidPoi :: Text
  , parentChargePointIDPoi :: Maybe Text
  , dataProviderIDPoi :: Int
  , dataProvidersReferencePoi :: Text
  , operatorIDPoi :: Maybe Int
  , operatorsReferencePoi :: Maybe Text
  , usageTypeIDPoi :: Int
  , usageCostPoi :: Maybe Text
  , addressInfoPoi :: AddressInfo
  , connectionsPoi :: Vector Connection
  , numberOfPointsPoi :: Maybe Text
  , generalCommentsPoi :: Maybe Text
  , datePlannedPoi :: Maybe Text
  , dateLastConfirmedPoi :: Maybe Text
  , statusTypeIDPoi :: Int
  , dateLastStatusUpdatePoi :: Text
  , metadataValuesPoi :: Maybe Text
  , dataQualityLevelPoi :: Int
  , dateCreatedPoi :: Text
  , submissionStatusTypeIDPoi :: Int
  }
  deriving (Show)

data AddressInfo = AddressInfo
  { addressInfoIDAddressInfo :: Int
  , titleAddressInfo :: Text
  , addressLine1AddressInfo :: Text
  , addressLine2AddressInfo :: Maybe Text
  , townAddressInfo :: Text
  , stateOrProvinceAddressInfo :: Text
  , postcodeAddressInfo :: Text
  , countryIDAddressInfo :: Int
  , countryAddressInfo :: Country
  , latitudeAddressInfo :: Float
  , longitudeAddressInfo :: Float
  , contactTelephone1AddressInfo :: Text
  , contactTelephone2AddressInfo :: Maybe Text
  , contactEmailAddressInfo :: Maybe Text
  , accessCommentsAddressInfo :: AccessComments
  , relatedURLAddressInfo :: Text
  , distanceAddressInfo :: Maybe Text
  , distanceUnitAddressInfo :: Int
  }
  deriving (Show)

data AccessComments
  = Available24HoursDailyAccessComments
  | EmptyAccessComments
  | The24HoursDailyAccessComments
  deriving (Show)

data Country = Country
  { isoCodeCountry :: ISOCode
  , continentCodeCountry :: ContinentCode
  , countryIDCountry :: Int
  , titleCountry :: CountryTitle
  }
  deriving (Show)

data ContinentCode
  = NaContinentCode
  deriving (Show)

data ISOCode
  = UsISOCode
  deriving (Show)

data CountryTitle
  = UnitedStatesCountryTitle
  deriving (Show)

data Connection = Connection
  { connectionIDConnection :: Int
  , connectionTypeIDConnection :: Int
  , connectionTypeConnection :: ConnectionType
  , referenceConnection :: Maybe Text
  , statusTypeIDConnection :: Maybe Text
  , statusTypeConnection :: Maybe Text
  , levelIDConnection :: Int
  , levelConnection :: Level
  , ampsConnection :: Maybe Int
  , voltageConnection :: Maybe Int
  , powerKWConnection :: Float
  , currentTypeIDConnection :: Int
  , currentTypeConnection :: CurrentType
  , quantityConnection :: Int
  , commentsConnection :: ConnectionComments
  }
  deriving (Show)

data ConnectionComments
  = KWPowerIsAnEstimateBasedOnTheConnectionTypeConnectionComments
  deriving (Show)

data ConnectionType = ConnectionType
  { formalNameConnectionType :: Maybe FormalName
  , isDiscontinuedConnectionType :: Maybe Bool
  , isObsoleteConnectionType :: Maybe Bool
  , connectionTypeIDConnectionType :: Int
  , titleConnectionType :: Text
  }
  deriving (Show)

data FormalName
  = IEC621963ConfigurationAAFormalName
  | IEC621963ConfigurationEEFormalName
  | SaeJ17722009FormalName
  deriving (Show)

data CurrentType = CurrentType
  { descriptionCurrentType :: Description
  , currentTypeIDCurrentType :: Int
  , titleCurrentType :: CurrentTypeTitle
  }
  deriving (Show)

data Description
  = AlternatingCurrentSinglePhaseDescription
  | DirectCurrentDescription
  deriving (Show)

data CurrentTypeTitle
  = ACSinglePhaseCurrentTypeTitle
  | DcCurrentTypeTitle
  deriving (Show)

data Level = Level
  { commentsLevel :: LevelComments
  , isFastChargeCapableLevel :: Bool
  , levelIDLevel :: Int
  , titleLevel :: LevelTitle
  }
  deriving (Show)

data LevelComments
  = Over2KWUsuallyNonDomesticSocketTypeLevelComments
  | The40KWAndHigherLevelComments
  deriving (Show)

data LevelTitle
  = Level2MediumOver2KWLevelTitle
  | Level3HighOver40KWLevelTitle
  deriving (Show)

data DataProvider = DataProvider
  { websiteURLDataProvider :: Text
  , commentsDataProvider :: Maybe Text
  , dataProviderStatusTypeDataProvider :: DataProviderStatusType
  , isRestrictedEditDataProvider :: Bool
  , isOpenDataLicensedDataProvider :: Bool
  , isApprovedImportDataProvider :: Bool
  , licenseDataProvider :: Text
  , dateLastImportedDataProvider :: Text
  , dataProviderIDDataProvider :: Int
  , titleDataProvider :: DataProviderTitle
  }
  deriving (Show)

data DataProviderStatusType = DataProviderStatusType
  { isProviderEnabledDataProviderStatusType :: Bool
  , dataProviderStatusTypeIDDataProviderStatusType :: Int
  , titleDataProviderStatusType :: DataProviderStatusTypeTitle
  }
  deriving (Show)

data DataProviderStatusTypeTitle
  = AutomatedImportDataProviderStatusTypeTitle
  deriving (Show)

data DataProviderTitle
  = AfdcEnergyGovDataProviderTitle
  deriving (Show)

data OperatorInfo = OperatorInfo
  { websiteURLOperatorInfo :: Text
  , commentsOperatorInfo :: Maybe Text
  , phonePrimaryContactOperatorInfo :: Maybe Text
  , phoneSecondaryContactOperatorInfo :: Maybe Text
  , isPrivateIndividualOperatorInfo :: Bool
  , addressInfoOperatorInfo :: Maybe Text
  , bookingURLOperatorInfo :: Maybe Text
  , contactEmailOperatorInfo :: Maybe Text
  , faultReportEmailOperatorInfo :: Maybe Text
  , isRestrictedEditOperatorInfo :: Bool
  , operatorInfoIDOperatorInfo :: Int
  , titleOperatorInfo :: Text
  }
  deriving (Show)

data StatusType = StatusType
  { isOperationalStatusType :: Bool
  , isUserSelectableStatusType :: Bool
  , statusTypeIDStatusType :: Int
  , titleStatusType :: StatusTypeTitle
  }
  deriving (Show)

data StatusTypeTitle
  = OperationalStatusTypeTitle
  | PlannedForFutureDateStatusTypeTitle
  deriving (Show)

data SubmissionStatus = SubmissionStatus
  { isLiveSubmissionStatus :: Bool
  , submissionStatusIDSubmissionStatus :: Int
  , titleSubmissionStatus :: SubmissionStatusTitle
  }
  deriving (Show)

data SubmissionStatusTitle
  = ImportedAndPublishedSubmissionStatusTitle
  deriving (Show)

data UsageType = UsageType
  { isPayAtLocationUsageType :: Maybe Text
  , isMembershipRequiredUsageType :: Maybe Bool
  , isAccessKeyRequiredUsageType :: Maybe Text
  , usageTypeIDUsageType :: Int
  , titleUsageType :: UsageTypeTitle
  }
  deriving (Show)

data UsageTypeTitle
  = PrivateRestrictedAccessUsageTypeTitle
  | PublicUsageTypeTitle
  deriving (Show)

instance FromJSON Poi where
  parseJSON (Object v) =
    Poi
      <$> v .: "DataProvider"
      <*> v .: "OperatorInfo"
      <*> v .: "UsageType"
      <*> v .: "StatusType"
      <*> v .: "SubmissionStatus"
      <*> v .: "UserComments"
      <*> v .: "PercentageSimilarity"
      <*> v .: "MediaItems"
      <*> v .: "IsRecentlyVerified"
      <*> v .: "DateLastVerified"
      <*> v .: "ID"
      <*> v .: "UUID"
      <*> v .: "ParentChargePointID"
      <*> v .: "DataProviderID"
      <*> v .: "DataProvidersReference"
      <*> v .: "OperatorID"
      <*> v .: "OperatorsReference"
      <*> v .: "UsageTypeID"
      <*> v .: "UsageCost"
      <*> v .: "AddressInfo"
      <*> v .: "Connections"
      <*> v .: "NumberOfPoints"
      <*> v .: "GeneralComments"
      <*> v .: "DatePlanned"
      <*> v .: "DateLastConfirmed"
      <*> v .: "StatusTypeID"
      <*> v .: "DateLastStatusUpdate"
      <*> v .: "MetadataValues"
      <*> v .: "DataQualityLevel"
      <*> v .: "DateCreated"
      <*> v .: "SubmissionStatusTypeID"

instance FromJSON AddressInfo where
  parseJSON (Object v) =
    AddressInfo
      <$> v .: "ID"
      <*> v .: "Title"
      <*> v .: "AddressLine1"
      <*> v .: "AddressLine2"
      <*> v .: "Town"
      <*> v .: "StateOrProvince"
      <*> v .: "Postcode"
      <*> v .: "CountryID"
      <*> v .: "Country"
      <*> v .: "Latitude"
      <*> v .: "Longitude"
      <*> v .: "ContactTelephone1"
      <*> v .: "ContactTelephone2"
      <*> v .: "ContactEmail"
      <*> v .: "AccessComments"
      <*> v .: "RelatedURL"
      <*> v .: "Distance"
      <*> v .: "DistanceUnit"

instance FromJSON AccessComments where
  parseJSON = withText "AccessComments" parseText
   where
    parseText "Available 24 hours daily" = return Available24HoursDailyAccessComments
    parseText "" = return EmptyAccessComments
    parseText "24 hours daily" = return The24HoursDailyAccessComments

instance FromJSON Country where
  parseJSON (Object v) =
    Country
      <$> v .: "ISOCode"
      <*> v .: "ContinentCode"
      <*> v .: "ID"
      <*> v .: "Title"

instance FromJSON ContinentCode where
  parseJSON = withText "ContinentCode" parseText
   where
    parseText "NA" = return NaContinentCode

instance FromJSON ISOCode where
  parseJSON = withText "ISOCode" parseText
   where
    parseText "US" = return UsISOCode

instance FromJSON CountryTitle where
  parseJSON = withText "CountryTitle" parseText
   where
    parseText "United States" = return UnitedStatesCountryTitle

instance FromJSON Connection where
  parseJSON (Object v) =
    Connection
      <$> v .: "ID"
      <*> v .: "ConnectionTypeID"
      <*> v .: "ConnectionType"
      <*> v .: "Reference"
      <*> v .: "StatusTypeID"
      <*> v .: "StatusType"
      <*> v .: "LevelID"
      <*> v .: "Level"
      <*> v .: "Amps"
      <*> v .: "Voltage"
      <*> v .: "PowerKW"
      <*> v .: "CurrentTypeID"
      <*> v .: "CurrentType"
      <*> v .: "Quantity"
      <*> v .: "Comments"

instance FromJSON ConnectionComments where
  parseJSON = withText "ConnectionComments" parseText
   where
    parseText "kW power is an estimate based on the connection type" = return KWPowerIsAnEstimateBasedOnTheConnectionTypeConnectionComments

instance FromJSON ConnectionType where
  parseJSON (Object v) =
    ConnectionType
      <$> v .: "FormalName"
      <*> v .: "IsDiscontinued"
      <*> v .: "IsObsolete"
      <*> v .: "ID"
      <*> v .: "Title"

instance FromJSON FormalName where
  parseJSON = withText "FormalName" parseText
   where
    parseText "IEC 62196-3 Configuration AA" = return IEC621963ConfigurationAAFormalName
    parseText "IEC 62196-3 Configuration EE" = return IEC621963ConfigurationEEFormalName
    parseText "SAE J1772-2009" = return SaeJ17722009FormalName

instance FromJSON CurrentType where
  parseJSON (Object v) =
    CurrentType
      <$> v .: "Description"
      <*> v .: "ID"
      <*> v .: "Title"

instance FromJSON Description where
  parseJSON = withText "Description" parseText
   where
    parseText "Alternating Current - Single Phase" = return AlternatingCurrentSinglePhaseDescription
    parseText "Direct Current" = return DirectCurrentDescription

instance FromJSON CurrentTypeTitle where
  parseJSON = withText "CurrentTypeTitle" parseText
   where
    parseText "AC (Single-Phase)" = return ACSinglePhaseCurrentTypeTitle
    parseText "DC" = return DcCurrentTypeTitle

instance FromJSON Level where
  parseJSON (Object v) =
    Level
      <$> v .: "Comments"
      <*> v .: "IsFastChargeCapable"
      <*> v .: "ID"
      <*> v .: "Title"

instance FromJSON LevelComments where
  parseJSON = withText "LevelComments" parseText
   where
    parseText "Over 2 kW, usually non-domestic socket type" = return Over2KWUsuallyNonDomesticSocketTypeLevelComments
    parseText "40KW and Higher" = return The40KWAndHigherLevelComments

instance FromJSON LevelTitle where
  parseJSON = withText "LevelTitle" parseText
   where
    parseText "Level 2 : Medium (Over 2kW)" = return Level2MediumOver2KWLevelTitle
    parseText "Level 3:  High (Over 40kW)" = return Level3HighOver40KWLevelTitle

instance FromJSON DataProvider where
  parseJSON (Object v) =
    DataProvider
      <$> v .: "WebsiteURL"
      <*> v .: "Comments"
      <*> v .: "DataProviderStatusType"
      <*> v .: "IsRestrictedEdit"
      <*> v .: "IsOpenDataLicensed"
      <*> v .: "IsApprovedImport"
      <*> v .: "License"
      <*> v .: "DateLastImported"
      <*> v .: "ID"
      <*> v .: "Title"

instance FromJSON DataProviderStatusType where
  parseJSON (Object v) =
    DataProviderStatusType
      <$> v .: "IsProviderEnabled"
      <*> v .: "ID"
      <*> v .: "Title"

instance FromJSON DataProviderStatusTypeTitle where
  parseJSON = withText "DataProviderStatusTypeTitle" parseText
   where
    parseText "Automated Import" = return AutomatedImportDataProviderStatusTypeTitle

instance FromJSON DataProviderTitle where
  parseJSON = withText "DataProviderTitle" parseText
   where
    parseText "afdc.energy.gov" = return AfdcEnergyGovDataProviderTitle

instance FromJSON OperatorInfo where
  parseJSON (Object v) =
    OperatorInfo
      <$> v .: "WebsiteURL"
      <*> v .: "Comments"
      <*> v .: "PhonePrimaryContact"
      <*> v .: "PhoneSecondaryContact"
      <*> v .: "IsPrivateIndividual"
      <*> v .: "AddressInfo"
      <*> v .: "BookingURL"
      <*> v .: "ContactEmail"
      <*> v .: "FaultReportEmail"
      <*> v .: "IsRestrictedEdit"
      <*> v .: "ID"
      <*> v .: "Title"

instance FromJSON StatusType where
  parseJSON (Object v) =
    StatusType
      <$> v .: "IsOperational"
      <*> v .: "IsUserSelectable"
      <*> v .: "ID"
      <*> v .: "Title"

instance FromJSON StatusTypeTitle where
  parseJSON = withText "StatusTypeTitle" parseText
   where
    parseText "Operational" = return OperationalStatusTypeTitle
    parseText "Planned For Future Date" = return PlannedForFutureDateStatusTypeTitle

instance FromJSON SubmissionStatus where
  parseJSON (Object v) =
    SubmissionStatus
      <$> v .: "IsLive"
      <*> v .: "ID"
      <*> v .: "Title"

instance FromJSON SubmissionStatusTitle where
  parseJSON = withText "SubmissionStatusTitle" parseText
   where
    parseText "Imported and Published" = return ImportedAndPublishedSubmissionStatusTitle

instance FromJSON UsageType where
  parseJSON (Object v) =
    UsageType
      <$> v .: "IsPayAtLocation"
      <*> v .: "IsMembershipRequired"
      <*> v .: "IsAccessKeyRequired"
      <*> v .: "ID"
      <*> v .: "Title"

instance FromJSON UsageTypeTitle where
  parseJSON = withText "UsageTypeTitle" parseText
   where
    parseText "Private - Restricted Access" = return PrivateRestrictedAccessUsageTypeTitle
    parseText "Public" = return PublicUsageTypeTitle
