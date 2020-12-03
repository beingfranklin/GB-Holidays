{-# LANGUAGE DeriveGeneric #-}

module Parse where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics

-- -- Sample format
-- [
--   {
--     "date": "2020-01-01",
--     "localName": "New Year's Day",
--     "name": "New Year's Day",
--     "countryCode": "GB",
--     "fixed": false,
--     "global": true,
--     "counties": null,
--     "launchYear": null,
--     "type": "Public"
--   }
--   ]
data HolidayRecord = HolidayRecord {
    date :: String,
    localName :: String,
    name :: String,
    countryCode :: String,
    global::Bool
} deriving (Show, Generic)

<<<<<<< HEAD
-- data CountriesRecord = CountriesRecord {
--     countryCode' :: String,
--      global::Bool
-- } deriving (Show, Generic)

-- data Country_holidaysRecord = Country_holidaysRecord {
--     countryCode'' :: String,
--     localName' :: String
-- } deriving (Show, Generic)
=======
data CountryRecord = CountryRecord {
    countryCode' :: String
} deriving (Show, Generic)

data Country_holidaysRecord = Country_holidaysRecord {
    countryCode'' :: String,
    localName' :: String
} deriving (Show, Generic)
>>>>>>> bdf704a19fc24e50908f6a4f827af1a1720cdc4b

instance FromJSON HolidayRecord
instance ToJSON HolidayRecord

<<<<<<< HEAD



=======
>>>>>>> bdf704a19fc24e50908f6a4f827af1a1720cdc4b
-- data OuterRecords = OuterRecords{
--     records :: [InnerRecord]
-- }deriving (Show, Generic)

-- instance FromJSON OuterRecords
-- instance ToJSON OuterRecords

parse :: L8.ByteString -> Either String [HolidayRecord]
<<<<<<< HEAD
parse json = eitherDecode json :: Either String [HolidayRecord]
=======
parse json = eitherDecode json :: Either String [HolidayRecord]
>>>>>>> bdf704a19fc24e50908f6a4f827af1a1720cdc4b
