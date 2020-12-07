{-# LANGUAGE DeriveGeneric #-}

module Parse where

import Data.Aeson ( eitherDecode, FromJSON, ToJSON )
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics ( Generic )

-- -- Sample format
-- [
--   {
--     "date": "2020-01-01",
--     "localName": "New Year's Day",
--     "name": "New Year's Day",
--     "countryCode": "GB",
--     "fixed": false,
--     "global": true,
--   }
--   ]
data HolidayRecord = HolidayRecord {
    date :: String,
    localName :: String,
    name :: String,
    countryCode :: String,
    global::Bool,
    fixed::Bool
} deriving (Show, Generic)


-- data CountryRecord = CountryRecord {
--     countryCode' :: String

-- data CountriesRecord = CountriesRecord {
--     countryCode' :: String,
--      global::Bool
-- } deriving (Show, Generic)

-- data Country_holidaysRecord = Country_holidaysRecord {
--     countryCode'' :: String,
--     localName' :: String
-- } deriving (Show, Generic)

instance FromJSON HolidayRecord
instance ToJSON HolidayRecord

parse :: L8.ByteString -> Either String [HolidayRecord]
parse json = eitherDecode json :: Either String [HolidayRecord]
