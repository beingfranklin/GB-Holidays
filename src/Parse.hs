{-# LANGUAGE DeriveGeneric #-}

module Parse
    ( parse,
    HolidayRecord
    ) where

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
    fixed :: Bool
} deriving (Show, Generic)
    
instance FromJSON HolidayRecord
instance ToJSON HolidayRecord

-- data OuterRecords = OuterRecords{
--     records :: [InnerRecord]
-- }deriving (Show, Generic)

-- instance FromJSON OuterRecords
-- instance ToJSON OuterRecords

parse :: L8.ByteString -> Either String [HolidayRecord]
parse json = eitherDecode json :: Either String [HolidayRecord]

