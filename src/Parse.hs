{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics (Generic)

-- |Sample JSON format
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
data HolidayRecord = HolidayRecord
  { date :: String,
    localName :: String,
    name :: String,
    countryCode :: String,
    global :: Bool,
    fixed :: Bool
  }
  deriving (Show, Generic)

instance FromJSON HolidayRecord
instance ToJSON HolidayRecord 
-- where
--   toJSON (HolidayRecord date localName name countryCode global fixed) =
--     object ["date" .= date, "localName" .= localName, "name" .= name, "countryCode" .= countryCode, "global" .= global, "fixed" .= fixed]

parse :: L8.ByteString -> Either String [HolidayRecord]
parse json = eitherDecode json :: Either String [HolidayRecord]
