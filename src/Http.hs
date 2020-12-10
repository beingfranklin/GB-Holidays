-- |HTTP module header
module Http
    ( download
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple
    ( parseRequest, getResponseBody, httpLBS )
-- |Download function is for converting JSON from URL
download :: String -> IO L8.ByteString
download url = do
    request <- parseRequest url
    response <- httpLBS request
    return $ getResponseBody response

