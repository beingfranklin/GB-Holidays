module Http
    ( download
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple
    ( parseRequest, getResponseBody, httpLBS )

download :: String -> IO L8.ByteString
download url = do
    request <- parseRequest url
    response <- httpLBS request
    return $ getResponseBody response

