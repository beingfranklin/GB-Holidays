module Main where

import Http
import Parse
import Database

main :: IO ()
main = do 
    let url = "https://date.nager.at/api/v2/publicholidays/2020/GB"
    print "Downloading..."
    json <- download url
    print "Parsing"
    case (parse json) of
          Left err -> print err
          -- Print first record
          Right recs -> do 
                    print recs
                    -- Print 5 records
                    -- Right recs -> print . (take 5) $ (HolidayRecord recs)
                    print "Saving on DB"
                    conn <- initialiseDB
                    insertDB conn recs 
                    print "Done!"

        -- P.S It Outputs the haskell datatype we specified

