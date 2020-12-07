module Main where

import Database
import Http
import Parse

main :: IO ()
main = do
  putStrLn "[CountryCode] - Country Codes available\n\
    \AU        Australia\n\
    \BR        Brazil\n\
    \CA        Canada\n\
    \DE        Germany\n\
    \ES        Spain\n\
    \FR        France\n\
    \IE        Ireland\n\
    \Input Country Code - \n"
  countryCodeInput <- getLine
  putStrLn "[Year] - Country Codes available\n\
    \2019      Calendar year 2019\n\
    \2020      Calendar year 2020\n\
    \2021      Calendar year 2021\n\
    \Input Year Code - \n"
  yearInput <- getLine
  let url = "https://date.nager.at/api/v2/publicholidays/"++yearInput++"/"++countryCodeInput
  print "Downloading..."
  json <- download url
  print "Parsing"
  case parse json of
    Left err -> print err
    Right recs -> do
      print recs
      putStrLn "Saving on DB"
      putStrLn "\n****************"
      conn <- initialiseDB
      insertDB conn recs
      insertSB conn recs
      insertLB conn recs
      putStrLn "Done!"
      putStrLn "\n****************"
      res <- queryDB conn countryCodeInput
      putStrLn "\n****************"
      putStrLn "Before Formatting Query"
      putStr $ show res
      putStrLn "\n****************"
      putStrLn "After Formatting Query"
      let converRes = sqlRowToString res
      mapM_ putStrLn converRes
      putStrLn "\n****************"
      sqlValue <- getUnprocessedSQLHolidays conn
      print $ show sqlValue
      print "Done!"
      putStrLn "\n****************"
      print "Date"
      dateSort <- selectHolidaysInDateRange conn "1-JAN-20" "31-JUL-20"
      print (concat dateSort)
      putStrLn "\n****************"
      putStrLn $ "Number of rows is " ++ show (length res)