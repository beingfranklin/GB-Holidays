module Main where

import Database
import Http
import Parse

main :: IO ()
main = do
  putStrLn "\n[CountryCode] - Country Codes available\n\
    \AU        Australia\n\
    \BR        Brazil\n\
    \CA        Canada\n\
    \DE        Germany\n\
    \ES        Spain\n\
    \FR        France\n\
    \IE        Ireland\n\
    \GB        United Kingdom\n\
    \Input Country Code - \n"
  countryCodeInput <- getLine
  putStrLn "\n[Year] - Country Codes available\n\
    \2019      Calendar year 2019\n\
    \2020      Calendar year 2020\n\
    \2021      Calendar year 2021\n\
    \Input Year Code - \n"
  yearInput <- getLine
  let url = "https://date.nager.at/api/v2/publicholidays/"++yearInput++"/"++countryCodeInput
  putStrLn  "\nDownloading..."
  json <- download url
  putStrLn  "\nParsing..."
  case parse json of
    Left err -> print err
    Right recs -> do
      putStrLn "\nSaving on DB...\n"
      conn <- initialiseDB
      insertDB conn recs
      insertSB conn recs
      insertLB conn recs
      res <- queryDB conn countryCodeInput
      putStrLn $ "\nTotal List of Holidays in " ++ countryCodeInput ++ " in the calendar year "++ yearInput
      let converRes = sqlRowToString res
      mapM_ putStrLn converRes
      putStrLn "\nSee the holidays between specific dates... "
      putStrLn "\nInput the start date [DD-MMM-YY] "
      startDate <- getLine
      putStrLn "\nInput the end date [DD-MMM-YY] "
      endDate <- getLine
      dateSort <- selectHolidaysInDateRange conn startDate endDate
      putStrLn $ "\nHolidays between " ++startDate ++ " and " ++ endDate ++ " are : \n"
      -- TODO Format the dates
      mapM_ putStrLn $ dateSort
      putStrLn $ "\nDo you want to see International or Local holidays in " ++ countryCodeInput ++" ?\nType Y for global and N for local -"
      inputGlobal <- getChar
      case inputGlobal of
        'Y'  -> do
          putStrLn  "\nInternational Holidays are : \n"
          names<- getLocalNames conn True
          putStrLn $ unlines names
        'y' -> do
          putStrLn  "\nInternational Holidays are : \n"
          names<- getLocalNames conn True
          putStrLn $ unlines names
        'N' -> do
          putStrLn  "\nLocal Holidays are : \n"
          names<- getLocalNames conn False
          putStrLn $ unlines names
        'n' -> do
          putStrLn  "\nLocal Holidays are : \n"
          names<- getLocalNames conn False
          putStrLn $ unlines names
        _ -> syntaxErrorForIsGlobal
      putStrLn $ "\nPreparing to write into the JSON file "
      putStrLn "\nWriting ..."
      jsonValue <- convertToJSON conn
      writeFile "DB.json" (jsonValue)
      putStrLn "\nFinished Writing!"


syntaxErrorForIsGlobal :: IO ()
syntaxErrorForIsGlobal = putStrLn 
      "Invalid Input\n\
      \\n\
      \Please either enter Y or N\n"