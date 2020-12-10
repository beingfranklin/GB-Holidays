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
    \GB        United Kingdom\n\
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
  print "Parsing..."
  case parse json of
    Left err -> print err
    Right recs -> do
      -- print recs
      putStrLn "Saving on DB..."
      -- putStrLn "\n****************"
      conn <- initialiseDB
      insertDB conn recs
      insertSB conn recs
      insertLB conn recs
      putStrLn "Done!"
      putStrLn "\n****************"
      res <- queryDB conn countryCodeInput
      putStrLn "\n****************"
      -- putStrLn "Before Formatting Query"
      -- putStr $ show res
      -- putStrLn "\n****************"
      putStrLn $ "\nList of Holidays in " ++ countryCodeInput
      let converRes = sqlRowToString res
      mapM_ putStrLn converRes
      putStrLn "\n****************"
      putStrLn "\nSee the holidays between specific dates... "
      putStrLn "\nInput the start date [DD-MMM-YY] "
      startDate <- getLine
      putStrLn "\nInput the end date [DD-MMM-YY] "
      endDate <- getLine
      dateSort <- selectHolidaysInDateRange conn startDate endDate
      putStrLn $ "\nHolidays between " ++startDate ++ " and " ++ endDate ++ " are : \n"
      -- TODO Format the dates
      print (concat dateSort)
      putStrLn "\n****************"
      putStrLn "\nDo you want to see Global (or Local) holidays? Type Y for global and N for local"
      inputGlobal <- getChar
      putStrLn  "\nGlobal/Local Holidays are : \n"
      case inputGlobal of
        'Y'  -> do
          names<- getLocalNames conn True
          print $ show names
        'y' -> do
          names<- getLocalNames conn True
          print $ show names
        'N' -> do
          names<- getLocalNames conn False
          print $ show names
        'n' -> do
          names<- getLocalNames conn False
          print $ show names
        _ -> syntaxErrorForIsGlobal
      putStrLn "\n****************"
      putStrLn $ "Number of rows in Table is " ++ show (length res)
      putStrLn "\n****************"
      -- | JSON Conversion code is below
      jsonValue <- convertToJSON conn
      print $ show jsonValue
      -- | JSON Conversion -> writing into file should be added below
      writeFile "DB.json" (jsonValue)
      print "Done!"
      putStrLn "\n****************"


syntaxErrorForIsGlobal :: IO ()
syntaxErrorForIsGlobal = putStrLn 
      "Invalid Input\n\
      \\n\
      \Please either enter Y or N\n"