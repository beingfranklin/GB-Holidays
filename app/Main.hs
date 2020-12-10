module Main where

import Database
    ( convertToJSON,
      getLocalNames,
      initialiseDB,
      insertDB,
      insertLB,
      insertSB,
      queryDB,
      selectHolidaysInDateRange,
      sqlRowToString )
import Http ( download )
import Parse ( parse )

-- | This is the main function of the program to download, parse and process public holidays of differnet countries of the world
main :: IO ()
main = do
  putStrLn "\nPublic Holidays of the World\n\
  \____________________________________________"
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
      putStrLn "\nSaved!\n"

      putStrLn "\nMain Menu (Choose the options)\n\
      \____________________________________________\n\
      \A -  Show all the public holidays saved on DB \n\
      \B -  Show the public holiday between two dates\n\
      \C -  Show the International/Local public holidays in the \n\
      \\n Input your option - \n"
      menuInput <- getLine
      case menuInput of
        "A" -> do
          res <- queryDB conn countryCodeInput
          putStrLn $ "\nAll the public holidays in " ++ countryCodeInput ++ " in the calendar year "++ yearInput
          let converRes = sqlRowToString res
          mapM_ putStrLn converRes
        "a" -> do
          res <- queryDB conn countryCodeInput
          putStrLn $ "\nAll the public holidays in " ++ countryCodeInput ++ " in the calendar year "++ yearInput
          let converRes = sqlRowToString res
          mapM_ putStrLn converRes
        "B" -> do
          putStrLn "\nSee the holidays between specific dates... "
          putStrLn "\nInput the start date [DD-MMM-YY] "
          startDate <- getLine
          putStrLn "\nInput the end date [DD-MMM-YY] "
          endDate <- getLine
          dateSort <- selectHolidaysInDateRange conn startDate endDate
          putStrLn $ "\nHolidays between " ++startDate ++ " and " ++ endDate ++ " are : \n"
          mapM_ putStrLn dateSort
        "b" -> do
          putStrLn "\nSee the holidays between specific dates... "
          putStrLn "\nInput the start date [DD-MMM-YY] "
          startDate <- getLine
          putStrLn "\nInput the end date [DD-MMM-YY] "
          endDate <- getLine
          dateSort <- selectHolidaysInDateRange conn startDate endDate
          putStrLn $ "\nHolidays between " ++startDate ++ " and " ++ endDate ++ " are : \n"
          mapM_ putStrLn dateSort
        "C" -> do
          putStrLn $ "\nDo you want to see International or Local holidays in " ++ countryCodeInput ++" ?\nType Y for global and N for local -"
          inputGlobal <- getLine
          case inputGlobal of
            "Y"  -> do
              putStrLn  "\nInternational Holidays are : \n"
              names<- getLocalNames conn True
              putStrLn $ unlines names
            "y" -> do
              putStrLn  "\nInternational Holidays are : \n"
              names<- getLocalNames conn True
              putStrLn $ unlines names
            "N" -> do
              putStrLn  "\nLocal Holidays are : \n"
              names<- getLocalNames conn False
              putStrLn $ unlines names
            "n" -> do
              putStrLn  "\nLocal Holidays are : \n"
              names<- getLocalNames conn False
              putStrLn $ unlines names
            _ -> syntaxErrorForIsGlobal 
        "c" -> do
          putStrLn $ "\nDo you want to see International or Local holidays in " ++ countryCodeInput ++" ?\nType Y for global and N for local -"
          inputGlobal <- getLine
          case inputGlobal of
            "Y"  -> do
              putStrLn  "\nInternational Holidays are : \n"
              names<- getLocalNames conn True
              putStrLn $ unlines names
            "y" -> do
              putStrLn  "\nInternational Holidays are : \n"
              names<- getLocalNames conn True
              putStrLn $ unlines names
            "N" -> do
              putStrLn  "\nLocal Holidays are : \n"
              names<- getLocalNames conn False
              putStrLn $ unlines names
            "n" -> do
              putStrLn  "\nLocal Holidays are : \n"
              names<- getLocalNames conn False
              putStrLn $ unlines names
            _ -> syntaxErrorForIsGlobal  
        _ -> syntaxErrorForOptions 
      putStrLn "\nPreparing to write into the JSON file... "
      putStrLn "\nWriting ..."
      jsonValue <- convertToJSON conn
      writeFile "DB.json" jsonValue
      putStrLn "\nFinished Writing!"
      putStrLn  "\nDo you want to exit the program?(Y/N)"
      inputContinue <- getLine
      case inputContinue of
        "N" -> do
          putStrLn  "\nReturning back to the program \n"
          main
        "n" -> do
          putStrLn  "\nReturning back to the program \n"
          main
        _ -> syntaxErrorForExit
      putStrLn  "\nExiting the program..."

-- | This function is used to catch the invalid inputs in Is Global input
syntaxErrorForIsGlobal :: IO ()
syntaxErrorForIsGlobal = putStrLn 
      "Invalid Input for the holiday type\n\
      \\n\
      \Please either enter Y or N next time\n"

-- | This function is used to catch the invalid inputs in Exit function
syntaxErrorForExit :: IO ()
syntaxErrorForExit = putStrLn 
      "\nRecognised a Exit case or Invalid Input."

-- | This function is used to catch the invalid inputs in Menu option function
syntaxErrorForOptions :: IO ()
syntaxErrorForOptions = putStrLn 
      "\nInvalid Menu Option."