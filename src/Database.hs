module Database where

--{initialiseDB,
--saveRecords
--}

import Database.HDBC
  ( IConnection (commit, prepare, run),
    SqlValue,
    Statement (executeMany),
    fromSql,
    quickQuery',
    toSql,
  )
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import Parse

-- This is a function that creates the tables
-- if the tables do not exists yet
initialiseDB :: IO Connection
initialiseDB = do
  conn <- connectSqlite3 "HolidayRecord.sqlite"
  run
    conn
    "CREATE TABLE IF NOT EXISTS holidays (\
    \ date VARCHAR(40) NOT NULL, \
    \ localName VARCHAR(40) NOT NULL, \
    \ name VARCHAR(40) NOT NULL \
    \)"
    []
  commit conn
  run
    conn
    "CREATE TABLE IF NOT EXISTS countries (\
    \ countryCode VARCHAR(40) NOT NULL, \
    \ global BOOL DEFAULT NULL \
    \)"
    []
  commit conn
  run
    conn
    "CREATE TABLE IF NOT EXISTS country_holidays (\
    \ countryCode VARCHAR(40) DEFAULT NULL,\
    \ localName VARCHAR(40) DEFAULT NULL \
    \)"
    []
  commit conn
  return conn

-- storeHolidays :: Connection
--              -> [date] -- ^ List of date to be stored on the database
--              -> IO ()
-- storeHolidays _ [] = return ()
-- storeHolidays conn xs = do

-- This function will insert the holiday records into the database
insertDB :: Connection -> [HolidayRecord] -> IO ()
insertDB conn records = do
  let xs = records -- need to use records and produce xs, this seems easiest possibility
  -- xs' <- filter (dateNotInDB conn) (nub xs)
  stmt <- prepare conn "INSERT INTO holidays (date,localName,name) VALUES (?,?,?)"
  putStrLn "Adding"
  -- let xs'' = mapM_ (\x -> putStrLn $ " - " ++ x) xs'
  executeMany stmt (map (\x -> [toSql (date x), toSql (localName x), toSql (name x)]) xs)
  commit conn

-- This function will insert the country records into the dsatabase
insertLB :: Connection -> [HolidayRecord] -> IO ()
insertLB conn records = do
  let xs = records
  stmt <- prepare conn "INSERT INTO countries (countryCode,global) VALUES (?,?)"
  putStrLn "Adding"
  executeMany stmt (map (\x -> [toSql (countryCode x), toSql (global x)]) xs)
  commit conn

-- This function will insert the country_holidays records into the dsatabase
insertSB :: Connection -> [HolidayRecord] -> IO ()
insertSB conn records = do
  let xs = records
  stmt <- prepare conn "INSERT INTO country_holidays (countryCode,localName) VALUES (?,?)"
  putStrLn "Adding"
  executeMany stmt (map (\x -> [toSql (countryCode x), toSql (localName x)]) xs)
  commit conn

-- This function will select all the holidays of a given country
queryDB :: Connection -> String -> IO [[SqlValue]]
queryDB conn countryCode =
  do
    quickQuery'
      conn
      "SELECT localName FROM country_holidays WHERE countryCode =(?)"
      [toSql countryCode]

--This function will select all the holidays in the date specified of a given country
selectHolidaysInDateRange :: Connection -> String -> String -> IO [String]
selectHolidaysInDateRange conn startDate endDate = do
  --let d1 = "31-JUL-20"
  --let d2 = "1-JAN-20"
  res <- quickQuery' conn "SELECT localName FROM holidays WHERE date BETWEEN (?) AND (?)" [toSql startDate, toSql endDate]
  return $ map fromSql $ concat $ res

-- This function will call all the names on the database.
getNAMEs :: Connection -> IO [String]
getNAMEs conn = do
  res <- quickQuery' conn "SELECT name FROM holidays" []
  -- return $ map fromSql (map head res)
  return $ map (fromSql . head) res

recordToSqlValues :: HolidayRecord -> [SqlValue]
recordToSqlValues holidays =
  [ toSql $ date holidays,
    toSql $ localName holidays,
    toSql $ name holidays
  ]

holidayToSqlValues :: HolidayRecord -> [SqlValue]
holidayToSqlValues countries =
  [ toSql $ countryCode countries,
    toSql $ global countries
  ]

prepareInsertRecordStmt :: Connection -> IO Statement
prepareInsertRecordStmt conn = prepare conn "INSERT INTO holidays VALUES (?,?)"

prepareSelectRecordStma :: Connection -> IO Statement
prepareSelectRecordStma conn = prepare conn "SELECT FROM Country_holidays VALUES (?,?)"

saveHolidayRecord :: [HolidayRecord] -> Connection -> IO ()
saveHolidayRecord records conn = do
  stmt <- prepareInsertRecordStmt conn
  executeMany stmt (map recordToSqlValues records)
  commit conn

savecountriesRecord :: [HolidayRecord] -> Connection -> IO ()
savecountriesRecord record conn = do
  stma <- prepareSelectRecordStma conn
  executeMany stma (map recordToSqlValues record)
  commit conn

-- sqlRowToLine :: [[SqlValue]] ->  String
-- sqlRowToLine row = 
--   map (fromSql :: SqlValue -> String)

sqlRowToString :: [[SqlValue]] ->  [String]
sqlRowToString xs = map (fromSql :: SqlValue -> String) (concat xs)

-- |Method to retrieve all the SQLs on the database.
getUnprocessedSQLHolidays :: Connection -> IO [(String, String, String)]
getUnprocessedSQLHolidays conn = do
   res <- quickQuery' conn "SELECT date, localName, name FROM holidays" []
   return $ map ( \xs -> ( fromSql (xs!!0), fromSql (xs!!1), fromSql (xs!!2) )) res

getUnprocessedSQLCountries :: Connection -> IO [(String, Bool)]
getUnprocessedSQLCountries conn = do
   res <- quickQuery' conn "SELECT countryCode, global FROM countries" []
   return $ map ( \xs -> ( fromSql (xs!!0), fromSql (xs!!1) )) res

getUnprocessedSQLCH :: Connection -> IO [(String, String)]
getUnprocessedSQLCH conn = do
   res <- quickQuery' conn "SELECT countryCode, localName FROM country_holidays" []
   return $ map ( \xs -> ( fromSql (xs!!0), fromSql (xs!!1) )) res


