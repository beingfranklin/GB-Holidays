-- |Database module header
module Database where

import Database.HDBC
import qualified Data.ByteString.Lazy.Char8 as L8
import Database.HDBC.Sqlite3
import Parse
import Data.Aeson

{--| 
  Database.HDBC - Database.HDBC is an abstraction layer between Haskell programs 
  parse - Imported parse library to make use of the record data type  
  Data.Aeson -  
  Database.HDBC.Sqlite3 - -}

-- | This is a function is for initialising the database and to create the table if the tables do not exists yet

initialiseDB :: IO Connection
initialiseDB = do
    {--| ConnectSqlite3 takes a filePath and that performs as an IO action  
      and then it returns a connection between that database file (conn)
      The run function runs any statement that we have prepared as String 
      it takes a connection, statement and some parameters 
      then it will perform that action. -}
  conn <- connectSqlite3 "HolidayRecord.sqlite"
  run
    conn
    {--| Sql statement is created 3 tables 
    holidays table has 4 fields
     three of them are String and one is Integer -}
    "CREATE TABLE IF NOT EXISTS holidays (\
    \ id INTEGER PRIMARY KEY NOT NULL,\
    \ date VARCHAR(40) NOT NULL, \
    \ localName VARCHAR(40) NOT NULL, \
    \ name VARCHAR(40) NOT NULL \
    \)"
    []
  commit conn
  run
    conn
    {--| The second table is countries which has two Boolean fields, one a String field and one an Integer field -}
    "CREATE TABLE IF NOT EXISTS countries (\
    \ id INTEGER PRIMARY KEY NOT NULL,\
    \ countryCode VARCHAR(40) NOT NULL, \
    \ global BOOL DEFAULT NULL, \
    \ fixed BOOL DEFAULT NULL \
    \)"
    []
  commit conn
  run
    conn
    {--| The third table is country_holidays which has 2 Srting fields and one an Integer field.  -}
    "CREATE TABLE IF NOT EXISTS country_holidays (\
    \ id INTEGER PRIMARY KEY NOT NULL,\
    \ countryCode VARCHAR(40) DEFAULT NULL,\
    \ localName VARCHAR(40) DEFAULT NULL \
    \)"
    []
  commit conn
  return conn


-- | This function will insert the holiday records into the database 
insertDB :: Connection -> [HolidayRecord] -> IO () 
insertDB conn records = do
  let xs = records 
  stmt <- prepare conn "INSERT INTO holidays (date,localName,name) VALUES (?,?,?)" --  this fuction use to insert the values into the parameters under the holidays table
  putStrLn "Adding" --  this fuction to Type for s string with newline character at the end 
  executeMany stmt (map (\x -> [toSql (date x), toSql (localName x), toSql (name x)]) xs) --  this function use for prepares a database operation and executes it against the parameters sequences
  commit conn --  this fuction to commit a transaction to the underlying database 

-- | This function will insert the country records into the dsatabase
insertLB :: Connection -> [HolidayRecord] -> IO () 
insertLB conn records = do
  let xs = records
  stmt <- prepare conn "INSERT INTO countries (countryCode,global,fixed) VALUES (?,?,?)" --  this fuction use to insert the values into the parameters under the country table
  putStrLn "Adding" --  this fuction to Type for s string with newline character at the end 
  executeMany stmt (map (\x -> [toSql (countryCode x), toSql (global x), toSql (fixed x)]) xs) --  this function use for prepares a database operation and executes it against the parameters sequences
  commit conn -- this fuction to commit a transaction to the underlying database

-- | This function will insert the country_holidays records into the dsatabase
insertSB :: Connection -> [HolidayRecord] -> IO ()
insertSB conn records = do
  let xs = records
  stmt <- prepare conn "INSERT INTO country_holidays (countryCode,localName) VALUES (?,?)" -- this fuction use to insert the values into the parameters under the country_holidays
  putStrLn "Adding" --  this fuction to Type for s string with newline character at the end 
  executeMany stmt (map (\x -> [toSql (countryCode x), toSql (localName x)]) xs) --  this function use for prepares a database operation and executes it against the parameters sequences
  commit conn --  this fuction to commit a transaction to the underlying database

-- | This function will select all the holidays of a given country
queryDB :: Connection -> String -> IO [[SqlValue]]
queryDB conn countryCode =
  do
    quickQuery' --  A quick way to do a query similar to preparing, executing.
      conn
      "SELECT localName FROM country_holidays WHERE countryCode =(?)" --  this function will select specific parameter values 
      [toSql countryCode]

-- | This function will select all the holidays in the date specified of a given country
selectHolidaysInDateRange :: Connection -> String -> String -> IO [String] 
selectHolidaysInDateRange conn startDate endDate = do
  res <- quickQuery' conn "SELECT localName FROM holidays WHERE date BETWEEN (?) AND (?)" [toSql startDate, toSql endDate] --  this function will select specific parameter values which was here specified between tow dates
  return $ map fromSql $ concat res --  this function will return the selection result

-- | This function will call all the names on the database.
getNames :: Connection -> IO [String]
getNames conn = do
  res <- quickQuery' conn "SELECT name FROM holidays" [] -- this function will select specific parameter values which are names
  return $ map fromSql $ concat res --  this function will return the selection result

-- | This function will call all the names on the database.
getLocalNames :: Connection -> Bool -> IO [String]
getLocalNames conn isGlobal = do
  res <-
    quickQuery'
      conn
      "SELECT country_holidays.localName FROM country_holidays \
      \INNER JOIN countries \
      \ON countries.id = country_holidays.id \
      \WHERE countries.global=(?)"
      [toSql isGlobal]
  return $ map fromSql $ concat res

-- | This function is used to convert from Haskell datatypes to SQL datatypes toSql function used from HDBC to do the convert for all values to SQL in the database
recordToSqlValues :: HolidayRecord -> [SqlValue]
recordToSqlValues holidays =
  [ toSql $ date holidays,
    toSql $ localName holidays,
    toSql $ name holidays
  ]

-- | This function is used to convert from Haskell datatypes to SQL datatypes toSql function used from HDBC to do the convert for all values to SQL in the database
holidayToSqlValues :: HolidayRecord -> [SqlValue]
holidayToSqlValues countries =
  [ toSql $ countryCode countries,
    toSql $ global countries,
    toSql $ fixed countries
  ]

-- |This function performs an action (ie. to insert the values into holidays)

prepareInsertRecordStmt :: Connection -> IO Statement
prepareInsertRecordStmt conn = prepare conn "INSERT INTO holidays VALUES (?,?)"

-- |This function performs an action (ie. to fetch the values from holidays)

prepareSelectRecordStma :: Connection -> IO Statement
prepareSelectRecordStma conn = prepare conn "SELECT FROM Country_holidays VALUES (?,?)"

-- |Thus function for saving all the records into the database passing the executeMany function in HDBC library  then execute a list of lists of values and it preforms an action which dose not return anything it just preforms that sql statement
saveHolidayRecord :: [HolidayRecord] -> Connection -> IO ()
saveHolidayRecord records conn = do
  stmt <- prepareInsertRecordStmt conn
  executeMany stmt (map recordToSqlValues records)
  commit conn

-- | This function saves the country record
savecountriesRecord :: [HolidayRecord] -> Connection -> IO ()
savecountriesRecord record conn = do
  stma <- prepareSelectRecordStma conn
  executeMany stma (map recordToSqlValues record)
  commit conn

-- | This function converts into Row into the string

sqlRowToString :: [[SqlValue]] -> [String]
sqlRowToString xs = map (fromSql :: SqlValue -> String) (concat xs)

-- | This function is to retrieve all the SQLs on the database
getUnprocessedSQLHolidays :: Connection -> IO [HolidayRecord]
getUnprocessedSQLHolidays conn = do
  res <-
    quickQuery'
      conn
      "SELECT holidays.date, holidays.localName, holidays.name, countries.countryCode, countries.global, countries.fixed FROM holidays \
      \INNER JOIN countries \
      \WHERE countries.id=holidays.id \
      \ORDER BY countries.id ASC"
      []
  return $ map (\xs -> HolidayRecord(fromSql (xs !! 0)) (fromSql (xs !! 1)) (fromSql (xs !! 2)) (fromSql (xs !! 3)) (fromSql (xs !! 4)) (fromSql (xs !! 5))) res

-- | This function converts SQL into JSON
convertToJSON :: Connection -> IO String
convertToJSON conn = do
  res <- getUnprocessedSQLHolidays conn
  return $ L8.unpack ( encode res)