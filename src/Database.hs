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
  ( HolidayRecord (countryCode, date, global, localName, name),
  )

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
    \ name VARCHAR(40) NOT NULL, \
    \ global BOOL DEFAULT NULL \
    \)"
    []
  commit conn
  run
    conn
    "CREATE TABLE IF NOT EXISTS countries (\
    \ countryCode VARCHAR(40) NOT NULL \
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
  stmt <- prepare conn "INSERT INTO holidays (date,localName,name,global) VALUES (?,?,?,?)"
  putStrLn "Adding"
  -- let xs'' = mapM_ (\x -> putStrLn $ " - " ++ x) xs'
  executeMany stmt (map (\x -> [toSql (date x), toSql (localName x), toSql (name x), toSql (global x)]) xs)
  commit conn

-- This function will select all the holidays of a given country
queryDB :: Connection -> String -> IO [[SqlValue]]
queryDB conn countryCode = do
  res <- quickQuery' conn "SELECT localName FROM country_holidays WHERE counryCode =(?)" [toSql countryCode]
  return res

--This function will select all the holidays in the date specified of a given country
querySQ :: Connection -> String -> IO Bool
querySQ conn date = do
  res <- quickQuery' conn "SELECT localName FROM holidays WHERE date BETWEEN'1-JAN-20'AND'31-JUL-20'"[toSql date]
  return (length res == 0)

-- This function will call all the names on the database.
getNAMEs :: Connection -> IO [String]
getNAMEs conn = do
  res <- quickQuery' conn "SELECT name FROM holidays" []
  return $ map fromSql (map head res)


recordToSqlValues :: HolidayRecord -> [SqlValue]
recordToSqlValues holidays =
  [ toSql $ date holidays,
    toSql $ localName holidays,
    toSql $ name holidays,
    toSql $ global holidays
  ]

holidayToSqlValues :: HolidayRecord -> [SqlValue]
holidayToSqlValues countries =
  [ toSql $ countryCode countries
  ]

prepareInsertRecordStmt :: Connection -> IO Statement
prepareInsertRecordStmt conn = prepare conn "INSERT INTO holidays VALUES (?,?)"
{- 
prepareInsertHolidayStmt conn = prepare conn "INSERT INTO holiday VALUES (?,?,?)"

prepareInsertGlobalStmt conn = prepare conn "INSERT INTO country_holidays VALUES (?,?,?)"
-}
saveHolidayRecord :: [HolidayRecord] -> Connection -> IO ()
saveHolidayRecord records conn = do
  stmt <- prepareInsertRecordStmt conn
  executeMany stmt (map recordToSqlValues records)
  commit conn

dateNotInDB = undefined  

nub = undefined