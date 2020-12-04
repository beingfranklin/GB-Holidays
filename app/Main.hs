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
    case parse json of
          Left err -> print err
          -- Print first record
          Right recs -> do 
                    print recs
                    -- Print 5 records
                    -- Right recs -> print . (take 5) $ (HolidayRecord recs)
                    putStrLn "Saving on DB"
                    putStrLn "\n****************"
                    conn <- initialiseDB
                    insertDB conn recs 
                    insertSB conn recs
                    insertLB conn recs  
                    putStrLn "Done!"
                    putStrLn "\n****************"
                    res <- queryDB conn "GB"
                    putStrLn "\n****************"
                    putStrLn "Before Formatting Query"
                    putStr $ show res
                    putStrLn "\n****************"
                    putStrLn "After Formatting Query"
                    let converRes = sqlRowToString res
                    mapM_ putStrLn converRes
                    putStrLn "\n****************"
                    putStrLn $ "Number of rows is " ++ show (length res)

                    

