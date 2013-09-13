module App.DB.Connect where

import Database.HaskellDB.Database             ( Database )
import Database.HaskellDB.HDBC                 ( hdbcConnect )
import Database.HaskellDB.Sql.SQLite           ( generator )
import Database.HDBC.Sqlite3                   ( connectSqlite3 )

withDB :: (Database -> IO a) -> IO a
withDB = hdbcConnect generator (connectSqlite3 "db.sqlite")
