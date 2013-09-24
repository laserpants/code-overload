{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings #-}

module App.DB.Model where

import Database.HaskellDB
import Database.HaskellDB.Database 
import Database.HaskellDB.HDBRec
import Database.HaskellDB.PrimQuery
import Database.HaskellDB.Query
import Data.Time                               ( UTCTime(..) )
import Data.Time.Clock.POSIX
import Data.Text

import qualified App.DB.Fields                 as F

parseUTCTime :: String -> UTCTime
parseUTCTime = posixSecondsToUTCTime . fromIntegral . read 

parseUTCTimeText :: Text -> UTCTime
parseUTCTimeText = parseUTCTime . unpack

lastId :: (ShowRecRow r, HasField F.Id r) => Table r -> Query (Rel (RecCons F.Id (Expr Int) RecNil))
lastId tbl = do
   s <- table tbl
   project $ F.id << _max (s!F.id)

----------------------------------- /~/ -----------------------------------

getText g fs s f = do
   m <- getValue fs s f
   case m of
      Nothing -> fail $ "Got NULL value from non-NULL field " ++ f
      Just v  -> return $ g v

instance GetValue Text where
   getValue = getText id  

instance GetValue (Maybe Text) where
   getValue = getText (Just . pack) 

instance ShowConstant Text where
   showConstant = StringLit . unpack
