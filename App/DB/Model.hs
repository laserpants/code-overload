{-# LANGUAGE FlexibleContexts #-}

module App.DB.Model where

import Database.HaskellDB
import Database.HaskellDB.HDBRec
import Data.Time                               ( UTCTime(..) )
import Data.Time.Clock.POSIX

import qualified App.DB.Fields                 as F

parseUTCTime :: String -> UTCTime
parseUTCTime time = posixSecondsToUTCTime $ fromIntegral $ read time

lastId :: (ShowRecRow r, HasField F.Id r) => Table r -> Query (Rel (RecCons F.Id (Expr Int) RecNil))
lastId tbl = do
   s <- table tbl
   project $ F.id << _max (s!F.id)
