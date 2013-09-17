{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module App.DB.Model.Comment where

import App.DB.Model
import App.Types.Comment
import Database.HaskellDB
import Database.HaskellDB.HDBRec
import Data.Time                               ( UTCTime(..), getCurrentTime )

import qualified App.DB.Tables.Snippets        as T
import qualified App.DB.Tables.Comments        as T
import qualified App.DB.Fields                 as F
import qualified Data.Text                     as Text

dbGetCommentsForSnippet id = do
   c <- table T.comments
   restrict ( c!F.entityId .==. constant id )
   project $ F.id          	  << c!F.id
           # F.userId         << c!F.userId
           # F.entityType     << c!F.entityType
           # F.entityId       << c!F.entityId
           # F.entityVersion  << c!F.entityVersion
           # F.created        << c!F.created
           # F.body           << c!F.body

----------------------------------- /~/ -----------------------------------

-- | Get comment by id
dbGetComment :: Int -> Query (Rel (RecCons F.Id               (Expr Int)
                                  (RecCons F.UserId           (Expr Int)
                                  (RecCons F.EntityType       (Expr String)
                                  (RecCons F.EntityId         (Expr Int)
                                  (RecCons F.EntityVersion    (Expr Int)
                                  (RecCons F.Created          (Expr String)
                                  (RecCons F.Body             (Expr Text.Text) RecNil))))))))
dbGetComment id = do
   c <- table T.comments
   restrict ( c!F.id .==. constant id )
   project $ F.id          	  << c!F.id
           # F.userId         << c!F.userId
           # F.entityType     << c!F.entityType
           # F.entityId       << c!F.entityId
           # F.entityVersion  << c!F.entityVersion
           # F.created        << c!F.created
           # F.body           << c!F.body

----------------------------------- /~/ -----------------------------------

-- | Insert a comment into the database
dbInsertComment :: Comment -> Database -> IO ()
dbInsertComment Comment{..} conn = do
   time <- getCurrentTime
   insert conn T.comments
      ( F.id             <<  _default
      # F.userId         <<- commentUserId 
      # F.entityType     <<- show commentEntityType
      # F.entityId       <<- commentEntityId
      # F.entityVersion  <<- commentEntityVersion
      # F.created        <<- show time
      # F.body           <<- commentBody )

----------------------------------- /~/ -----------------------------------

commentFactory :: (Select (Attr F.Id            Int)       r Int,
                   Select (Attr F.UserId        Int)       r Int,
                   Select (Attr F.EntityType    String)    r String,
                   Select (Attr F.EntityId      Int)       r Int,
                   Select (Attr F.EntityVersion Int)       r Int,
                   Select (Attr F.Created       String)    r String,
                   Select (Attr F.Body          Text.Text) r Text.Text) => r 
                -> Comment
commentFactory o =
   let entity = case o!F.entityType of
                   "remix" -> RemixEntity
                   _       -> SnippetEntity in 
   Comment { commentId            = o!F.id 
		   , commentUserId        = o!F.userId
		   , commentEntityType    = entity
		   , commentEntityId      = o!F.entityId
		   , commentEntityVersion = o!F.entityVersion
		   , commentCreated       = parseUTCTime $ o!F.created
		   , commentBody          = o!F.body
		   }
