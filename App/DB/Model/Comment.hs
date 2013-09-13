{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module App.DB.Model.Comment where

import App.DB.Model
import App.Types.Comment
import Database.HaskellDB
import Data.Time                               ( UTCTime(..) )

import qualified App.DB.Tables.Snippets        as T
import qualified App.DB.Tables.Comments        as T
import qualified App.DB.Fields                 as F

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

commentFactory :: (Select (Attr F.Id Int) r Int,
                   Select (Attr F.UserId Int) r Int,
                   Select (Attr F.EntityType String) r String,
                   Select (Attr F.EntityId Int) r Int,
                   Select (Attr F.EntityVersion Int) r Int,
                   Select (Attr F.Created String) r String,
                   Select (Attr F.Body String) r String) => r 
                -> Comment
commentFactory o =
   let t = case o!F.entityType of
              "remix" -> RemixEntity
              _       -> SnippetEntity in 
   Comment { commentId            = o!F.id 
		   , commentUserId        = o!F.userId
		   , commentEntityType    = t
		   , commentEntityId      = o!F.entityId
		   , commentEntityVersion = o!F.entityVersion
		   , commentCreated       = parseUTCTime $ o!F.created
		   , commentBody          = o!F.body
		   }
