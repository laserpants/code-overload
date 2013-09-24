{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module App.DB.Model.Upvote where

import App.DB.Model
import App.Types.Upvote
import Database.HaskellDB
import Database.HaskellDB.HDBRec

import qualified App.DB.Tables.Upvotes         as T
import qualified App.DB.Fields                 as F

dbInsertUpvote :: Upvote -> Database -> IO ()
dbInsertUpvote Upvote{..} conn =
   insert conn T.upvotes
      ( F.userId    <<- upvoteUserId
      # F.snippetId <<- upvoteSnippetId )

dbGetUpvote Upvote{..} conn = do
   u <- table T.upvotes
   restrict ( u!F.userId    .==. constant upvoteUserId 
         .&&. u!F.snippetId .==. constant upvoteSnippetId )
   project $ F.userId    << u!F.userId
           # F.snippetId << u!F.snippetId
