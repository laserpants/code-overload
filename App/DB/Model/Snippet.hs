{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module App.DB.Model.Snippet where

import App.DB.Model
import App.Types.Snippet
import Database.HaskellDB
import Data.Time                               ( UTCTime(..) )

import Database.HaskellDB.HDBRec

import qualified App.DB.Tables.Snippets        as T
import qualified App.DB.Tables.SnippetVersions as T
import qualified App.DB.Tables.Comments        as T
import qualified App.DB.Fields                 as F


dbGetSnippet :: Int -> Query (Rel (RecCons F.Id (Expr Int)
                                  (RecCons F.CurrentVersion (Expr Int)
                                  (RecCons F.Created (Expr [Char])
                                  (RecCons F.UserId (Expr Int)
                                  (RecCons F.Description (Expr [Char])
                                  (RecCons F.Body (Expr [Char]) RecNil)))))))
dbGetSnippet id = do
   s <- table T.snippets
   v <- table T.snippetVersions
   restrict ( s!F.id        .==. constant id 
         .&&. v!F.snippetId .==. constant id 
         .&&. v!F.version   .==. s!F.currentVersion )
   project $ F.id          	  << s!F.id
           # F.currentVersion << s!F.currentVersion
           # F.created     	  << s!F.created
           # F.userId     	  << s!F.userId
           # F.description    << s!F.description
           # F.body           << v!F.body

dbInsertSnippet Snippet{..} conn =
   insert conn T.snippets
      ( F.id             <<  _default
      # F.currentVersion <<- 1
      # F.created        <<- show snippetCreated
      # F.userId         <<- snippetUserId
      # F.description    <<- snippetDescription )

dbInsertSnippetVersion SnippetVersion{..} conn = 
   insert conn T.snippetVersions
      ( F.snippetId     <<- versionSnippetId
      # F.version       <<- versionNumber
      # F.body          <<- versionBody
      # F.created       <<- show versionCreated )

snippetFactory :: (Select (Attr F.Id Int) r Int, 
                   Select (Attr F.CurrentVersion Int) r Int,
                   Select (Attr F.Created String) r String,
                   Select (Attr F.UserId Int) r Int,
                   Select (Attr F.Description String) r String) => r
                -> Snippet
snippetFactory o = Snippet 
   { snippetId               = o!F.id
   , snippetCurrentVersion   = o!F.currentVersion
   , snippetCreated          = parseUTCTime $ o!F.created 
   , snippetUserId           = o!F.userId
   , snippetDescription      = o!F.description
   , snippetComments         = []
   }
