{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module App.DB.Model.Comment where

import App.DB.Model
import App.Types.Comment
import Database.HaskellDB
import Database.HaskellDB.HDBRec
import Data.Time                               ( UTCTime(..), getCurrentTime )
import Data.Text 

import qualified App.DB.Tables.Snippets        as T
import qualified App.DB.Tables.Comments        as T
import qualified App.DB.Fields                 as F

dbGetCommentsForSnippet id = do
   c <- table T.comments
   restrict ( c!F.snippetId .==. constant id )
   project $ F.id          	  << c!F.id
           # F.userId         << c!F.userId
           # F.snippetId      << c!F.snippetId
           # F.snippetVersion << c!F.snippetVersion
           # F.created        << c!F.created
           # F.body           << c!F.body

----------------------------------- /~/ -----------------------------------

-- | Get comment by id
dbGetComment :: Int -> Query (Rel (RecCons F.Id               (Expr Int)
                                  (RecCons F.UserId           (Expr Int)
                                  (RecCons F.SnippetId        (Expr Int)
                                  (RecCons F.SnippetVersion   (Expr Int)
                                  (RecCons F.Created          (Expr Text)
                                  (RecCons F.Body             (Expr Text) RecNil)))))))
dbGetComment id = do
   c <- table T.comments
   restrict ( c!F.id .==. constant id )
   project $ F.id          	  << c!F.id
           # F.userId         << c!F.userId
           # F.snippetId      << c!F.snippetId
           # F.snippetVersion << c!F.snippetVersion
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
      # F.snippetId      <<- commentSnippetId
      # F.snippetVersion <<- commentSnippetVersion
      # F.created        <<- pack (show time)
      # F.body           <<- commentBody 
      )

----------------------------------- /~/ -----------------------------------

commentFactory :: (Select (Attr F.Id             Int)   r Int,
                   Select (Attr F.UserId         Int)   r Int,
                   Select (Attr F.SnippetId      Int)   r Int,
                   Select (Attr F.SnippetVersion Int)   r Int,
                   Select (Attr F.Created        Text)  r Text,
                   Select (Attr F.Body           Text)  r Text) => r 
                -> Comment
commentFactory o =
   Comment { commentId             = o!F.id 
		   , commentUserId         = o!F.userId
		   , commentSnippetId      = o!F.snippetId
		   , commentSnippetVersion = o!F.snippetVersion
		   , commentCreated        = parseUTCTimeText $ o!F.created
		   , commentBody           = o!F.body
		   }
