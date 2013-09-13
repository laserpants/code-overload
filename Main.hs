{-# LANGUAGE RecordWildCards, OverloadedStrings, FlexibleContexts #-}

module Main where

import App.DB.Connect
import App.DB.Model
import App.DB.Model.Snippet
import App.DB.Model.Comment
import App.Types
import App.Types.Comment
import App.Types.Snippet
import App.Types.User
import Control.Monad                           ( mzero )
import Control.Monad.Trans.Class               ( lift )
import Database.HaskellDB
import Data.Aeson
import Data.Time.Clock

import qualified App.DB.Tables.Snippets        as T
import qualified App.DB.Tables.SnippetVersions as T
import qualified App.DB.Tables.Comments        as T
import qualified App.DB.Fields                 as F

main :: IO ()
main = do
   res <- withDB $ \db -> query db $ lastId T.snippets
   print res

{-

   let snippetId = 1
   comments <- withDB $ \db -> query db $ dbGetCommentsForSnippet snippetId

   res <- withDB $ \db -> query db $ dbGetSnippet snippetId

   let s = (snippetFactory $ head res) { snippetComments = map commentFactory comments }
   -}

   let s = Snippet {
                   }

   withDB $ \db -> dbInsertSnippet s db

   id <- withDB $ \db -> query db $ lastId T.snippets
   
   t <- getCurrentTime

   let sv = SnippetVersion { versionSnippetId = 1
  						   , versionNumber    = 1
						   , versionBody      = ""
						   , versionCreated   = t
						   }

   withDB $ \db -> dbInsertSnippetVersion sv db
   
   print $ encode s
-}
