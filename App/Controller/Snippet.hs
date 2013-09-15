{-# LANGUAGE OverloadedStrings #-}

module App.Controller.Snippet where

import App.Json
import App.DB.Connect
import App.DB.Model
import App.DB.Model.Comment
import App.DB.Model.Snippet
import App.Types.Snippet
import App.Router
import Control.Monad.Trans.Class               ( lift )
import Database.HaskellDB

import qualified App.DB.Tables.Snippets        as T
import qualified App.DB.Tables.SnippetVersions as T
import qualified App.DB.Tables.Comments        as T
import qualified App.DB.Fields                 as F

-- GET /snippet/:id
getSnippet :: Int -> ServR
getSnippet id = do
   lift $ print $ "GET /snippet/" ++ show id
   res <- lift $ withDB $ \db -> query db $ dbGetSnippet id
   case null res of
      True  -> jsonErrorNotFound
      False -> do
         c <- lift $ withDB $ \db -> query db $ dbGetCommentsForSnippet id
         let comments = map commentFactory c
         jsonOkResponse (snippetFactory $ head res) { snippetComments = comments }

-- PUT /snippet/:id
putSnippet :: Int -> ServR
putSnippet id = do
   lift $ print $ "PUT /snippet/" ++ show id
   put <- translatePostData
   dbRun put fn
   where fn snippet db = do
         current <- query db $ dbGetCurrentSnippetVersion id
         case null current of
            True  -> return $ jsonBadRequest
            False -> do
               let vNumber = succ $ (head current)!F.currentVersion
                   version = SnippetVersion { versionSnippetId = id
                                            , versionNumber    = vNumber
                                            , versionBody      = versionBody $ snippetCurrentVersion snippet
                                            , versionCreated   = undefined
                                            }
               dbInsertSnippetVersion version db
               dbUpdateCurrentSnippetVersion id vNumber db
               return $ jsonOkDefault

-- DELETE /snippet/:id
deleteSnippet :: Int -> ServR
deleteSnippet id = do
   lift $ print $ "DELETE /snippet/" ++ show id
   lift $ withDB $ dbDeleteSnippet id 
   jsonOkDefault

-- POST /snippet
postSnippet :: ServR
postSnippet = do
   lift $ print "POST /snippet"
   post <- translatePostData
   dbRun post dbInsertSnippet

-- GET /snippet
getSnippets :: ServR
getSnippets = do
   lift $ print "GET /snippet"
   res <- lift $ withDB $ \db -> query db $ dbGetSnippetIndex
   let snippets = fmap simpleSnippetFactory res
   jsonOkResponse snippets
