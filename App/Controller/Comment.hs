{-# LANGUAGE OverloadedStrings #-}

module App.Controller.Comment where

import App.Json
import App.DB.Connect
import App.DB.Model
import App.DB.Model.Comment
import App.DB.Model.Snippet
import App.Types.Snippet
import App.Router
import Control.Monad.Trans.Class               ( lift )
import Database.HaskellDB

import qualified App.DB.Tables.Comments        as T
import qualified App.DB.Fields                 as F

-- GET /comment/:id
getComment :: Int -> ServR
getComment id = do
   lift $ print $ "GET /comment/" ++ show id
   res <- lift $ withDB $ \db -> query db $ dbGetComment id
   case null res of
      True  -> jsonErrorNotFound
      False -> jsonOkResponse (commentFactory $ head res)
   
-- POST /comment
postComment :: ServR
postComment = do
   lift $ print "POST /comment"
   post <- translatePostData
   dbRun post dbInsertComment
