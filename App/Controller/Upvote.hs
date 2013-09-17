module App.Controller.Upvote where

import App.Json
import App.DB.Connect
import App.DB.Model.Upvote
import App.Types.Upvote
import App.Router
import Control.Monad.Trans.Class
import Database.HaskellDB
import Happstack.Server

import qualified App.DB.Tables.Upvotes         as T
import qualified App.DB.Fields                 as F

-- POST /upvote
postUpvote :: ServR
postUpvote = do
   post <- translatePostData
   case post of
      Nothing     -> jsonBadRequest
      Just upvote -> do
         upvoteExists >>= \e -> if e then jsonBadRequest else dbRun post dbInsertUpvote
         where upvoteExists = do 
               v <- lift $ withDB $ \db -> query db $ dbGetUpvote upvote db
               return $ not $ null v
