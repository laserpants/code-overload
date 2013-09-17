module App.DB.Tables.Remixes where

import App.DB.Fields
import Database.HaskellDB.DBLayout
import Data.Text

type Remixes = (RecCons SnippetId (Expr Int) 
               (RecCons UserId (Expr Int) 
               (RecCons Body (Expr Text) RecNil)))

remixes :: Table Remixes
remixes = baseTable "remixes" 
        $ hdbMakeEntry SnippetId 
        # hdbMakeEntry UserId
        # hdbMakeEntry Body
