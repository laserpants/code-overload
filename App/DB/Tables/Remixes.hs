module App.DB.Tables.Remixes where

import Database.HaskellDB.DBLayout
import App.DB.Fields

type Remixes = (RecCons SnippetId (Expr Int) 
               (RecCons UserId (Expr Int) 
               (RecCons Body (Expr String) RecNil)))

remixes :: Table Remixes
remixes = baseTable "remixes" 
        $ hdbMakeEntry SnippetId 
        # hdbMakeEntry UserId
        # hdbMakeEntry Body
