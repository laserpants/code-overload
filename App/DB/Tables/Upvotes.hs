module App.DB.Tables.Upvotes where

import Database.HaskellDB.DBLayout
import App.DB.Fields

type Upvotes = (RecCons UserId    (Expr Int) 
               (RecCons SnippetId (Expr Int) RecNil))

upvotes :: Table Upvotes
upvotes = baseTable "upvotes" 
        $ hdbMakeEntry UserId
        # hdbMakeEntry SnippetId
