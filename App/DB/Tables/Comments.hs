module App.DB.Tables.Comments where

import App.DB.Fields
import Database.HaskellDB.DBLayout
import Data.Text

type Comments = (RecCons Id             (Expr Int) 
                (RecCons UserId         (Expr Int) 
                (RecCons SnippetId      (Expr Int) 
                (RecCons SnippetVersion (Expr Int) 
                (RecCons Created        (Expr Text) 
                (RecCons Body           (Expr Text) RecNil))))))

comments :: Table Comments
comments = baseTable "comments" 
         $ hdbMakeEntry Id 
         # hdbMakeEntry UserId 
         # hdbMakeEntry SnippetId
         # hdbMakeEntry SnippetVersion
         # hdbMakeEntry Created
         # hdbMakeEntry Body
