module App.DB.Tables.Comments where

import Database.HaskellDB.DBLayout
import App.DB.Fields

type Comments = (RecCons Id (Expr Int) 
                (RecCons UserId (Expr Int) 
                (RecCons EntityType (Expr String) 
                (RecCons EntityId (Expr Int) 
                (RecCons EntityVersion (Expr Int) 
                (RecCons Created (Expr String) 
                (RecCons Body (Expr String) RecNil)))))))

comments :: Table Comments
comments = baseTable "comments" 
         $ hdbMakeEntry Id 
         # hdbMakeEntry UserId 
         # hdbMakeEntry EntityType
         # hdbMakeEntry EntityId
         # hdbMakeEntry EntityVersion
         # hdbMakeEntry Created
         # hdbMakeEntry Body
