module App.DB.Tables.Comments where

import App.DB.Fields
import Database.HaskellDB.DBLayout
import Data.Text

type Comments = (RecCons Id            (Expr Int) 
                (RecCons UserId        (Expr Int) 
                (RecCons EntityType    (Expr String) 
                (RecCons EntityId      (Expr Int) 
                (RecCons EntityVersion (Expr Int) 
                (RecCons Created       (Expr String) 
                (RecCons Body          (Expr Text) RecNil)))))))

comments :: Table Comments
comments = baseTable "comments" 
         $ hdbMakeEntry Id 
         # hdbMakeEntry UserId 
         # hdbMakeEntry EntityType
         # hdbMakeEntry EntityId
         # hdbMakeEntry EntityVersion
         # hdbMakeEntry Created
         # hdbMakeEntry Body
