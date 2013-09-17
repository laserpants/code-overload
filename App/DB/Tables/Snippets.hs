module App.DB.Tables.Snippets where

import App.DB.Fields
import Database.HaskellDB.DBLayout
import Data.Text

type Snippets = (RecCons Id               (Expr Int) 
                (RecCons CurrentVersion   (Expr Int) 
                (RecCons Created          (Expr Text) 
                (RecCons UserId           (Expr Int) 
                (RecCons Description      (Expr Text) RecNil)))))

snippets :: Table Snippets
snippets = baseTable "snippets" 
         $ hdbMakeEntry Id 
         # hdbMakeEntry CurrentVersion
         # hdbMakeEntry Created
         # hdbMakeEntry UserId
         # hdbMakeEntry Description
