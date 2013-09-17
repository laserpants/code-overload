module App.DB.Tables.Snippets where

import Database.HaskellDB.DBLayout
import App.DB.Fields

type Snippets = (RecCons Id               (Expr Int) 
                (RecCons CurrentVersion   (Expr Int) 
                (RecCons Created          (Expr String) 
                (RecCons UserId           (Expr Int) 
                (RecCons Description      (Expr String) RecNil)))))

snippets :: Table Snippets
snippets = baseTable "snippets" 
         $ hdbMakeEntry Id 
         # hdbMakeEntry CurrentVersion
         # hdbMakeEntry Created
         # hdbMakeEntry UserId
         # hdbMakeEntry Description
