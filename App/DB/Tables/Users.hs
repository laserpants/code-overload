module App.DB.Tables.Users where

import Database.HaskellDB.DBLayout
import App.DB.Fields

type Users = (RecCons Id (Expr Int) 
             (RecCons Email (Expr String) RecNil))

users :: Table Users
users = baseTable "users" 
      $ hdbMakeEntry Id 
      # hdbMakeEntry Email
