module App.DB.Tables.Users where

import App.DB.Fields
import Database.HaskellDB.DBLayout
import Data.Text

type Users = (RecCons Id    (Expr Int) 
             (RecCons Email (Expr Text) RecNil))

users :: Table Users
users = baseTable "users" 
      $ hdbMakeEntry Id 
      # hdbMakeEntry Email
