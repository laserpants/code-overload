module App.DB.Tables.SnippetVersions where

import App.DB.Fields
import Database.HaskellDB.DBLayout
import Data.Text

type SnippetVersions = (RecCons SnippetId      (Expr Int) 
                       (RecCons Version        (Expr Int) 
                       (RecCons Body           (Expr Text) 
                       (RecCons VersionCreated (Expr Text) RecNil))))

snippetVersions :: Table SnippetVersions
snippetVersions = baseTable "snippet_versions" 
                $ hdbMakeEntry SnippetId 
                # hdbMakeEntry Version
                # hdbMakeEntry Body
                # hdbMakeEntry VersionCreated
