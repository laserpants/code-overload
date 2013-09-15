module App.DB.Tables.SnippetVersions where

import Database.HaskellDB.DBLayout
import App.DB.Fields

type SnippetVersions = (RecCons SnippetId (Expr Int) 
                       (RecCons Version (Expr Int) 
                       (RecCons Body (Expr String) 
                       (RecCons VersionCreated (Expr String) RecNil))))

snippetVersions :: Table SnippetVersions
snippetVersions = baseTable "snippet_versions" 
                $ hdbMakeEntry SnippetId 
                # hdbMakeEntry Version
                # hdbMakeEntry Body
                # hdbMakeEntry VersionCreated
