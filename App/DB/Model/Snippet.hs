{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module App.DB.Model.Snippet where

import App.DB.Model
import App.Types.Snippet
import Database.HaskellDB
import Database.HaskellDB.HDBRec
import Data.Time                               ( UTCTime(..), getCurrentTime )
import Data.Text

import qualified App.DB.Tables.Snippets        as T
import qualified App.DB.Tables.SnippetVersions as T
import qualified App.DB.Tables.Comments        as T
import qualified App.DB.Tables.Upvotes         as T
import qualified App.DB.Fields                 as F

-- | Fetch all root (parentId = 0) snippets from the database
dbGetSnippetIndex :: Query (Rel (RecCons F.Id               (Expr Int)
                                (RecCons F.ParentId         (Expr Int)
                                (RecCons F.CurrentVersion   (Expr Int)
                                (RecCons F.Created          (Expr Text)
                                (RecCons F.UserId           (Expr Int)
                                (RecCons F.Description      (Expr Text) RecNil)))))))
dbGetSnippetIndex = do
   s <- table T.snippets
   restrict $ s!F.parentId .==. constant 0 
   project $ F.id          	  << s!F.id
           # F.parentId       << s!F.parentId
           # F.currentVersion << s!F.currentVersion
           # F.created     	  << s!F.created
           # F.userId     	  << s!F.userId
           # F.description    << s!F.description

----------------------------------- /~/ -----------------------------------

dbGetCurrentSnippetVersion :: Int -> Query (Rel (RecCons F.CurrentVersion (Expr Int) RecNil))
dbGetCurrentSnippetVersion id = do
   s <- table T.snippets
   restrict $ s!F.id .==. constant id 
   project $ F.currentVersion << s!F.currentVersion

dbUpdateCurrentSnippetVersion :: Int -> Int -> Database -> IO ()
dbUpdateCurrentSnippetVersion id newVersion conn = do
   update conn T.snippets
      ( \content -> content!F.id .==. constant id )
      ( \content -> F.currentVersion << constant newVersion )

----------------------------------- /~/ -----------------------------------

-- | Get snippet by id
dbGetSnippet :: Int -> Query (Rel (RecCons F.Id               (Expr Int)
                                  (RecCons F.ParentId         (Expr Int)
                                  (RecCons F.CurrentVersion   (Expr Int)
                                  (RecCons F.Created          (Expr Text)
                                  (RecCons F.UserId           (Expr Int)
                                  (RecCons F.Description      (Expr Text)
                                  (RecCons F.Body             (Expr Text)
                                  (RecCons F.VersionCreated   (Expr Text) RecNil)))))))))
dbGetSnippet id = do
   s <- table T.snippets
   v <- table T.snippetVersions
   restrict ( s!F.id        .==. constant id 
         .&&. v!F.snippetId .==. constant id 
         .&&. v!F.version   .==. s!F.currentVersion )
   project $ F.id          	  << s!F.id
           # F.parentId       << s!F.parentId
           # F.currentVersion << s!F.currentVersion
           # F.created     	  << s!F.created
           # F.userId     	  << s!F.userId
           # F.description    << s!F.description
           # F.body           << v!F.body
           # F.versionCreated << v!F.versionCreated

----------------------------------- /~/ -----------------------------------

-- | Get all children for a given snippet
dbGetChildren :: Int -> Query (Rel (RecCons F.Id               (Expr Int)
                                   (RecCons F.ParentId         (Expr Int)
                                   (RecCons F.CurrentVersion   (Expr Int)
                                   (RecCons F.Created          (Expr Text)
                                   (RecCons F.UserId           (Expr Int)
                                   (RecCons F.Description      (Expr Text)
                                   (RecCons F.Body             (Expr Text)
                                   (RecCons F.VersionCreated   (Expr Text) RecNil)))))))))

dbGetChildren parent = do
   s <- table T.snippets
   v <- table T.snippetVersions
   restrict ( s!F.parentId  .==. constant parent 
         .&&. v!F.snippetId .==. s!F.id
         .&&. v!F.version   .==. s!F.currentVersion )
   project $ F.id          	  << s!F.id
           # F.parentId       << s!F.parentId
           # F.currentVersion << s!F.currentVersion
           # F.created     	  << s!F.created
           # F.userId     	  << s!F.userId
           # F.description    << s!F.description
           # F.body           << v!F.body
           # F.versionCreated << v!F.versionCreated

----------------------------------- /~/ -----------------------------------

-- | Insert a snippet into the database
dbInsertSnippet :: Snippet -> Database -> IO ()
dbInsertSnippet Snippet{..} conn = do
   time <- getCurrentTime
   insert conn T.snippets
      ( F.id             <<  _default
      # F.parentId       <<- snippetParentId
      # F.currentVersion <<- 1
      # F.created        <<- pack (show time)
      # F.userId         <<- snippetUserId
      # F.description    <<- snippetDescription 
      )

dbInsertSnippetVersion :: SnippetVersion -> Database -> IO ()
dbInsertSnippetVersion SnippetVersion{..} conn = do
   time <- getCurrentTime
   insert conn T.snippetVersions
      ( F.snippetId      <<- versionSnippetId
      # F.version        <<- versionNumber
      # F.body           <<- versionBody
      # F.versionCreated <<- pack (show time) 
      )

----------------------------------- /~/ -----------------------------------

-- | Update an existing snippet in the database
dbUpdateSnippet :: Int -> Snippet -> Database -> IO ()
dbUpdateSnippet id Snippet{..} conn = do
   let version = case snippetCurrentVersion of
              		  SnippetVersion{..} -> versionNumber
   	                  VersionNumber n    -> n
   update conn T.snippets
      ( \content -> content!F.id .==. constant id )
      ( \content -> F.id             <<  _default
                  # F.parentId       <<- snippetParentId
                  # F.currentVersion <<- version
                  # F.created        <<- (pack $ show snippetCreated)
                  # F.userId         <<- snippetUserId
                  # F.description    <<- snippetDescription )

-- | Delete a snippet, all of its versions and associated data
dbDeleteSnippet :: Int -> Database -> IO ()
dbDeleteSnippet snippetId conn = do 
   let id = constant snippetId
   delete conn T.snippets $ \content -> content!F.id .==. id
   -- delete all associated records in snippet_versions
   delete conn T.snippetVersions $ \content -> content!F.snippetId .==. id
   -- delete all associated comments
   delete conn T.comments $ \content -> content!F.snippetId .==. id
   -- upvotes
   delete conn T.upvotes $ \content -> content!F.snippetId .==. id
   -- child snippets
   delete conn T.snippets $ \content -> content!F.parentId .==. id
   
----------------------------------- /~/ -----------------------------------

snippetFactory :: (Select (Attr F.Id Int)              r Int, 
                   Select (Attr F.ParentId Int)        r Int,
                   Select (Attr F.CurrentVersion Int)  r Int,
                   Select (Attr F.Created Text)        r Text,
                   Select (Attr F.UserId Int)          r Int,
                   Select (Attr F.Description Text)    r Text, 
                   Select (Attr F.Body Text)           r Text,
                   Select (Attr F.VersionCreated Text) r Text) => r
                -> Snippet

snippetFactory o = Snippet 
   { snippetId               = o!F.id
   , snippetParentId         = o!F.parentId
   , snippetCurrentVersion   = SnippetVersion 
	   { versionSnippetId = o!F.id
	   , versionNumber    = o!F.currentVersion
	   , versionBody      = o!F.body
	   , versionCreated   = parseUTCTimeText $ o!F.versionCreated
	   }
   , snippetCreated          = parseUTCTimeText $ o!F.created
   , snippetUserId           = o!F.userId
   , snippetDescription      = o!F.description
   , snippetComments         = []
   }

----------------------------------- /~/ -----------------------------------

simpleSnippetFactory :: (Select (Attr F.Id Int)              r Int, 
                         Select (Attr F.ParentId Int)        r Int,
                         Select (Attr F.CurrentVersion Int)  r Int,
                         Select (Attr F.Created Text)        r Text,
                         Select (Attr F.UserId Int)          r Int,
                         Select (Attr F.Description Text)    r Text) => r
                      -> Snippet

simpleSnippetFactory o = Snippet 
   { snippetId               = o!F.id
   , snippetParentId         = o!F.parentId
   , snippetCurrentVersion   = VersionNumber $ o!F.currentVersion
   , snippetCreated          = parseUTCTimeText $ o!F.created
   , snippetUserId           = o!F.userId
   , snippetDescription      = o!F.description
   , snippetComments         = []
   }

----------------------------------- /~/ -----------------------------------

snippetVersionFactory :: (Select (Attr F.Id Int)              r Int, 
                          Select (Attr F.CurrentVersion Int)  r Int,
                          Select (Attr F.Created Text)        r Text,
                          Select (Attr F.UserId Int)          r Int,
                          Select (Attr F.Description Text)    r Text, 
                          Select (Attr F.Body Text)           r Text,
                          Select (Attr F.VersionCreated Text) r Text) => r
                       -> SnippetVersion

snippetVersionFactory o = SnippetVersion
   { versionSnippetId = o!F.id
   , versionNumber    = 0
   , versionBody      = o!F.body
   , versionCreated   = parseUTCTimeText $ o!F.created
   }
