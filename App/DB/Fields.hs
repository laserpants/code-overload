module App.DB.Fields where

import Database.HaskellDB.DBLayout
import Data.Text
import Prelude                                hiding ( id )

-- | id

data Id = Id

instance FieldTag Id where 
   fieldName _ = "id"

id :: Attr Id Int
id = mkAttr Id

-- | email

data Email = Email

instance FieldTag Email where 
   fieldName _ = "email"

email :: Attr Email Text
email = mkAttr Email

-- | current version

data CurrentVersion = CurrentVersion

instance FieldTag CurrentVersion where 
   fieldName _ = "current_version"

currentVersion :: Attr CurrentVersion Int
currentVersion = mkAttr CurrentVersion

-- | created

data Created = Created

instance FieldTag Created where 
   fieldName _ = "created"

created :: Attr Created Text
created = mkAttr Created

-- | user id

data UserId = UserId

instance FieldTag UserId where 
   fieldName _ = "user_id"

userId :: Attr UserId Int
userId = mkAttr UserId

-- | description

data Description = Description

instance FieldTag Description where 
   fieldName _ = "description"

description :: Attr Description Text
description = mkAttr Description

-- | version

data Version = Version

instance FieldTag Version where 
   fieldName _ = "version"

version :: Attr Version Int
version = mkAttr Version

-- | body

data Body = Body

instance FieldTag Body where 
   fieldName _ = "contents"

body :: Attr Body Text
body = mkAttr Body

-- | snippet id

data SnippetId = SnippetId

instance FieldTag SnippetId where 
   fieldName _ = "snippet_id"

snippetId :: Attr SnippetId Int
snippetId = mkAttr SnippetId

-- | remix id

data RemixId = RemixId

instance FieldTag RemixId where 
   fieldName _ = "remix_id"

remixId :: Attr RemixId Int
remixId = mkAttr RemixId

-- | entity type

data EntityType = EntityType

instance FieldTag EntityType where 
   fieldName _ = "entity_type"

entityType :: Attr EntityType Text
entityType = mkAttr EntityType

-- | entity id

data EntityId = EntityId

instance FieldTag EntityId where 
   fieldName _ = "entity_id"

entityId :: Attr EntityId Int
entityId = mkAttr EntityId

-- | entity version

data EntityVersion = EntityVersion

instance FieldTag EntityVersion where 
   fieldName _ = "entity_version"

entityVersion :: Attr EntityVersion Int
entityVersion = mkAttr EntityVersion

-- | version created

data VersionCreated = VersionCreated

instance FieldTag VersionCreated where 
   fieldName _ = "created"

versionCreated :: Attr VersionCreated Text
versionCreated = mkAttr VersionCreated
