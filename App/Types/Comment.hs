{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module App.Types.Comment where

import App.Types
import Control.Applicative                    ( (<|>), (<$>), (<*>), pure )
import Control.Monad                          ( mzero )
import Data.Aeson
import Data.Time.Clock                        ( UTCTime )

data EntityType = SnippetEntity | RemixEntity
   deriving (Show)

instance ToJSON EntityType where
   toJSON RemixEntity = "remix"
   toJSON _           = "snippet"

instance FromJSON EntityType where
   parseJSON "remix" = return RemixEntity
   parseJSON _       = return SnippetEntity

data Comment = Comment
   { commentId             :: Int
   , commentUserId         :: Int
   , commentEntityType     :: EntityType
   , commentEntityId       :: Int
   , commentEntityVersion  :: Int
   , commentCreated        :: UTCTime
   , commentBody           :: String
   } deriving (Show)

instance FromJSON Comment where
   parseJSON (Object o) =
      Comment <$> o .: "comment_id"
              <*> o .: "user_id"
              <*> o .: "type"
              <*> o .: "entity_id"
              <*> o .: "entity_version"
              <*> o .: "created"
              <*> o .: "body"
   parseJSON _ = mzero

instance ToJSON Comment where
   toJSON Comment{..} = object [ "comment_id"      .= commentId
                               , "user_id"         .= commentUserId
                               , "type"            .= commentEntityType
                               , "entity_id"       .= commentEntityId
                               , "entity_version"  .= commentEntityVersion
                               , "created"         .= commentCreated
                               , "body"            .= commentBody ]

instance KeyIndexable Comment where
   index = commentId
