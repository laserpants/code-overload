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
      Comment <$>  o .: "id"
              <*>  o .: "userId"
              <*>  o .: "type"
              <*>  o .: "entityId"
              <*>  o .: "entityVersion"
              <*> (o .: "created" >>= toUTCTime)
              <*>  o .: "body"
   parseJSON _ = mzero

instance ToJSON Comment where
   toJSON Comment{..} = object [ "id"              .= commentId
                               , "userId"          .= commentUserId
                               , "type"            .= commentEntityType
                               , "entityId"        .= commentEntityId
                               , "entityVersion"   .= commentEntityVersion
                               , "created"         .= commentCreated
                               , "body"            .= commentBody ]

instance KeyIndexable Comment where
   index = commentId
