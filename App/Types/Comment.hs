{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module App.Types.Comment where

import App.Types
import Control.Applicative                    ( (<|>), (<$>), (<*>), pure )
import Control.Monad                          ( mzero )
import Data.Aeson
import Data.Time.Clock                        ( UTCTime )
import Data.Text

data EntityType = SnippetEntity | RemixEntity

instance Show EntityType where
   show RemixEntity = "remix"
   show _           = "snippet"

instance ToJSON EntityType where
   toJSON = String . pack . show

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
      Comment <$> (o .: "id"              <|> pure 0)
              <*> (o .: "userId")
              <*> (o .: "type"            <|> pure SnippetEntity)
              <*> (o .: "entityId")
              <*> (o .: "entityVersion"   <|> pure 1)
              <*> (o .: "created"         <|> pure 0 >>= toUTCTime)
              <*> (o .: "body")
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
