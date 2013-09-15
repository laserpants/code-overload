{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module App.Types.Snippet where

import App.Types
import App.Types.Comment
import Control.Applicative                    ( Applicative, (<|>), (<$>), (<*>), pure )
import Control.Monad                          ( mzero )
import Data.Aeson
import Data.Time.Clock                        ( UTCTime )

data SnippetVersion = VersionNumber Int
   | SnippetVersion
   { versionSnippetId      :: Int
   , versionNumber         :: Int
   , versionBody           :: String
   , versionCreated        :: UTCTime
   } deriving (Show)

data Snippet = Snippet
   { snippetId             :: Int
   , snippetCurrentVersion :: SnippetVersion
   , snippetCreated        :: UTCTime
   , snippetUserId         :: Int
   , snippetDescription    :: String
   , snippetComments       :: [Comment]
   } deriving (Show)

instance FromJSON SnippetVersion where
   parseJSON (Object o) =
      SnippetVersion <$>  o .: "snippetId"
                     <*>  o .: "version"
                     <*>  o .: "body"
                     <*> (o .: "created" >>= toUTCTime)
   parseJSON v = VersionNumber <$> parseJSON v

instance ToJSON SnippetVersion where
   toJSON SnippetVersion{..} = object [ "snippetId"   .= versionSnippetId
                                      , "version"     .= versionNumber
                                      , "body"        .= versionBody
                                      , "created"     .= versionCreated ]
   toJSON (VersionNumber v) = toJSON v

instance FromJSON Snippet where
   parseJSON (Object o) = do   
      comments <- parseJSON =<< (o .: "comments")
      Snippet <$> (o .: "id" <|> pure 0)
              <*>  o .: "currentVersion"
              <*> (o .: "created" >>= toUTCTime)
              <*>  o .: "userId"
              <*>  o .: "description"
              <*> (pure comments)
   parseJSON _ = mzero

instance ToJSON Snippet where
   toJSON Snippet{..} = object [ "id"              .= snippetId
                               , "currentVersion"  .= snippetCurrentVersion
                               , "created"         .= snippetCreated
                               , "userId"          .= snippetUserId
                               , "description"     .= snippetDescription 
                               , "comments"        .= snippetComments ]

instance KeyIndexable Snippet where
   index = snippetId
