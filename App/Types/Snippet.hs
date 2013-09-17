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
      SnippetVersion <$> (o .: "snippetId" <|> pure 0)
                     <*> (o .: "version"   <|> pure 1)
                     <*> (o .: "body")
                     <*> (o .: "created"   <|> pure 0 >>= toUTCTime)
   parseJSON v = VersionNumber <$> parseJSON v

instance ToJSON SnippetVersion where
   toJSON SnippetVersion{..} = object [ "snippetId"   .= versionSnippetId
                                      , "version"     .= versionNumber
                                      , "body"        .= versionBody
                                      , "created"     .= versionCreated ]
   toJSON (VersionNumber v) = toJSON v

instance FromJSON Snippet where
   parseJSON (Object o) = do
      let versionOne = pure $ VersionNumber 1
      comments <- (o .: "comments" >>= parseJSON) <|> pure []
      Snippet <$> (o .: "id"             <|> pure 0)
              <*> (o .: "currentVersion" <|> versionOne)
              <*> (o .: "created"        <|> pure 0 >>= toUTCTime)
              <*> (o .: "userId")
              <*> (o .: "description")
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
