{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module App.Types.Snippet where

import App.Types
import App.Types.Comment
import Control.Applicative                    ( (<|>), (<$>), (<*>), pure )
import Control.Monad                          ( mzero )
import Data.Aeson
import Data.Time.Clock                        ( UTCTime )

data SnippetVersion = SnippetVersion
   { versionSnippetId      :: Int
   , versionNumber         :: Int
   , versionBody           :: String
   , versionCreated        :: UTCTime
   } deriving (Show)

data Snippet = Snippet
   { snippetId             :: Int
   , snippetCurrentVersion :: Int
   , snippetCreated        :: UTCTime
   , snippetUserId         :: Int
   , snippetDescription    :: String
   , snippetComments       :: [Comment]
   } deriving (Show)

instance FromJSON SnippetVersion where
   parseJSON (Object o) =
      SnippetVersion <$> o .: "snippet_id"
                     <*> o .: "version"
                     <*> o .: "body"
                     <*> o .: "created"
   parseJSON _ = mzero

instance ToJSON SnippetVersion where
   toJSON SnippetVersion{..} = object [ "snippet_id"  .= versionSnippetId
                                      , "version"     .= versionNumber
                                      , "body"        .= versionBody
                                      , "created"     .= versionCreated ]

instance FromJSON Snippet where
   parseJSON (Object o) = do
      comments <- parseJSON =<< (o .: "comments")
      Snippet <$> (o .: "id" <|> pure 0)
              <*>  o .: "current_version"
              <*>  o .: "created"
              <*>  o .: "user_id"
              <*>  o .: "description"
              <*>  o .: comments
   parseJSON _ = mzero

instance ToJSON Snippet where
   toJSON Snippet{..} = object [ "id"              .= snippetId
                               , "current_version" .= snippetCurrentVersion
                               , "created"         .= snippetCreated
                               , "user_id"         .= snippetUserId
                               , "description"     .= snippetDescription 
                               , "comments"        .= snippetComments ]

instance KeyIndexable Snippet where
   index = snippetId
