{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module App.Types.Comment where

import App.Types
import Control.Applicative                     ( (<|>), (<$>), (<*>), pure )
import Control.Monad                           ( mzero )
import Data.Aeson
import Data.Text
import Data.Time                               ( UTCTime )

data Comment = Comment
   { commentId             :: !Int
   , commentUserId         :: !Int
   , commentSnippetId      :: !Int
   , commentSnippetVersion :: !Int
   , commentCreated        :: !UTCTime
   , commentBody           :: !Text
   } deriving (Show)

instance FromJSON Comment where
   parseJSON (Object o) =
      Comment <$> (o .: "id"              <|> pure 0)
              <*> (o .: "userId")
              <*> (o .: "snippetId")
              <*> (o .: "snippetVersion"  <|> pure 1)
              <*> (o .: "created"         <|> pure 0 >>= toUTCTime)
              <*> (o .: "body")
   parseJSON _ = mzero

instance ToJSON Comment where
   toJSON Comment{..} = object [ "id"              .= commentId
                               , "userId"          .= commentUserId
                               , "snippetId"       .= commentSnippetId
                               , "snippetVersion"  .= commentSnippetVersion
                               , "created"         .= commentCreated
                               , "body"            .= commentBody ]

instance KeyIndexable Comment where
   index = commentId
