{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module App.Types.Upvote where

import App.Types
import Control.Applicative                     ( (<|>), (<$>), (<*>), pure )
import Control.Monad                           ( mzero )
import Data.Aeson

data Upvote = Upvote
   { upvoteUserId          :: !Int
   , upvoteSnippetId       :: !Int
   } deriving (Show)

instance FromJSON Upvote where
   parseJSON (Object o) =
      Upvote <$> o .: "userId"
             <*> o .: "snippetId"
   parseJSON _ = mzero

instance ToJSON Upvote where
   toJSON Upvote{..} = object [ "userId"   .= upvoteUserId
                              , "snippet"  .= upvoteSnippetId ]
