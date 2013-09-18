{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module App.Types.Upvote where

import App.Types
import Control.Applicative                    ( Applicative, (<|>), (<$>), (<*>), pure )
import Control.Monad                          ( mzero )
import Data.Aeson

data Upvote = Upvote
   { upvoteUserId          :: !Int
   , upvoteRemixId         :: !Int
   } deriving (Show)

instance FromJSON Upvote where
   parseJSON (Object o) =
      Upvote <$> o .: "userId"
             <*> o .: "remixId"
   parseJSON _ = mzero

instance ToJSON Upvote where
   toJSON Upvote{..} = object [ "userId"   .= upvoteUserId
                              , "remixId"  .= upvoteRemixId ]
