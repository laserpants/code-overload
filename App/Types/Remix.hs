{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module App.Types.Remix where

import App.Types
import Control.Applicative                    ( (<|>), (<$>), (<*>), pure )
import Data.Aeson
import Data.Text

data Remix = Remix
   { remixSnippetId        :: Int
   , remixUserId           :: Int
   , remixBody             :: !Text
   } deriving (Show)

instance FromJSON Remix where
   parseJSON (Object o) =
      Remix <$> o .: "snippetId"
            <*> o .: "userId"
            <*> o .: "body"
   parseJSON _ = mzero

instance ToJSON Remix where
   toJSON Remix{..} = object [ "snippetId"   .= remixSnippetId
                             , "userId"      .= remixUserId
                             , "body"        .= remixBody ]
