{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module App.Types.Remix where

import App.Types
import Control.Applicative                    ( (<|>), (<$>), (<*>), pure )
import Data.Aeson

data Remix = Remix
   { remixSnippetId        :: Int
   , remixUserId           :: Int
   , remixBody             :: String
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
