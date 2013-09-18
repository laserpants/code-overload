{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module App.Types.User where

import App.Types
import Control.Applicative                    ( (<|>), (<$>), (<*>), pure )
import Control.Monad                          ( mzero )
import Data.Aeson

data User = User
   { userId    :: !Int
   , userEmail :: !Text
   } deriving (Show)

instance FromJSON User where
   parseJSON (Object o) =
      User <$> (o .: "id" <|> pure 0)
           <*>  o .: "email"
   parseJSON _ = mzero

instance ToJSON User where
   toJSON User{..} = object [ "id"    .= userId
                            , "email" .= userEmail ]

instance KeyIndexable User where
   index = userId
