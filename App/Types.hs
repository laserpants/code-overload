module App.Types where

import Control.Applicative                    ( Applicative, pure )
import Data.Time.Clock                        ( UTCTime )
import Data.Time.Clock.POSIX

class KeyIndexable a
   where index :: a -> Int

toUTCTime :: (Applicative f) => Int -> f UTCTime
toUTCTime = pure . posixSecondsToUTCTime . fromIntegral
