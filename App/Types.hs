module App.Types where

class KeyIndexable a
   where index :: a -> Int
