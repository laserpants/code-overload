{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Main where

import App.Controller.Comment
import App.Controller.Snippet
import App.Controller.Upvote
import App.Router
import Control.Monad						   ( msum, mzero, join )
import Happstack.Server

import Data.Aeson
import App.Types.Snippet
import App.Types.Comment

import Data.ByteString.Lazy

main = do
   let x = decode "{ \"userId\": 1, \"entityId\": 1, \"entityVersion\": 5, \"body\": \"hello\" }"
   print (x :: Maybe Comment)


{-
main :: IO ()
main = simpleHTTP nullConf $ do
   decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
   msum $ join [ [method OPTIONS >> preFlight]
               , restRoute "snippet" getSnippet putSnippet deleteSnippet getSnippets postSnippet
               , restRoute "comment" getComment (const mzero) (const mzero) mzero postComment
               , restRoute "upvote"  (const mzero) (const mzero) (const mzero) mzero postUpvote
               ]
-}
