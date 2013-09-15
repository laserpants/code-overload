module Main where

import App.Controller.Comment
import App.Controller.Snippet
import App.Router
import Control.Monad						   ( msum, mzero, join )
import Happstack.Server

main :: IO ()
main = simpleHTTP nullConf $ do
   decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
   msum $ join [ [method OPTIONS >> preFlight]
               , restRoute "snippet" getSnippet putSnippet deleteSnippet getSnippets postSnippet
               , restRoute "comment" getComment (const mzero) (const mzero) mzero postComment
               ]
