{-# LANGUAGE OverloadedStrings #-}

module App.Router where

import App.Json
import Control.Applicative                     ( (<|>), (<$>), (<*>) )
import Happstack.Server

type ServR = ServerPartT IO Response

-- | Pre-flight response sent back to user-agent on OPTIONS request
preFlight :: ServR
preFlight = do
   req <- askRq
   let origin = getHeaderBS "origin" req
   ok $ toResponse $ PreflightResp $ case origin of
      Nothing -> "*"
      Just o  ->  o

----------------------------------- /~/ -----------------------------------

restRoute :: String -> (Int -> ServR) -> (Int -> ServR) -> (Int -> ServR) -> ServR -> ServR -> [ServR]
restRoute param get put delete index post =
   [ dir param $ path $ \_id ->
             (method GET     >> get _id)
         <|> (method PUT     >> put _id)
         <|> (method DELETE  >> delete _id)
   , dir param $ 
             (method GET     >> index)
         <|> (method POST    >> post)
   ]
