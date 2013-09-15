{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module App.Json where

import App.DB.Connect
import App.Types
import Control.Monad.Trans.Class              ( lift )
import Data.Aeson
import Database.HaskellDB
import Happstack.Server

import qualified Data.ByteString			  as BS 
import qualified Data.ByteString.Char8	      as BC 
import qualified Data.Map	          	      as M

-- | Represents a JSON response object
data ResponseObj = ResponseObj
   { responseSuccess :: Bool
   , responseMessage :: BS.ByteString
   }

instance ToJSON ResponseObj where
   toJSON ResponseObj{..} = object [ "success"  .= responseSuccess
                                   , "response" .= responseMessage ]

instance ToMessage Value where
   toContentType _ = BC.pack "application/json; charset=UTF-8"
   toMessage       = encode
   toResponse val  = 
      let bs  = toMessage val
          res = Response 200 M.empty nullRsFlags bs Nothing
       in setHeaderBS "Access-Control-Allow-Origin" "*" $ 
          setHeaderBS "Content-Type" (toContentType val) res

-- | Represents a REST pre-flight OPTIONS response
data PreflightResp = PreflightResp BS.ByteString

instance ToMessage PreflightResp where
   toResponse (PreflightResp orig) =
      let resp = Response 200 M.empty nullRsFlags "" Nothing 
         in foldr (uncurry setHeaderBS) resp
      [ ("Access-Control-Allow-Origin", orig)
      , ("Access-Control-Allow-Methods", "POST, GET, OPTIONS, PUT, DELETE")
      , ("Access-Control-Allow-Headers", "Content-Type, Cookie, Origin, X-Request, X-Requested-With, Accept")
      , ("Content-Length", "0")
      , ("Keep-Alive", "timeout=2, max=100")
      , ("Connection", "Keep-Alive")
      , ("X-Powered-By", "Happstack")
      , ("Content-Type", "text/html; charset=UTF-8")
      ]

jsonOkResponse :: (ToJSON a) => a -> ServerPartT IO Response
jsonOkResponse = ok . toResponse . toJSON 

jsonBadRequest :: ServerPartT IO Response
jsonBadRequest = badRequest . toResponse . toJSON $ 
                 ResponseObj False "400 Bad Request"

jsonErrorNotFound :: ServerPartT IO Response
jsonErrorNotFound = notFound . toResponse . toJSON $ 
                    ResponseObj False "404 Resource not found"

jsonOkDefault :: ServerPartT IO Response
jsonOkDefault = jsonOkResponse $ ResponseObj True ""

-- | Decode POST request body in the ServerPart monad to a (Maybe t) value
-- where the type t must be a FromJSON instance 
translatePostData :: (FromJSON a) => ServerPartT IO (Maybe a)
translatePostData = do
   req   <- askRq
   body  <- takeRequestBody req
   return $ body >>= \(Body b) -> decode b

dbRun :: (FromJSON t) => Maybe t -> (t -> Database -> IO a) -> ServerPartT IO Response
dbRun obj fn = 
   case obj of
      Nothing -> jsonBadRequest
      Just it -> (lift $ withDB $ fn it) >> jsonOkDefault
