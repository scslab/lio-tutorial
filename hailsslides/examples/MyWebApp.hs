{-# LANGUAGE OverloadedStrings #-}
module MyWebApp where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as S8

import Prelude hiding (lookup)
import Hails.HttpServer
import Hails.Web
import Hails.Web.Frank
import Hails.Data.Hson
import LIO
import qualified Data.Text as T
import Hails.Web.User

server :: Application
server = mkRouter $ do
  routeTop (redirectTo "/static/name.html")
  get "/echo-name" $ do
    mfname <- queryParam "fname"
    mlname <- queryParam "lname"
    case (mfname, mlname) of
      (Just fname, Just lname) -> respond $ 
        okHtml $ L.fromChunks ["Hello ", fname, " ", lname, "!"]
      _       -> respond badRequest
  post "/echo-name2" $ do
    doc <- hsonRequest
    let mname = do
          fname <- lookup "fname" doc
          lname <- lookup "fname" doc
          return (S8.pack fname, S8.pack lname)
    case mname of
      (Just (fname, lname)) -> respond $ 
        okHtml $ L.fromChunks["Hello ",  fname, " ", lname, "!"]
      _       -> respond badRequest
  get "/echo" $ withUserOrDoAuth $ \user ->
     respond $ okHtml $ L8.pack $ "Hello " ++ (T.unpack user)
    
    where hsonRequest = request >>= labeledRequestToHson >>= (liftLIO . unlabel)
