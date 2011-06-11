{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point.

module Main (main) where

import Amelie.Controller
import Amelie.Controller.Browse   as Browse
import Amelie.Controller.Cache    (newCache)
import Amelie.Controller.Home     as Home
import Amelie.Controller.New      as New
import Amelie.Controller.Paste    as Paste
import Amelie.Controller.Raw      as Raw
import Amelie.Controller.Script   as Script
import Amelie.Controller.Style    as Style
import Amelie.Model.Config        (auth)
import Amelie.Types
import Amelie.Types.Cache

import Snap.Http.Server
import Snap.Types
import Snap.Util.FileServe

import Database.PostgreSQL.Simple (Pool,newPool)

-- | Main entry point.
main :: IO ()
main = do
  p <- newPool auth
  cache <- newCache
  setUnicodeLocale "en_US"
  httpServe config (serve p cache)
  where config = addListen (ListenHttp "0.0.0.0" 10000) defaultConfig

-- | Serve the controllers.
serve :: Pool -> Cache -> Snap ()
serve p cache = route routes where
  routes = [("/css/amelie.css", run Style.handle)
           ,("/js/amelie.js", run Script.handle)
           ,("/css/",serveDirectory "wwwroot/css")
           ,("/js/",serveDirectory "wwwroot/js")
           ,("",run Home.handle)
           ,("/:id",run Paste.handle)
           ,("/raw/:id",run Raw.handle)
           ,("/new",run New.handle)
           ,("/browse/page/:page/offset/:offset",run Browse.handle)
           ,("/browse/page/:page",run Browse.handle)
           ,("/browse",run Browse.handle)
           ]
  run = runHandler p cache
