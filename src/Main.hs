{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point.

module Main (main) where

import Amelie.Controller
import Amelie.Controller.Home     as Home
import Amelie.Controller.Paste    as Paste
import Amelie.Controller.Script   as Script
import Amelie.Controller.Style    as Style
import Amelie.Model.Config        (auth)

import Snap.Http.Server
import Snap.Types
import Snap.Util.FileServe

import Database.PostgreSQL.Simple (Pool,newPool)

-- | Main entry point.
main :: IO ()
main = do
  p <- newPool auth
  setUnicodeLocale "en_US"
  httpServe config (serve p)
  where config = addListen (ListenHttp "0.0.0.0" 10000) defaultConfig

-- | Serve the controllers.
serve :: Pool -> Snap ()
serve p = route routes where
  routes = [("/css/amelie.css", run Style.handle)
           ,("/js/amelie.js", run Script.handle)
           ,("/css/",serveDirectory "wwwroot/css")
           ,("/js/",serveDirectory "wwwroot/js")
           ,("",run Home.handle)
           ,("/:id",run Paste.handle)
           ]
  run = runHandler p
