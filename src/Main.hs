{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point.

module Main (main) where

import Amelie.Config
import Amelie.Controller
import Amelie.Controller.Browse   as Browse
import Amelie.Controller.Cache    (newCache)
import Amelie.Controller.Home     as Home
import Amelie.Controller.New      as New
import Amelie.Controller.Paste    as Paste
import Amelie.Controller.Raw      as Raw
import Amelie.Controller.Script   as Script
import Amelie.Controller.Style    as Style
import Amelie.Model.Announcer     (newAnnouncer)
import Amelie.Types
import Amelie.Types.Cache

import Snap.Http.Server           hiding (Config)
import Snap.Types
import Snap.Util.FileServe

import Control.Concurrent.Chan    (Chan)
import Data.Text.Lazy             (Text)
import Database.PostgreSQL.Simple (Pool,newPool)
import System.Environment

-- | Main entry point.
main :: IO ()
main = do
  cpath:_ <- getArgs
  config <- getConfig cpath
  announces <- newAnnouncer (configAnnounce config)
  pool <- newPool (configPostgres config)
  cache <- newCache
  setUnicodeLocale "en_US"
  httpServe server (serve config pool cache announces)
 where server = addListen (ListenHttp "0.0.0.0" 10000) defaultConfig

-- | Serve the controllers.
serve :: Config -> Pool -> Cache -> Chan Text -> Snap ()
serve conf p cache ans = route routes where
  routes = [("/css/amelie.css", run Style.handle)
           ,("/js/amelie.js", run Script.handle)
           ,("/css/",serveDirectory "wwwroot/css")
           ,("/js/",serveDirectory "wwwroot/js")
           ,("",run Home.handle)
           ,("/:id",run Paste.handle)
           ,("/raw/:id",run Raw.handle)
           ,("/new",run New.handle)
           ,("/new/:channel",run New.handle)
           ,("/browse/page/:page/offset/:offset",run Browse.handle)
           ,("/browse/page/:page",run Browse.handle)
           ,("/browse",run Browse.handle)
           ]
  run = runHandler conf p cache ans
