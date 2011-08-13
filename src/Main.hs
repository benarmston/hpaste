{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point.

module Main (main) where

import Amelie.Config
import Amelie.Controller
import Amelie.Controller.Activity as Activity
import Amelie.Controller.Browse   as Browse
import Amelie.Controller.Cache    (newCache)
import Amelie.Controller.Diff     as Diff
import Amelie.Controller.Home     as Home
import Amelie.Controller.New      as New
import Amelie.Controller.Paste    as Paste
import Amelie.Controller.Raw      as Raw
import Amelie.Controller.Report   as Report
import Amelie.Controller.Reported as Reported
import Amelie.Controller.Script   as Script
import Amelie.Controller.Stepeval as Stepeval
import Amelie.Controller.Steps    as Steps
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

-- @ label main
-- @ do Main entry point.
-- @ trigger readConfig
-- @ trigger connectToIRC
-- @ trigger createConnectionPool
-- @ next server
-- | Main entry point.
main :: IO ()
main = do
  cpath:_ <- getArgs
  -- @ label readConfig
  -- @ task Read config file.
  config <- getConfig cpath
  -- @ label connectToIRC
  -- @ task Connect to IRC.
  announces <- newAnnouncer (configAnnounce config)
  -- @ label createConnectionPool
  -- @ task Create connection pool.
  pool <- newPool (configPostgres config)
  cache <- newCache
  setUnicodeLocale "en_US"
  httpServe server (serve config pool cache announces)
 where server = addListen (ListenHttp "0.0.0.0" 10000) defaultConfig

-- @ label server
-- @ do Web server.
-- @ trigger serve
-- | Serve the controllers.
serve :: Config -> Pool -> Cache -> Chan Text -> Snap ()
serve conf p cache ans = route routes where
  -- @ label serve
  -- @ if Static?
  -- @ then staticServe
  -- @ else pageServe
  routes = [
            -- @ label staticServe
            -- @ do Serve static content.
            ("/css/amelie.css", run Style.handle)
           ,("/js/amelie.js", run Script.handle)
           ,("/css/",serveDirectory "wwwroot/css")
           ,("/js/",serveDirectory "wwwroot/js")
           ,("/hs/",serveDirectory "wwwroot/hs")
            -- @ label pageServe
            -- @ do Serve page.
           ,("",run Home.handle)
            -- @ next homePage
           ,("/:id",run Paste.handle)
            -- @ next pastePage
           ,("/steps/:id",run Steps.handle)
            -- @ next stepsPage
           ,("/raw/:id",run Raw.handle)
            -- @ next rawPastePage
           ,("/report/:id",run Report.handle)
           ,("/reported",run Reported.handle)
            -- @ next reportPastePage
           ,("/new",run New.handle)
            -- @ next newPastePage
           ,("/edit/:id",run New.handle)
            -- @ next annotatePage
           ,("/new/:channel",run New.handle)
            -- @ next browsePage
           ,("/browse/page/:page/offset/:offset",run Browse.handle)
           ,("/browse/page/:page",run Browse.handle)
           ,("/browse",run Browse.handle)
            -- @ next activityPage
           ,("/activity",run Activity.handle)
            -- @ next diffPage
           ,("/diff/:this/:that",run Diff.handle)
            -- @ next stepEvalRawPage
           ,("/stepeval/raw",run Stepeval.handleRaw)
            -- @ next stepEvalPage
           ,("/stepeval",run Stepeval.handle)
           ]
  run = runHandler conf p cache ans
