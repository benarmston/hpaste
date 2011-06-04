{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point.

module Main (main) where

import Amelie.Controller
import Amelie.Controller.Home  as Home
import Amelie.Controller.Paste as Paste
import Amelie.Controller.Style as Style

import Snap.Http.Server
import Snap.Types
import Snap.Util.FileServe

-- | Main entry point.
main :: IO ()
main = do
  setUnicodeLocale "en_US"
  httpServe config serve
  where config = addListen (ListenHttp "0.0.0.0" 10000) defaultConfig

-- | Serve the controllers.
serve :: Snap ()
serve = route routes where
  routes = [ ("/css/amelie.css", runHandler Style.handle)
           , ("/css/",serveDirectory "wwwroot/css")
           , ("",runHandler Home.handle)
           , ("/:id",runHandler Paste.handle)
           ]
