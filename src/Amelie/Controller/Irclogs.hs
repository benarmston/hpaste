{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Irclogs page controller.

module Amelie.Controller.Irclogs
  (handle)
  where

import Amelie.Controller
import Amelie.Model.Irclogs
import Amelie.Types
import Amelie.View.Irclogs  (page)

import Data.String.ToString
import Data.String
import Snap.Types

handle :: Controller ()
handle = do
  channel <- get "channel"
  date <- get "date"
  time <- get "timestamp"
  logs <- getNarrowedLogs channel date time
  output $ page channel date time logs

  where get key = do
          value <- fmap (fmap toString) $ getParam (fromString key)
          case value of
            Nothing -> error $ "Missing parameter: " ++ key
            Just value -> return value
