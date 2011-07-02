{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Stepeval explanation controller.

module Amelie.Controller.Stepeval
  (handle
  ,handleRaw)
  where

import           Amelie.Types

import           Amelie.Controller
import           Amelie.Model
import           Amelie.Model.Paste
import           Amelie.View.Stepeval (page)

import           Control.Monad.Env
import           Control.Monad.IO
import qualified Data.Text.IO         as T
import           Data.Text.Lazy       (fromStrict)
import           Prelude              hiding ((++))
import           Snap.Types

-- | Handle the stepeval explanation page.
handle :: Controller ()
handle = do
  conf     <- env controllerStateConfig
  contents <- io $ T.readFile $ configStepevalPrelude conf
  hints    <- model $ generateHints "stepeval" contents
  output $ page StepevalPage {
    seHints = hints
  , sePaste = contents
  }

-- | Handle the raw stepeval Prelude view.
handleRaw :: Controller ()
handleRaw = do
  modifyResponse $ setContentType "text/plain; charset=UTF-8"
  conf     <- env controllerStateConfig
  contents <- io $ T.readFile $ configStepevalPrelude conf
  outputText . fromStrict $ contents
