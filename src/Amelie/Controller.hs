{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Controller routing/handling.

module Amelie.Controller
  (runHandler
  ,output
  ,outputText
  ,goHome
  ,justOrGoHome)
  where

import Amelie.Types
import Amelie.Types.Cache

import Control.Monad.Reader       (runReaderT)
import Data.Text.Lazy             (Text,toStrict)
import Database.PostgreSQL.Simple (Pool,withPoolConnection)
import Snap.Types                 (Snap,writeText,redirect)
import Text.Blaze                 (Html)
import Text.Blaze.Renderer.Text   (renderHtml)

-- | Run a controller handler.
runHandler :: Pool -> Cache -> Controller () -> Snap ()
runHandler pool cache ctrl = do
  withPoolConnection pool $ \conn -> do
    let state = ControllerState conn cache
    runReaderT (runController ctrl) state 

-- | Strictly renders HTML to Text before outputting it via Snap.
--   This ensures that any lazy exceptions are caught by the Snap
--   handler.
output :: Html -> Controller ()
output html = outputText $ renderHtml $ html

-- | Strictly renders text before outputting it via Snap.
--   This ensures that any lazy exceptions are caught by the Snap
--   handler.
outputText :: Text -> Controller ()
outputText text = do
  let !x = toStrict $ text
  writeText x

-- | Generic redirect to home page.
goHome :: Controller ()
goHome = redirect "/"

-- | Extract a Just value or go home.
justOrGoHome :: Maybe a -> (a -> Controller ()) -> Controller ()
justOrGoHome x m = maybe goHome m x
