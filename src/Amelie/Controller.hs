{-# OPTIONS -Wall #-}

-- | Controller routing/handling.

module Amelie.Controller
  (runHandler
  ,output
  ,outputText)
  where

import Amelie.Types

import Control.Monad.Reader       (runReaderT)
import Data.Text.Lazy             (Text,toStrict)
import Database.PostgreSQL.Simple (Pool,withPoolConnection)
import Snap.Types                 (Snap,writeText)
import Text.Blaze                 (Html)
import Text.Blaze.Renderer.Text   (renderHtml)

-- | Run a controller handler.
runHandler :: Pool -> Controller () -> Snap ()
runHandler pool ctrl = do
  withPoolConnection pool $ \conn -> do
    let state = ControllerState conn
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
