{-# OPTIONS -Wall #-}

-- | Controller routing/handling.

module Amelie.Controller
  (runHandler
  ,output)
  where

import Amelie.Types
import Amelie.Model.Config        (auth)

import Control.Monad.IO
import Control.Monad.Reader       (runReaderT)
import Data.Text.Lazy             (toStrict)
import Database.PostgreSQL.Simple (connect)
import Snap.Types                 (Snap,writeText)
import Text.Blaze                 (Html)
import Text.Blaze.Renderer.Text   (renderHtml)

-- | Run a controller handler.
runHandler :: Controller () -> Snap ()
runHandler ctrl = do
  conn <- io $ connect auth
  let state = ControllerState conn
  runReaderT (runController ctrl) state 

-- | Strictly renders HTML to Text before outputting it via Snap.
--   This ensures that any lazy exceptions are caught by the Snap
--   handler.
output :: Html -> Controller ()
output html = do
  let !x = toStrict $ renderHtml $ html
  writeText $ x
