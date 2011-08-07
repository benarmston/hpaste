{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Create new paste controller.

module Amelie.Controller.New
  (handle)
  where

import Amelie.Controller
import Amelie.Controller.Paste (pasteForm,getPasteId)
import Amelie.Model
import Amelie.Model.Channel    (getChannels)
import Amelie.Model.Language   (getLanguages)
import Amelie.Model.Paste      (getPasteById)
import Amelie.View.Edit        as Edit (page)
import Amelie.View.New         as New (page)

import Control.Applicative
import Data.Text.Encoding      (decodeUtf8)
import Snap.Types

-- @ label annotatePage
-- @ do Annotate a paste.
-- @ trigger getPasteById
-- @ trigger updatePaste
-- @ next pastePage

-- @ label newPastePage
-- @ do Create a new paste.
-- @ trigger createPaste
-- @ next pastePage

handle :: Controller ()
handle = do
  chans <- model $ getChannels
  langs <- model $ getLanguages
  defChan <- fmap decodeUtf8 <$> getParam "channel"
  pid <- getPasteId
  case pid of
    Just pid -> do
      paste <- model $ getPasteById (fromIntegral pid)
      form <- pasteForm chans langs defChan paste
      justOrGoHome paste $ \paste -> do
        output $ Edit.page paste form
    Nothing -> do
      form <- pasteForm chans langs defChan Nothing
      output $ New.page form
