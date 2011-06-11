{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Create new paste controller.

module Amelie.Controller.New
  (handle)
  where

import Amelie.Controller
import Amelie.Controller.Paste (pasteForm)
import Amelie.Model
import Amelie.Model.Channel    (getChannels)
import Amelie.Model.Language   (getLanguages)
import Amelie.View.New         (page)

import Control.Applicative
import Data.Text.Encoding      (decodeUtf8)
import Snap.Types

handle :: Controller ()
handle = do
  chans <- model $ getChannels
  langs <- model $ getLanguages
  defChan <- fmap decodeUtf8 <$> getParam "channel"
  form <- pasteForm chans langs defChan
  output $ page form
