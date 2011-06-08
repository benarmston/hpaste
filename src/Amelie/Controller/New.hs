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

handle :: Controller ()
handle = do
  chans <- model $ getChannels
  langs <- model $ getLanguages
  form <- pasteForm chans langs
  output $ page form
