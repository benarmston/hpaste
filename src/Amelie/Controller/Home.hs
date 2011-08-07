{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Home page controller.

module Amelie.Controller.Home
  (handle)
  where

import Amelie.Controller       (outputText)
import Amelie.Controller.Cache (cache)
import Amelie.Controller.Paste (pasteForm)
import Amelie.Model
import Amelie.Model.Channel    (getChannels)
import Amelie.Model.Language   (getLanguages)
import Amelie.Model.Paste      (getLatestPastes)
import Amelie.Types.Cache      as Key
import Amelie.View.Home        (page)

-- @ label homePage
-- @ do Home page.
-- @ trigger getLatestPastes
-- @ next pastePage
-- @ next newPastePage
-- @ next activityPage
handle :: Controller ()
handle = do
  html <- cache Key.Home $ do
    pastes <- model $ getLatestPastes
    chans <- model $ getChannels
    langs <- model $ getLanguages
    form <- pasteForm chans langs Nothing Nothing
    return $ Just $ page chans langs pastes form
  maybe (return ()) outputText html
