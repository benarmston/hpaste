{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Home page controller.

module Amelie.Controller.Home
  (handle)
  where

import Amelie.Controller       (output)
import Amelie.Controller.Paste (pasteForm)
import Amelie.Model
import Amelie.Model.Home       (getPastes)
import Amelie.View.Home        (page)

handle :: Controller ()
handle = do
  pastes <- model $ getPastes
  form <- pasteForm
  output $ page pastes form
