{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Home page controller.

module Amelie.Controller.Home
  (handle)
  where

import Amelie.Controller        (output)
import Amelie.Model
import Amelie.Model.Home        (getPastes)
import Amelie.View.Home         (page)

handle :: Controller ()
handle = do
  pastes <- model $ getPastes
  output $ page pastes
