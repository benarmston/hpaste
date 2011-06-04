{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Stylesheet controller.

module Amelie.Controller.Style
  (handle)
  where

import Amelie.Controller (outputText)
import Amelie.Model
import Amelie.View.Style (style)

import Snap.Types        (modifyResponse,setContentType)

handle :: Controller ()
handle = do
  modifyResponse $ setContentType "text/css"
  outputText $ style
