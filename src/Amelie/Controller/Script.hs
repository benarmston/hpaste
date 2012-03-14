{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | JavaScript controller.

module Amelie.Controller.Script
  (handle)
  where

import Amelie.Controller  (outputText)
import Amelie.Model
import Amelie.View.Script (script)

import Snap.Core          (modifyResponse,setContentType)

handle :: Controller ()
handle = do
  modifyResponse $ setContentType "text/javascript"
  outputText $ script
