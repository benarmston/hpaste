{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Diff page controller.

module Amelie.Controller.Diff
  (handle)
  where

import Amelie.Controller
import Amelie.Controller.Paste (withPasteKey)
import Amelie.Model
import Amelie.View.Diff        (page)

-- | Diff one paste with another.
handle :: Controller ()
handle = do
  withPasteKey "this" $ \this ->
    withPasteKey "that" $ \that ->
      output $ page this that
