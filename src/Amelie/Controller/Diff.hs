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

-- @ label diffPage
-- @ do Diff two pastes.
-- @ trigger getPasteById
-- @ trigger getPasteById
handle :: Controller ()
handle = do
  withPasteKey "this" $ \this ->
    withPasteKey "that" $ \that ->
      output $ page this that
