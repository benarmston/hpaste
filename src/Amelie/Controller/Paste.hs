{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Paste controller.

module Amelie.Controller.Paste
  (pasteForm)
  where

import           Amelie.Types

import           Amelie.View.Paste           (pasteFormlet)

import           Control.Applicative
import           Control.Monad.Error
import           Data.Maybe
import           Prelude                     hiding ((++))
import           Snap.Types
import           Text.Blaze.Html5            as H
import           Text.Formlet

-- | Control paste editing / submission.
pasteForm :: Controller Html
pasteForm = do
  params <- getParams
  submitted <- isJust <$> getParam "submit"
  let (getValue,html) = pasteFormlet params submitted []
      value = formletValue getValue params
      val = either (const Nothing) Just $ value
  return html
