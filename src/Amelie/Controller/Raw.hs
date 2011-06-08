{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Raw controller.

module Amelie.Controller.Raw
  (handle)
  where

import Amelie.Types

import Amelie.Model
import Amelie.Model.Paste      (getPasteById)

import Control.Applicative
import Data.ByteString.UTF8    (toString)
import Data.Maybe
import Prelude                 hiding ((++))
import Safe
import Snap.Types

-- | Handle the paste page.
handle :: Controller ()
handle = do
  pid <- (>>= readMay) . fmap (toString) <$> getParam "id"
  case pid of
    Nothing -> goHome
    Just (pid :: Integer) -> do
      modifyResponse $ setContentType "text/plain"
      paste <- model $ getPasteById (fromIntegral pid)
      maybe goHome (writeText . pastePaste) paste

  where goHome = redirect "/"
