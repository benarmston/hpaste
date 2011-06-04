{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Paste controller.

module Amelie.Controller.Paste
  (handle
  ,pasteForm)
  where

import Amelie.Types

import Amelie.Model
import Amelie.Model.Paste   (createOrEdit,getPasteById)
import Amelie.View.Paste    (pasteFormlet,page)
import Amelie.Controller

import Control.Applicative
import Data.ByteString.UTF8 (toString)
import Data.Maybe
import Data.Monoid.Operator ((++))
import Data.String          (fromString)
import Prelude              hiding ((++))
import Safe
import Snap.Types
import Text.Blaze.Html5     as H hiding (output)
import Text.Formlet

-- | Handle the paste page.
handle :: Controller ()
handle = do
  pid <- (>>= readMay) . fmap (toString) <$> getParam "id"
  case pid of
    Nothing -> goHome
    Just (pid :: Integer) -> do
      paste <- model $ getPasteById (fromIntegral pid)
      maybe goHome (output . page) paste

  where goHome = redirect "/"

-- | Control paste editing / submission.
pasteForm :: Controller Html
pasteForm = do
  params <- getParams
  submitted <- isJust <$> getParam "submit"
  let (getValue,html) = pasteFormlet params submitted []
      value = formletValue getValue params
      val = either (const Nothing) Just $ value
  case val of
    Nothing    -> return ()
    Just paste -> do
      pid <- model $ createOrEdit paste
      maybe (return ()) redirectToPaste pid
  return html

-- | Redirect to the paste's page.
redirectToPaste :: PasteId -> Controller ()
redirectToPaste (PasteId pid) =
  redirect $ "/" ++ fromString (show pid)
