{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Paste controller.

module Amelie.Controller.Steps
  (handle)
  where

import Amelie.Types

import Amelie.Controller
import Amelie.Controller.Paste (getPasteId)
import Amelie.Controller.Cache (cache)
import Amelie.Model
import Amelie.Model.Channel    (getChannels)
import Amelie.Model.Language   (getLanguages)
import Amelie.Model.Paste
import Amelie.Types.Cache      as Key
import Amelie.View.Steps       (page,exprFormlet)

import Data.Maybe
import Prelude                 hiding ((++))
import Snap.Types
import Text.Formlet
import Control.Applicative
import Data.Text               (unpack)
import Text.Blaze.Html5        as H hiding (output)

-- @ label stepsPage
-- @ do Show code steps.
-- @ trigger getPasteById
-- | Handle the paste page.
handle :: Controller ()
handle = do
  pid <- getPasteId
  justOrGoHome pid $ \(pid :: Integer) -> do
      (form,expr) <- exprForm
      html <- cache (Key.Steps pid expr) $ do
        paste <- model $ getPasteById (fromIntegral pid)
        case paste of
          Nothing -> return Nothing
          Just paste -> do
            hints <- model $ getHints (pasteId paste)
            steps <- model $ generateSteps (pastePaste paste) expr
            pastes <- model $ getAnnotations (fromIntegral pid)
            ahints <- model $ mapM (getHints.pasteId) pastes
            chans <- model $ getChannels
            langs <- model $ getLanguages
            return $ Just $ page StepsPage {
              spChans       = chans
            , spLangs       = langs
            , spAnnotations = pastes
            , spHints       = hints
            , spSteps       = steps
            , spPaste       = paste
            , spAnnotationHints = ahints
            , spForm = form
            }
      justOrGoHome html outputText

-- | Control paste editing / submission.
exprForm :: Controller (Html,String)
exprForm = do
  params <- getParams
  submitted <- isJust <$> getParam "submit"
  let formlet = ExprFormlet {
          efSubmitted = submitted
        , efParams    = params
        }
      (getValue,_) = exprFormlet formlet
      value = formletValue getValue params
      (_,html) = exprFormlet formlet
      val = either (const Nothing) Just $ value
  return (html,maybe "" unpack val)
