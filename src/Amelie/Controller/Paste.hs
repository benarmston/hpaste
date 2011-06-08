{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Paste controller.

module Amelie.Controller.Paste
  (handle
  ,pasteForm)
  where

import Amelie.Types

import Amelie.Controller
import Amelie.Controller.Cache (cache,resetCache)
import Amelie.Model
import Amelie.Model.Channel    (getChannels)
import Amelie.Model.Language   (getLanguages)
import Amelie.Model.Paste      (createOrEdit,getPasteById,getAnnotations)
import Amelie.Types.Cache      as Key
import Amelie.View.Paste       (pasteFormlet,page)

import Control.Applicative
import Data.ByteString.UTF8    (toString)
import Data.Maybe
import Data.Monoid.Operator    ((++))
import Data.String             (fromString)
import Prelude                 hiding ((++))
import Safe
import Snap.Types
import Text.Blaze.Html5        as H hiding (output)
import Text.Formlet

-- | Handle the paste page.
handle :: Controller ()
handle = do
  pid <- (>>= readMay) . fmap (toString) <$> getParam "id"
  case pid of
    Nothing -> goHome
    Just (pid :: Integer) -> do
      html <- cache (Key.Paste pid) $ do
        paste <- model $ getPasteById (fromIntegral pid)
        pastes <- model $ getAnnotations (fromIntegral pid)
        chans <- model $ getChannels
        langs <- model $ getLanguages
        return $ flip (page chans langs) pastes <$> paste
      case html of
        Just html -> outputText html
        Nothing   -> goHome

  where goHome = redirect "/"

-- | Control paste editing / submission.
pasteForm :: [Channel] -> [Language] -> Controller Html
pasteForm channels languages = do
  params <- getParams
  submitted <- isJust <$> getParam "submit"
  let formlet = PasteFormlet {
          pfSubmitted = submitted
        , pfErrors    = []
        , pfParams    = params
        , pfChannels  = channels
        , pfLanguages = languages
        }
      (getValue,_) = pasteFormlet formlet
      value = formletValue getValue params
      errors = either id (const []) value
      (_,html) = pasteFormlet formlet { pfErrors = errors }
      val = either (const Nothing) Just $ value
  case val of
    Nothing    -> return ()
    Just paste -> do
      resetCache Key.Home
      pid <- model $ createOrEdit paste
      maybe (return ()) redirectToPaste pid
  return html

-- | Redirect to the paste's page.
redirectToPaste :: PasteId -> Controller ()
redirectToPaste (PasteId pid) =
  redirect $ "/" ++ fromString (show pid)
