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
import Amelie.Model.Paste
import Amelie.Types.Cache      as Key
import Amelie.View.Paste       (pasteFormlet,page)

import Control.Applicative
import Control.Monad           ((>=>))
import Data.ByteString.UTF8    (toString)
import Data.Maybe
import Data.Monoid.Operator    ((++))
import Data.String             (fromString)
import Data.Text               (Text)
import Prelude                 hiding ((++))
import Safe
import Snap.Types
import Text.Blaze.Html5        as H hiding (output)
import Text.Formlet

-- | Handle the paste page.
handle :: Controller ()
handle = do
  pid <- (fmap toString >=> readMay) <$> getParam "id"
  justOrGoHome pid $ \(pid :: Integer) -> do
      html <- cache (Key.Paste pid) $ do
        paste <- model $ getPasteById (fromIntegral pid)
        case paste of
          Nothing -> return Nothing
          Just paste -> do
            hints <- model $ getHints (pasteId paste)
            pastes <- model $ getAnnotations (fromIntegral pid)
            chans <- model $ getChannels
            langs <- model $ getLanguages
            return $ Just $ page PastePage {
              ppChans       = chans
            , ppLangs       = langs
            , ppAnnotations = pastes
            , ppHints       = hints
            , ppPaste       = paste
            }
      justOrGoHome html outputText

-- | Control paste editing / submission.
pasteForm :: [Channel] -> [Language] -> Maybe Text -> Controller Html
pasteForm channels languages defChan = do
  params <- getParams
  submitted <- isJust <$> getParam "submit"
  let formlet = PasteFormlet {
          pfSubmitted = submitted
        , pfErrors    = []
        , pfParams    = params
        , pfChannels  = channels
        , pfLanguages = languages
        , pfDefChan   = defChan
        }
      (getValue,_) = pasteFormlet formlet
      value = formletValue getValue params
      errors = either id (const []) value
      (_,html) = pasteFormlet formlet { pfErrors = errors }
      val = either (const Nothing) Just $ value
  case val of
    Nothing -> return ()
    Just PasteSubmit{pasteSubmitSpamTrap=Just{}} -> goHome
    Just paste -> do
      resetCache Key.Home
      pid <- model $ createOrEdit paste
      maybe (return ()) redirectToPaste pid
  return html

-- | Redirect to the paste's page.
redirectToPaste :: PasteId -> Controller ()
redirectToPaste (PasteId pid) =
  redirect $ "/" ++ fromString (show pid)
