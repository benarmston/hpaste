{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Paste views.

module Amelie.View.Paste
  (pasteFormlet)
  where

import Amelie.Types

import Control.Applicative    ((<$>),(<*>))
import Control.Monad          (when)
import Snap.Types             (Params)
import Text.Blaze.Html5       as H
import Text.Blaze.Html5.Extra
import Text.Formlet
import Data.Text (Text)

-- | A formlet for paste submission / editing.
pasteFormlet :: Params -> Bool -> [Text] -> (Formlet PasteSubmit,Html)
pasteFormlet params submitted errors =
  let form = postForm $ do
        formletHtml formlet params
        submitInput "submit" "Submit"
        when submitted $
          when (not (null errors)) $
            mapM_ (p . toHtml) errors
  in (formlet,form)
  
    where formlet =
            PasteSubmit <$> req (textInput "title" "Title")
                        <*> req (textInput "author" "Author")
                        <*> opt (dropInput "language" "Language")
                        <*> opt (dropInput "channel" "Channel")
                        <*> req (areaInput "paste" "Paste")
