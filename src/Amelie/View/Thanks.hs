{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Thanks view.

module Amelie.View.Thanks
  (page)
  where

import Amelie.Types
import Amelie.View.Html
import Amelie.View.Layout

import Data.String
import Data.Text              (Text)
import Prelude                hiding ((++))
import Text.Blaze.Html5       as H hiding (map)

-- | Render the thanks5 page.
page :: String -> String -> Html
page title msg =
  layoutPage $ Page {
    pageTitle = fromString title
  , pageBody  = thanks title msg
  , pageName  = "thanks"
  }

thanks :: String -> String -> Html
thanks title msg = do
  darkSection (fromString title) $ do
    p $ toHtml msg
    p $ href ("/" :: Text)
             ("Go back home" :: Text)
