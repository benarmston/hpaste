{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Edit paste view.

module Amelie.View.Edit
  (page)
  where

import Amelie.Types
import Amelie.View.Html
import Amelie.View.Layout

import Data.Monoid.Operator ((++))
import Prelude              hiding ((++))
import Text.Blaze.Html5     as H hiding (map)
import Data.Text.Lazy

-- | Render the create edit paste page.
page :: Paste -> Html -> Html
page Paste{..} form =
  layoutPage $ Page {
    pageTitle = "Edit: " ++ pasteTitle
  , pageBody = lightSection ("Edit: " ++ fromStrict pasteTitle) form
  , pageName = "edit"
  }
