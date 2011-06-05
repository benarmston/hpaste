{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Home page view.

module Amelie.View.Home
  (page)
  where

import           Amelie.Types
import           Amelie.View.Html
import           Amelie.View.Layout
import           Amelie.View.Paste (pasteLink)

import           Data.Maybe                  (fromMaybe)
import           Data.Time.Show              (showDateTime)
import           Prelude                     hiding ((++))
import           Text.Blaze.Html5            as H hiding (map)
import qualified Text.Blaze.Html5.Attributes as A

-- | Render the home page.
page :: [Paste] -> Html -> Html
page ps form =
  layoutPage $ Page {
    pageTitle = "λ Knights!"
  , pageBody = content ps form
  }

-- | Render the home page body.
content :: [Paste] -> Html -> Html
content ps form = do
  createNew form
  latest ps

-- | Create a new paste section.
createNew :: Html -> Html
createNew = lightSection "Create new paste"

-- | View the latest pastes.
latest :: [Paste] -> Html
latest ps = do
  darkSection "Latest pastes" $
    table ! A.width "100%" $ do
      tr $ mapM_ (th . toHtml) $ words "Title Author When Language Channel"
      pastes ps

    where pastes = mapM_ $ \paste@Paste{..} -> tr $ do
                     td $ pasteLink paste pasteTitle
                     td $ toHtml pasteAuthor
                     td $ toHtml $ showDateTime $ pasteDate
                     td $ toHtml $ fromMaybe "-" pasteLanguage
                     td $ toHtml $ fromMaybe "-" pasteChannel
