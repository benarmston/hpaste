{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Home page view.

module Amelie.View.Home
  (page)
  where

import           Amelie.Types
import           Amelie.View.Layout

import           Data.Text                   (Text)
import           Text.Blaze.Html5            as H hiding (map)
import qualified Text.Blaze.Html5.Attributes as A

-- | Render the home page.
page :: [Paste] -> Html
page ps =
  layoutPage $ Page {
    pageTitle = "λ Knights!"
  , pageBody = latest ps
  }

-- | View the latest pastes.
latest :: [Paste] -> Html
latest ps =
  table $ do
    tr $ do th $ toHtml ("Title" :: Text) 
            th $ toHtml ("Author" :: Text)
    pastes ps
    where pastes = mapM_ $ \Paste{..} -> tr $ do
                     td $ toHtml pasteTitle
                     td $ toHtml pasteAuthor
