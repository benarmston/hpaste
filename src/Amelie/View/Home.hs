{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Home page view.

module Amelie.View.Home
  (page)
  where

import           Amelie.Types
import           Amelie.View.Layout

import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text)
import           Data.Time.Show              (showDateTime)
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
latest ps = do
  h2 "Latest pastes"
  table $ do
    tr $ mapM_ (th . toHtml) $ words "Title Author When Language Channel"
    pastes ps

    where pastes = mapM_ $ \Paste{..} -> tr $ do
                     td $ toHtml pasteTitle
                     td $ toHtml pasteAuthor
                     td $ toHtml $ showDateTime $ pasteDate
                     td $ toHtml $ fromMaybe "-" pasteLanguage
                     td $ toHtml $ fromMaybe "-" pasteChannel
