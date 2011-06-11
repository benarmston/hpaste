{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Activity page view.

module Amelie.View.Activity
  (page)
  where

import Amelie.Types
import Amelie.View.Html
import Amelie.View.Layout

import Control.Monad
import Data.Text          (Text)
import Prelude            hiding ((++))
import Text.Blaze.Html5   as H hiding (map)

-- | Render the activity page.
page :: [Commit] -> Html
page commits =
  layoutPage $ Page {
    pageTitle = "Development activity"
  , pageBody = activity commits
  , pageName = "activity"
  }

-- | View the paginated pastes.
activity :: [Commit] -> Html
activity commits = do
  darkSection "Development activity" $ do
    p $ do "Repository: "
           href ("https://github.com/chrisdone/amelie/" :: Text)
                ("https://github.com/chrisdone/amelie/" :: Text)
  forM_ commits $ \Commit{..} -> do
    lightSection commitTitle $ do
      p $ toHtml $ show commitDate
      p $ href commitLink ("Go to diff" :: Text)
