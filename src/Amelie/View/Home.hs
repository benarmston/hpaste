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

import           Data.Maybe                  (fromMaybe)
import           Data.Monoid.Operator        ((++))
import           Data.String                 (fromString)
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

createNew :: Html -> Html
createNew form = do
  H.div ! aClasses ["section","section-light"] $ do
    h2 "Create new paste"
    form

-- | View the latest pastes.
latest :: [Paste] -> Html
latest ps = do
  H.div ! aClasses ["section","section-dark"] $ do
    h2 "Latest pastes"
    table ! A.width "100%" $ do
      tr $ mapM_ (th . toHtml) $ words "Title Author When Language Channel"
      pastes ps

    where pastes = mapM_ $ \Paste{..} -> tr $ do
                     let pid = fromIntegral pasteId :: Integer
                     td $ href pid $ toHtml pasteTitle
                     td $ toHtml pasteAuthor
                     td $ toHtml $ showDateTime $ pasteDate
                     td $ toHtml $ fromMaybe "-" pasteLanguage
                     td $ toHtml $ fromMaybe "-" pasteChannel
            where href pid = H.a ! A.href ("/" ++ fromString (show pid))
                  
