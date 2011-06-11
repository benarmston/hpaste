{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Browse page view.

module Amelie.View.Browse
  (page)
  where

import Amelie.Types
import Amelie.View.Html
import Amelie.View.Layout
import Amelie.View.Paste  (pasteLink)

import Data.Time.Show     (showDateTime)
import Prelude            hiding ((++))
import Text.Blaze.Html5   as H hiding (map)

-- | Render the browse page.
page :: Pagination -> [Channel] -> [Language] -> [Paste] -> Html
page pn chans langs ps =
  layoutPage $ Page {
    pageTitle = "Browse pastes"
  , pageBody = browse pn chans langs ps
  , pageName = "browse"
  }

-- | View the paginated pastes.
browse :: Pagination -> [Channel] -> [Language] -> [Paste] -> Html
browse pn channels languages ps = do
  darkSection "Latest pastes" $ do
    paginate pn $ do
      table ! aClass "latest-pastes" $ do
        tr $ mapM_ (th . toHtml) $ words "Title Author When Language Channel"
        pastes ps

    where pastes = mapM_ $ \paste@Paste{..} -> tr $ do
                     td $ pasteLink paste pasteTitle
                     td $ toHtml pasteAuthor
                     td $ toHtml $ showDateTime $ pasteDate
                     td $ showLanguage languages pasteLanguage
                     td $ showChannel channels pasteChannel
