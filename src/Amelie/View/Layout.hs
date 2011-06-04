{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Page layout.

module Amelie.View.Layout
  (layoutPage)
  where

import           Amelie.Types

import           Text.Blaze.Html5            as H hiding (map)
import qualified Text.Blaze.Html5.Attributes as A

-- | Render the page in a layout.
layoutPage :: Page -> Html
layoutPage Page{..} = do
  docTypeHtml $ do
    html $ do
      meta ! A.httpEquiv "Content-Type" ! A.content "text/html; charset=UTF-8"
      title $ text $ pageTitle
    body $
      wrap $ do
        logo
        pageBody

-- | Show the hpaste logo.
logo :: Html
logo = do
  img ! A.src "/css/hpaste.png"

-- | Layout wrapper.
wrap :: Html -> Html
wrap x = H.div ! A.class_ "amelie-wrap" $ x
