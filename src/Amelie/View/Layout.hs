{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Page layout.

module Amelie.View.Layout
  (layoutPage)
  where

import           Amelie.Types
import           Amelie.View.Html

import           Data.Monoid.Operator        ((++))
import           Prelude                     hiding ((++))
import           Text.Blaze.Html5            as H hiding (map)
import qualified Text.Blaze.Html5.Attributes as A

-- | Render the page in a layout.
layoutPage :: Page -> Html
layoutPage Page{..} = do
  docTypeHtml $ do
    html $ do
      meta ! A.httpEquiv "Content-Type" ! A.content "text/html; charset=UTF-8"
      link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/css/amelie.css"
      js "jquery.js"
      js "amelie.js"
      title $ toHtml $ pageTitle ++ " :: hpaste — Haskell Pastebin"
    body ! A.id (toValue pageName) $
      wrap $ do
        logo
        pageBody
    
    where js s = script ! A.type_ "text/javascript"
                        ! A.src ("/js/" ++ s) $
                        return ()

-- | Show the hpaste logo.
logo :: Html
logo = do
  a ! A.href "/" ! A.title "Back to home" $ do
    img ! aClass "logo" ! A.src "/css/hpaste.png"

-- | Layout wrapper.
wrap :: Html -> Html
wrap x = H.div ! aClass "wrap" $ x
