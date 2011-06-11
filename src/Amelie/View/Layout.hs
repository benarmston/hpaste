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
import           Text.Blaze.Html5            as H hiding (map,nav)
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
        nav
        logo
        pageBody
        foot
    
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

-- | Navigation.
nav :: Html
nav = do
  H.div ! aClass "nav" $ do
    a ! A.href "/activity" $ "Changelog"

-- | Page footer.
foot :: Html
foot = H.div ! aClass "footer" $ p $
  lnk "http://github.com/chrisdone/amelie" "Web site source code on Github"
  //
  lnk "http://book.realworldhaskell.org/" "Real World Haskell"
  //
  lnk "http://haskell.org/" "Haskell.org"
  //
  lnk "http://planet.haskell.org/" "Planet Haskell"

    where lnk url t = href (url :: String) (t :: String)
          left // right = do _ <- left; (" / " :: Html); right
