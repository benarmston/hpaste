{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Stepeval explanation view.

module Amelie.View.Stepeval
  (page)
  where

import           Amelie.Types
import           Amelie.View.Highlight
import           Amelie.View.Hlint
import           Amelie.View.Html
import           Amelie.View.Layout

import           Data.Monoid.Operator        ((++))
import           Data.Text                   (Text)
import           Language.Haskell.HLint
import           Prelude                     hiding ((++))
import           Text.Blaze.Html5            as H hiding (map)

-- | Render the page page.
page :: StepevalPage -> Html
page StepevalPage{..} =
  layoutPage $ Page {
    pageTitle = "Stepeval support"
  , pageBody = do explanation
                  viewPaste sePaste seHints
  , pageName = "paste"
  }
  
explanation :: Html
explanation = do
  lightSection "Stepeval" $ do
    p $ do "A program/library for evaluating "
           "a Haskell expression step-by-step. This web site uses it "
           "for stepping through provided expressions."
    p $ href ("https://github.com/benmachine/stepeval" :: Text)
             ("Repository for Stepeval" :: Text)
    p $ do "Stepeval comes with a simple Prelude of pure functions "
           "(see below) that can be used when stepping through "
           "expressions. This may be expanded upon in the future."
    p $ do "This web site will automatically include declarations "
           "from the paste as the expression to be evaluted."

-- | View a paste's details and content.
viewPaste :: Text -> [Suggestion] -> Html
viewPaste paste hints = do
  pasteDetails "Stepeval Prelude"
  pasteContent paste
  viewSuggestions hints

-- | List the details of the page in a dark section.
pasteDetails :: Text -> Html
pasteDetails title =
  darkNoTitleSection $ do
    pasteNav
    h2 $ toHtml title
    ul ! aClass "paste-specs" $ do
      detail "Language" $ "Haskell"
      detail "Raw" $ href ("/stepeval/raw" :: Text)
                          ("View raw link" :: Text)
    clear

    where detail title content = do
            li $ do strong (title ++ ":"); content

-- | Individual paste navigation.
pasteNav :: Html
pasteNav =
  H.div ! aClass "paste-nav" $ do
    href ("https://github.com/benmachine/stepeval" :: Text)
         ("Go to stepeval project" :: Text)

-- | Show the paste content with highlighting.
pasteContent :: Text -> Html
pasteContent paste =
  lightNoTitleSection $
    highlightHaskell paste