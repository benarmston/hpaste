{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Report view.

module Amelie.View.Report
  (page,reportFormlet)
  where

import           Amelie.Types
import           Amelie.View.Highlight
import           Amelie.View.Html
import           Amelie.View.Layout

import           Data.Monoid.Operator        ((++))
import           Data.Text                   (Text)
import           Prelude                     hiding ((++))
import           Text.Blaze.Html5            as H hiding (map)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Formlet

-- | Render the page page.
page :: Html -> Paste -> Html
page form paste =
  layoutPage $ Page {
    pageTitle = "Report a paste"
  , pageBody = do reporting form; viewPaste paste
  , pageName = "paste"
  }
  
reporting :: Html -> Html
reporting form = do
  lightSection "Report a paste" $ do
    p $ do "Please state any comments regarding the paste:"
    H.form ! A.method "post" $ do
      form

-- | View a paste's details and content.
viewPaste :: Paste -> Html
viewPaste Paste{..} = do
  pasteDetails pasteTitle
  pasteContent pastePaste

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

-- | A formlet for report submission / editing.
reportFormlet :: ReportFormlet -> (Formlet Text,Html)
reportFormlet ReportFormlet{..} =
  let frm = form $ do
        formletHtml reportSubmit rfParams
        submitInput "submit" "Submit"
  in (reportSubmit,frm)

reportSubmit :: Formlet Text
reportSubmit = req (textInput "report" "Comments" Nothing)
