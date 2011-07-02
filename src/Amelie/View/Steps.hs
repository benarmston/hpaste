{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Paste steps view.

module Amelie.View.Steps
  (page
  ,exprFormlet)
  where

import           Amelie.Types
import           Amelie.View.Highlight
import           Amelie.View.Hlint           (viewHints)
import           Amelie.View.Html
import           Amelie.View.Layout
import           Amelie.View.Paste           (pasteLink)

import           Control.Monad
import           Data.Monoid.Operator        ((++))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Lazy              (fromStrict)
import           Prelude                     hiding ((++),div)
import           Text.Blaze.Html5            as H hiding (map)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Formlet

-- | Render the steps page.
page :: StepsPage -> Html
page StepsPage{spPaste=p@Paste{..},..} =
  layoutPage $ Page {
    pageTitle = pasteTitle
  , pageBody = viewPaste spForm p spHints spSteps
  , pageName = "steps"
  }

-- | View a paste's details and content.
viewPaste :: Html -> Paste -> [Hint] -> [Text] -> Html
viewPaste form paste@Paste{..} hints steps = do
  case pasteParent of
    Nothing -> return ()
    Just{}  -> let an = "a" ++ show (fromIntegral pasteId :: Integer)
               in a ! A.name (toValue an) $ return ()
  pasteDetails paste
  pasteContent paste
  stepsForm form
  viewSteps steps
  viewHints hints

stepsForm :: Html -> Html
stepsForm form =
  lightNoTitleSection $
    div ! aClass "steps-expr" $
      form

-- | A formlet for expr submission / editing.
exprFormlet :: ExprFormlet -> (Formlet Text,Html)
exprFormlet ExprFormlet{..} =
  let frm = form $ do
        formletHtml exprSubmit efParams
        submitInput "submit" "Submit"
  in (exprSubmit,frm)

exprSubmit :: Formlet Text
exprSubmit = req (textInput "expr" "Expression" Nothing)

viewSteps :: [Text] -> Html
viewSteps steps =
  lightSection "Steps (displaying 50 max.)" $
    div ! aClass "steps" $ do
      highlightHaskell $ T.intercalate "\n\n" steps

-- | List the details of the page in a dark section.
pasteDetails :: Paste -> Html
pasteDetails paste@Paste{..} =
  darkNoTitleSection $ do
    pasteNav
    h2 $ toHtml $ fromStrict pasteTitle
    ul ! aClass "paste-specs" $ do
      detail "Paste" $ pasteLink paste $ "#" ++ show pasteId
      detail "Author" $ pasteAuthor
    clear

    where detail title content = do
            li $ do strong (title ++ ":"); toHtml content

-- | Individual paste navigation.
pasteNav :: Html
pasteNav =
  H.div ! aClass "paste-nav" $ do
    href ("/stepeval" :: Text)
         ("About evaluation step support" :: Text)

-- | Show the paste content with highlighting.
pasteContent :: Paste -> Html
pasteContent paste =
  lightNoTitleSection $ highlightHaskell (pastePaste paste)
