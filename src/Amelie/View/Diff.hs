{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Diff page view.

module Amelie.View.Diff
  (page)
  where

import           Amelie.Types
import           Amelie.View.Html
import           Amelie.View.Layout
import           Amelie.View.Paste    (pasteLink)

import           Control.Monad
import           Data.Algorithm.Diff
import           Data.Monoid.Operator ((++))
import qualified Data.Text            as T
import           Data.Text.Lazy       (pack)
import           Prelude              hiding ((++))
import           Text.Blaze.Html5     as H hiding (map)

-- | Render the diff page.
page :: Paste -> Paste -> Html
page this that =
  layoutPage $ Page {
    pageTitle = "Diff two pastes"
  , pageBody = diffBody this that
  , pageName = "diff"
  }

-- | View the diff between the two pastes.
diffBody :: Paste -> Paste -> Html
diffBody this that = do
  darkSection ("Diff: " ++ pid1 ++ " / " ++ pid2) $ do
    pasteMention this pid1
    pasteMention that pid2
  lightNoTitleSection $ do
    viewDiff this that
  
    where pasteMention paste pid = p $ do
            pasteLink paste pid
            ": "
            toHtml $ pasteTitle paste
          pid1 = pack (show (pasteId this))
          pid2 = pack (show (pasteId that))

-- | View the diff between the two pastes.
viewDiff :: Paste -> Paste -> Html
viewDiff this that = do
  H.table ! aClass "code" $
    td $
      pre $ do
        forM_ groups $ \(indicator,lines) -> do
          let (ind,prefix) =
                case indicator of
                  B -> ("diff-both","  ")
                  F -> ("diff-first","- ")
                  S -> ("diff-second","+ ")
              lins = map (prefix++) lines
          H.div ! aClass ind $ toHtml $ T.unlines $ lins

    where groups = getGroupedDiff lines1 lines2
          lines1 = T.lines (pastePaste this)
          lines2 = T.lines (pastePaste that)
