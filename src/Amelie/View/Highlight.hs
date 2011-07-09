{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Code highlighting.

module Amelie.View.Highlight
 (highlightPaste
 ,highlightHaskell)
  where

import           Amelie.Types
import           Amelie.View.Html

import           Control.Monad
import           Data.List                     (find)
import           Data.Monoid.Operator
import           Data.Text                     (Text,unpack,pack)
import qualified Data.Text                     as T
import           Language.Haskell.HsColour.CSS (hscolour)
import           Prelude                       hiding ((++))
import           Text.Blaze.Html5              as H hiding (map)
import qualified Text.Blaze.Html5.Attributes   as A

-- | Syntax highlight the paste.
highlightPaste :: [Language] -> Paste -> Html
highlightPaste langs Paste{..} =
  H.table ! aClass "code" $ do
    td ! aClass "line-nums" $ do
      pre $
        forM_ [1..length (T.lines pastePaste)] $ \i -> do
          let name = "line" ++ pack (show i)
          href ("#" ++ name) (toHtml i) ! A.id (toValue name) ! A.name (toValue name)
          "\n"
    td $
      case lang of
        Just (Language{languageName="haskell"}) ->
          preEscapedString $ hscolour False (unpack pastePaste)
        _ -> pre $ toHtml pastePaste

  where lang = find ((==pasteLanguage) . Just . languageId) langs

highlightHaskell :: Text -> Html
highlightHaskell paste =
  H.table ! aClass "code" $
    td $ preEscapedString $ hscolour False (unpack paste)
