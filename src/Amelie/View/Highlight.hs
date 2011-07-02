{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Code highlighting.

module Amelie.View.Highlight
 (highlightPaste
 ,highlightHaskell)
  where

import Amelie.Types
import Amelie.View.Html

import Data.List                     (find)
import Data.Text                     (Text,unpack)
import Language.Haskell.HsColour.CSS (hscolour)
import Prelude                       hiding ((++))
import Text.Blaze.Html5              as H hiding (map)

-- | Syntax highlight the paste.
highlightPaste :: [Language] -> Paste -> Html
highlightPaste langs Paste{..} =
  H.table ! aClass "code" $
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
