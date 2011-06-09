{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Code highlighting.

module Amelie.View.Highlight
  -- (highlightPaste)
  where

import Amelie.Types

import Data.Char
import Data.List                        (find)
import Data.Monoid.Operator             ((++))
import Data.Text                        (unpack,replace)
import Data.Text.Encoding               (encodeUtf8)
import Prelude                          hiding ((++))
import Text.Blaze.Html5                 as H hiding (map)
import Text.Highlighter.Formatters.Html (format)
import Text.Highlighter.Lexer           (runLexer)
import Text.Highlighter.Lexers
import Text.Highlighter.Types

-- | Syntax highlight the paste.
highlightPaste :: [Language] -> Paste -> Html
highlightPaste langs Paste{..} =
  case lang >>= ((`lookupLang` (map snd lexers)) . unpack . languageName) of
    Nothing -> pre $ toHtml pastePaste
    Just lexer ->
      case runLexer lexer (encodeUtf8 (clean pastePaste ++ "\n")) of
        Right tokens -> format True tokens
        _            -> pre $ toHtml pastePaste
        
  where lang = find ((==pasteLanguage) . Just . languageId) langs
        lookupLang name = find $ \lexer -> lower (lName lexer) == lower name
        lower = map toLower
        clean = replace "\r\n" "\n"
