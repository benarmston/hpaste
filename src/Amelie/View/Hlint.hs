{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Show hlint suggestions.

module Amelie.View.Hlint
 (viewHints
 ,viewSuggestions)
  where

import Amelie.Types
import Amelie.View.Html

import Data.List              (intersperse)
import Language.Haskell.HLint
import Prelude                hiding ((++))
import Text.Blaze.Html5       as H hiding (map)

-- | Show hlint hints for a Haskell paste.
viewHints :: [Hint] -> Html
viewHints = mapM_ showHint where
  showHint hint =
    section $
      pre ! aClass "hint" $ sequence_ $ intersperse br $ map toHtml lns
    where section = case hintType hint of
                      Ignore  -> \_ -> return ()
                      Warning -> warnNoTitleSection
                      Error   -> errorNoTitleSection
          lns = lines $ clean $ hintContent hint
          clean = dropWhile (==':') . dropWhile (/=':')

viewSuggestions :: [Suggestion] -> Html
viewSuggestions = viewHints . map toHint where
  toHint s = Hint (suggestionSeverity s)
                  (show s)
