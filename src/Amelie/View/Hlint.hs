{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Show hlint suggestions.

module Amelie.View.Hlint
 (viewHints)
  where

import Amelie.View.Html

import Data.List (intersperse)
import Language.Haskell.HLint
import Prelude                hiding ((++))
import Text.Blaze.Html5       as H hiding (map)

-- | Show hlint hints for a Haskell paste.
viewHints :: [Suggestion] -> Html
viewHints = mapM_ showHint where
  showHint hint =
    section $ pre $ sequence_ $ intersperse br $ map toHtml lns
    where section = case suggestionSeverity hint of
                      Ignore  -> \_ -> return ()
                      Warning -> warnNoTitleSection
                      Error   -> errorNoTitleSection
          lns = lines $ clean $ show hint
          clean = dropWhile (==':') . dropWhile (/=':')
