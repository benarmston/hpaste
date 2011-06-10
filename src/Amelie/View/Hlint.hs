{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Show hlint suggestions.

module Amelie.View.Hlint
 (viewHints)
  where

import Language.Haskell.HLint
import Prelude                hiding ((++))
import Text.Blaze.Html5       as H hiding (map)

-- | Show hlint hints for a Haskell paste.
viewHints :: [Suggestion] -> Html
viewHints _ =
 return ()
