{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

-- | Generate hlint hints.

module Amelie.Controller.Hlint
       (getHints)
       where

import Amelie.Types

import Control.Monad.IO
import Data.Text              (Text,unpack)
import Language.Haskell.HLint

-- | Get hints for a Haskell paste from hlint.
getHints :: Text -> Controller [Suggestion]
getHints text = do
  hints <- io $ hlint [unpack text,"--quiet"]
  return hints
