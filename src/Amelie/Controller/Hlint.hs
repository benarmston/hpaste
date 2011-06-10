{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Generate hlint hints.

module Amelie.Controller.Hlint
       (getHints)
       where

import Amelie.Types

import Control.Monad
import Control.Monad.IO
import Data.Text.IO           as T (writeFile)
import Language.Haskell.HLint
import System.Directory
import System.FilePath

-- | Get hints for a Haskell paste from hlint.
getHints :: Paste -> Controller [Suggestion]
getHints Paste{pasteId=(fromIntegral -> pid :: Integer),..} = io $ do
  tmpdir <- getTemporaryDirectory
  let tmp = tmpdir </> show pid ++ ".hs"
  exists <- doesFileExist tmp
  unless exists $ T.writeFile tmp $ pastePaste
  hints <- hlint [tmp,"--quiet","--ignore=Parse error"]
  return hints
