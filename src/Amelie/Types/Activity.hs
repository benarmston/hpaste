module Amelie.Types.Activity where

import Data.Text.Lazy (Text)
import Data.Time      (UTCTime)

data Commit = Commit {
  commitTitle :: Text
 ,commitContent :: Text
 ,commitDate :: UTCTime
 ,commitLink :: Text
} deriving Show