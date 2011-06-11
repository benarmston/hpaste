{-# OPTIONS -Wall #-}

-- | HTML caching types.

module Amelie.Types.Cache
       (Key(..)
       ,Cache(..))
       where

import Control.Concurrent.MVar (MVar)
import Data.Map                (Map)
import Data.Text.Lazy          (Text)

data Key =
    Home
  | Paste Integer
  | Activity
    deriving (Eq,Ord)

data Cache =
  Cache {
    cacheMap :: MVar (Map Key Text)
  }
