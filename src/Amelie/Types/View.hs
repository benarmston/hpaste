module Amelie.Types.View
  (Pagination(..))
  where

import Data.Text (Text)

-- | Pagination data.
data Pagination = Pagination {
   pnPage :: Integer
 , pnLimit :: Integer
 , pnRoot :: Text
 , pnResults :: Integer
 , pnTotal :: Integer
} deriving Show
