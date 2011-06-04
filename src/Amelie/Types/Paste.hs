{-# OPTIONS -Wall #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The paste type.

module Amelie.Types.Paste
       (Paste(..))
       where

import Data.Text                               (Text,pack)
import Database.PostgreSQL.Simple.QueryResults (QueryResults(..))
import Text.Blaze                              (ToHtml(..),toHtml)

-- | A paste.
data Paste = Paste {
   pasteTitle    :: Text
  ,pasteAuthor   :: Text
  ,pasteLanguage :: Maybe Text
  ,pasteChannel  :: Maybe Text
  ,pastePaste    :: Text
} deriving Show

instance ToHtml Paste where
  toHtml paste@Paste{..} = toHtml $ pack $ show paste

instance QueryResults Paste where
  convertResults field values = Paste {
      pasteTitle = title
    , pasteAuthor = author
    , pasteLanguage = language
    , pasteChannel = channel
    , pastePaste = paste
    }
    where (title,author,language,channel,paste) = convertResults field values
