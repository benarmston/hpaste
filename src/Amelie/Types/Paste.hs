{-# OPTIONS -Wall #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The paste type.

module Amelie.Types.Paste
       (Paste(..)
       ,PasteSubmit(..))
       where

import Data.Text                               (Text,pack)
import Data.Time                               (UTCTime,zonedTimeToUTC)
import Database.PostgreSQL.Simple.QueryResults (QueryResults(..))
import Text.Blaze                              (ToHtml(..),toHtml)

-- | A paste.
data Paste = Paste {
   pasteTitle    :: Text
  ,pasteDate     :: UTCTime
  ,pasteAuthor   :: Text
  ,pasteLanguage :: Maybe Text
  ,pasteChannel  :: Maybe Text
  ,pastePaste    :: Text
} deriving Show

-- | A paste submission or edit.
data PasteSubmit = PasteSubmit {
   pasteSubmitTitle    :: Text
  ,pasteSubmitAuthor   :: Text
  ,pasteSubmitLanguage :: Maybe Text
  ,pasteSubmitChannel  :: Maybe Text
  ,pasteSubmitPaste    :: Text
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
    , pasteDate = zonedTimeToUTC date
    }
    where (date,title,author,language,channel,paste) = convertResults field values
