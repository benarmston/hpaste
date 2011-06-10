{-# OPTIONS -Wall #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The paste type.

module Amelie.Types.Paste
       (Paste(..)
       ,PasteSubmit(..)
       ,PasteFormlet(..))
       where

import Amelie.Types.Newtypes
import Amelie.Types.Language
import Amelie.Types.Channel

import Data.Text                               (Text,pack)
import Data.Time                               (UTCTime,zonedTimeToUTC)
import Database.PostgreSQL.Simple.QueryResults (QueryResults(..))
import Snap.Types                              (Params)
import Text.Blaze                              (ToHtml(..),toHtml)

-- | A paste.
data Paste = Paste {
   pasteId       :: PasteId
  ,pasteTitle    :: Text
  ,pasteDate     :: UTCTime
  ,pasteAuthor   :: Text
  ,pasteLanguage :: Maybe LanguageId
  ,pasteChannel  :: Maybe ChannelId
  ,pastePaste    :: Text
  ,pasteViews    :: Integer 
  ,pasteParent   :: Maybe PasteId
} deriving Show

-- | A paste submission or edit.
data PasteSubmit = PasteSubmit {
   pasteSubmitId       :: Maybe PasteId
  ,pasteSubmitTitle    :: Text
  ,pasteSubmitAuthor   :: Text
  ,pasteSubmitLanguage :: Maybe LanguageId
  ,pasteSubmitChannel  :: Maybe ChannelId
  ,pasteSubmitPaste    :: Text
  ,pasteSubmitSpamTrap :: Maybe Text
} deriving Show

instance ToHtml Paste where
  toHtml paste@Paste{..} = toHtml $ pack $ show paste

instance QueryResults Paste where
  convertResults field values = Paste {
      pasteTitle = title
    , pasteAuthor = author
    , pasteLanguage = language
    , pasteChannel = channel
    , pastePaste = content
    , pasteDate = zonedTimeToUTC date
    , pasteId = pid
    , pasteViews = views
    , pasteParent = parent
    }
    where (pid,title,content,author,date,views,language,channel,parent) =
            convertResults field values

data PasteFormlet = PasteFormlet {
   pfSubmitted :: Bool
 , pfErrors :: [Text]
 , pfParams :: Params
 , pfLanguages :: [Language]
 , pfChannels :: [Channel]
}
