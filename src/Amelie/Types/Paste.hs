{-# OPTIONS -Wall -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The paste type.

module Amelie.Types.Paste
       (Paste(..)
       ,PasteSubmit(..)
       ,PasteFormlet(..)
       ,PastePage(..)
       ,Hint(..))
       where

import Amelie.Types.Newtypes
import Amelie.Types.Language
import Amelie.Types.Channel

import Blaze.ByteString.Builder                (toByteString)
import Blaze.ByteString.Builder.Char.Utf8      as Utf8 (fromString)
import Data.Text                               (Text,pack)
import Data.Time                               (UTCTime,zonedTimeToUTC)
import Database.PostgreSQL.Simple.Param        (Param(..),Action(..))
import Database.PostgreSQL.Simple.QueryResults (QueryResults(..))
import Database.PostgreSQL.Simple.Result       (Result(..))
import Language.Haskell.HLint                  (Severity)
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
 , pfDefChan :: Maybe Text
}

data PastePage = PastePage {
    ppPaste :: Paste
  , ppChans :: [Channel]
  , ppLangs :: [Language]
  , ppHints :: [Hint]
  , ppAnnotations :: [Paste]
}

instance Param Severity where
  render = Escape . toByteString . Utf8.fromString . show
  {-# INLINE render #-}

instance Result Severity where
  convert f = read . convert f
  {-# INLINE convert #-}

-- | A hlint (or like) suggestion.
data Hint = Hint {
   hintType    :: Severity
 , hintContent :: String
}

instance QueryResults Hint where
  convertResults field values = Hint {
      hintType = severity
    , hintContent = content
    }
    where (severity,content) = convertResults field values
