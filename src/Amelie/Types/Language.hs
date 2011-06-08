{-# OPTIONS -Wall #-}

-- | The language type.

module Amelie.Types.Language
       (Language(..))
       where

import Amelie.Types.Newtypes

import Data.Text                               (Text)
import Database.PostgreSQL.Simple.QueryResults (QueryResults(..))

data Language = Language {
  languageId    :: LanguageId
 ,languageName  :: Text
 ,languageTitle :: Text
} deriving Show

instance QueryResults Language where
  convertResults field values = Language {
      languageName = name
    , languageId = lid
    , languageTitle = title
    }
    where (lid,name,title) = convertResults field values
