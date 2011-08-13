module Amelie.Types.Report where

import Amelie.Types.Newtypes                   (PasteId)

import Data.Text                               (Text)
import Data.Time                               (UTCTime,zonedTimeToUTC)
import Database.PostgreSQL.Simple.QueryResults (QueryResults(..))

data Report = Report {
  reportDate :: UTCTime
 ,reportPasteId :: PasteId
 ,reportComments :: Text
} deriving Show

instance QueryResults Report where
  convertResults field values = Report {
      reportDate = zonedTimeToUTC date
    , reportPasteId = paste
    , reportComments = comments
    }
    where (date,paste,comments) = convertResults field values
