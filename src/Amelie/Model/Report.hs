{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | Report model.

module Amelie.Model.Report
  (getReports,createReport)
  where

import Amelie.Types
import Amelie.Model

import Control.Monad
import Prelude                      hiding ((++))

-- @ label getReports
-- @ task Get reports.
-- | Get the reports.
getReports :: Model [Paste]
getReports =
  queryNoParams ["SELECT *"
                ,"FROM report"
                ,"ORDER BY id DESC"
                ,"LIMIT 20"]

-- @ label createReport
-- @ task Create report.
-- | Create a new report.
createReport :: ReportSubmit -> Model (Maybe ReportId)
createReport ReportSubmit{..} = do
  res <- single ["INSERT INTO report"
                ,"(paste,comments)"
                ,"VALUES"
                ,"(?,?)"
                ,"returning id"]
                (rsPaste,rsComments)
  return res
