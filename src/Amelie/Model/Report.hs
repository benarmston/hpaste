{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | Report model.

module Amelie.Model.Report
  (getSomeReports,createReport,countReports)
  where

import Amelie.Types
import Amelie.Model

import Control.Monad
import Data.Maybe
import Data.Monoid.Operator ((++))
import Prelude              hiding ((++))

-- | Get some paginated reports.
getSomeReports :: Pagination -> Model [Report]
getSomeReports Pagination{..} =
  queryNoParams ["SELECT created,paste,comments"
                ,"FROM report"
                ,"ORDER BY id DESC"
                ,"OFFSET " ++ show (max 0 (pnPage - 1) * pnLimit)
                ,"LIMIT " ++ show pnLimit]

-- | Count reports.
countReports :: Model Integer
countReports = do
  rows <- singleNoParams ["SELECT COUNT(*)"
                         ,"FROM report"]
  return $ fromMaybe 0 rows

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
