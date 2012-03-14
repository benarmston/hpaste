{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reported page controller.

module Amelie.Controller.Reported
  (handle)
  where

import Amelie.Controller     (output,getPagination)
import Amelie.Model
import Amelie.Model.Report   (getSomeReports,countReports)
import Amelie.View.Reported  (page)

-- | List the reported pastes.
handle :: Controller ()
handle = do
  pn <- getPagination
  total <- model countReports
  reports <- model $ getSomeReports pn
  let pn' = pn { pnRoot = "/reported"
               , pnResults = fromIntegral (length reports)
               , pnTotal = total }
  output $ page pn' reports
