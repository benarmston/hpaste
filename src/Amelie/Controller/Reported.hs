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

-- @ label reportedPage
-- @ do Reported pastes.
-- @ trigger getPastes
-- @ next pastePage
-- @ next reportedPage
handle :: Controller ()
handle = do
  pn <- getPagination
  total <- model countReports
  reports <- model $ getSomeReports pn
  let pn' = pn { pnRoot = "/reported"
               , pnResults = fromIntegral (length reports)
               , pnTotal = total }
  output $ page pn' reports
