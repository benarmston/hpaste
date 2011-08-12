{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-- | Report controller.

module Amelie.Controller.Report
  (handle)
  where

import           Amelie.Controller
import           Amelie.Model
import           Amelie.Model.Paste   (getPasteById)
import           Amelie.Model.Report
import           Amelie.Types
import           Amelie.View.Report
import qualified Amelie.View.Thanks   as Thanks

import           Control.Applicative
import           Data.ByteString.UTF8 (toString)
import           Data.Maybe
import           Data.Monoid.Operator ((++))
import           Data.Text            (unpack)
import           Prelude              hiding ((++))
import           Safe
import           Snap.Types
import           Text.Blaze.Html5     as H hiding (output,map,body)
import           Text.Formlet

-- | Handle the report/delete page.
handle :: Controller ()
handle = do
  pid <- (>>= readMay) . fmap (toString) <$> getParam "id"
  case pid of
    Nothing -> goHome
    Just (pid :: Integer) -> do
      paste <- model $ getPasteById (fromIntegral pid)
      (frm,val) <- exprForm
      case val of
        Just comment -> do
          _ <- model $ createReport ReportSubmit { rsPaste = fromIntegral pid
                                                 , rsComments = comment }
          output $ Thanks.page "Reported" $
                               "Thanks, your comments have " ++
                               "been reported to the administrator."
        Nothing -> maybe goHome (output . page frm) paste

-- | Report form.
exprForm :: Controller (Html,Maybe String)
exprForm = do
  params <- getParams
  submitted <- isJust <$> getParam "submit"
  let formlet = ReportFormlet {
          rfSubmitted = submitted
        , rfParams    = params
        }
      (getValue,_) = reportFormlet formlet
      value = formletValue getValue params
      (_,html) = reportFormlet formlet
      val = either (const Nothing) Just $ value
  return (html,fmap unpack val)
