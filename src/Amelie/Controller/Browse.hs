{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Browse page controller.

module Amelie.Controller.Browse
  (handle)
  where

import Amelie.Controller     (output,getPagination)
import Amelie.Model
import Amelie.Model.Channel  (getChannels)
import Amelie.Model.Language (getLanguages)
import Amelie.Model.Paste    (getSomePastes,countPublicPastes)
import Amelie.View.Browse    (page)

handle :: Controller ()
handle = do
  pn <- getPagination
  total <- model countPublicPastes
  pastes <- model $ getSomePastes pn
  let pn' = pn { pnRoot = "/browse"
               , pnResults = fromIntegral (length pastes)
               , pnTotal = total }
  chans <- model getChannels
  langs <- model getLanguages
  output $ page pn' chans langs pastes
