{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Home page model.

module Amelie.Model.Home
  (getPastes)
  where

import Amelie.Model
import Amelie.Model.Paste

getPastes :: Model [Paste]
getPastes = getLatestPastes
