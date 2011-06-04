{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Paste model.

module Amelie.Model.Paste
  (getLatestPastes)
  where

import Amelie.Types
import Amelie.Model

getLatestPastes :: Model [Paste]
getLatestPastes = queryNoParams ["SELECT created,title,author,language,channel,content"
                                ,"FROM toplevel_paste"
                                ,"ORDER BY id DESC"
                                ,"LIMIT 10"]
