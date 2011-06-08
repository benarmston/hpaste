{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Channel model.

module Amelie.Model.Channel
  (getChannels)
  where

import Amelie.Types
import Amelie.Model

-- | Get the channels.
getChannels :: Model [Channel]
getChannels =
  queryNoParams ["SELECT *"
                ,"FROM channel"]
