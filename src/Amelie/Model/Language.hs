{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Language model.

module Amelie.Model.Language
  (getLanguages)
  where

import Amelie.Types
import Amelie.Model

-- | Get the languages.
getLanguages :: Model [Language]
getLanguages =
  queryNoParams ["SELECT *"
                ,"FROM language"]
