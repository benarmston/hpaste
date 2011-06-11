{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Activity page controller.

module Amelie.Controller.Activity
  (handle)
  where

import Amelie.Controller       (outputText)
import Amelie.Controller.Cache (cache)
import Amelie.Model
import Amelie.Model.Activity   (getCommits)
import Amelie.Types.Cache      as Key
import Amelie.View.Activity    (page)

import Control.Monad.Env       (env)

handle :: Controller ()
handle = do
  html <- cache Key.Activity $ do
    uri <- env $ configCommits . controllerStateConfig
    commits <- model $ getCommits uri
    return $ Just $ page commits
  maybe (return ()) outputText html
