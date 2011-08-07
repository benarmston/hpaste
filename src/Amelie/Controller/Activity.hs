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

-- @ label activityPage
-- @ do Show Amelie project commit activity.
-- @ trigger getCommits
handle :: Controller ()
handle = do
  html <- cache Key.Activity $ do
    uri <- env $ configCommits . controllerStateConfig
    repourl <- env $ configRepoURL . controllerStateConfig
    commits <- model $ getCommits uri
    return $ Just $ page repourl commits
  maybe (return ()) outputText html
