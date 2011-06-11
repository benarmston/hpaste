{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Activity model.

module Amelie.Model.Activity
  (getCommits)
  where

import Amelie.Types
import Amelie.Model

import Control.Monad.IO      (io)
import Data.Maybe            (mapMaybe)
import Data.Text.Lazy        (pack)
import Data.Time
import Network.Curl.Download
import System.Locale
import Text.Feed.Query

-- | Get commits of this project from a commit feed.
getCommits :: String -> Model [Commit]
getCommits uri = io $ do
  result <- openAsFeed uri
  case result of
    Left _ -> return []
    Right feed -> return $
      let items = getFeedItems feed
      in mapMaybe makeCommit items
        
  where makeCommit item = do
          title <- getItemTitle item
          datestr <- getItemDate item
          date <- parseDateString datestr
          link <- getItemLink item
          return $ Commit {
            commitTitle = pack $ title
          , commitContent = "" -- Getting content from atom does not work.
          , commitDate = date
          , commitLink = pack link
          }
        -- E.g. 2011-06-11T11:15:11-07:00
        parseDateString = parseTime defaultTimeLocale "%Y-%M-%dT%T%Z"
