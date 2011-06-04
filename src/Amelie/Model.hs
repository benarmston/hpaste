{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Model running.

module Amelie.Model
  (model
  ,query
  ,queryNoParams
  ,module Amelie.Types)
  where

import           Amelie.Types

import           Control.Monad.Env
import           Control.Monad.IO
import           Control.Monad.Reader                
import           Data.String
import qualified Database.PostgreSQL.Simple              as DB
import           Database.PostgreSQL.Simple.QueryParams
import           Database.PostgreSQL.Simple.QueryResults

model :: Model a -> Controller a
model action = do
  conn <- env controllerStateConn
  let state = ModelState conn
  io $ runReaderT (runModel action) state

query :: (QueryParams ps,QueryResults r) => [String] -> ps -> Model [r]
query q ps = do
  conn <- env modelStateConn
  Model $ ReaderT (\_ -> DB.query conn (fromString (unlines q)) ps)

queryNoParams :: (QueryResults r) => [String] -> Model [r]
queryNoParams q = do
  conn <- env modelStateConn
  Model $ ReaderT (\_ -> DB.query_ conn (fromString (unlines q)))
