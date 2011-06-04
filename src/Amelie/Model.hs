{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Model running.

module Amelie.Model
  (model
  ,query
  ,single
  ,queryNoParams
  ,exec
  ,module Amelie.Types
  ,DB.Only(..))
  where

import           Amelie.Types

import           Control.Monad.Env
import           Control.Monad.IO
import           Control.Monad.Reader
import           Data.String
import qualified Database.PostgreSQL.Simple              as DB
import           Database.PostgreSQL.Simple              (Only(..))
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

single :: (QueryParams ps,QueryResults (Only r)) => [String] -> ps -> Model (Maybe r)
single q ps = do
  rows <- query q ps
  case rows of
    [(Only r)] -> return (Just r)
    _          -> return Nothing

queryNoParams :: (QueryResults r) => [String] -> Model [r]
queryNoParams q = do
  conn <- env modelStateConn
  Model $ ReaderT (\_ -> DB.query_ conn (fromString (unlines q)))

exec :: (QueryParams ps) => [String] -> ps -> Model Integer
exec q ps = do
  conn <- env modelStateConn
  Model $ ReaderT (\_ -> DB.execute conn (fromString (unlines q)) ps)
