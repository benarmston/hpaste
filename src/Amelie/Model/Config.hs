{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Database configuration.

module Amelie.Model.Config where

import Database.PostgreSQL.Simple

-- | Postgres connection information.
auth :: ConnectInfo
auth = ConnectInfo { connectHost = "127.0.0.1"
                   , connectPort = 5432
                   , connectUser = "amelie"
                   , connectPassword = "amelie"
                   , connectDatabase = "amelie"
                   }
