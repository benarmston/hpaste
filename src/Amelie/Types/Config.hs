-- | Site-wide configuration.

module Amelie.Types.Config
       (Config(..)
       ,Announcer(..))
       where

import Database.PostgreSQL.Simple (ConnectInfo)

-- | Site-wide configuration.
data Config = Config {
    configAnnounce :: Announcer
  , configPostgres :: ConnectInfo
  , configDomain   :: String
  , configCommits  :: String
  , configRepoURL  :: String
  } deriving (Show)

-- | Announcer configuration.
data Announcer = Announcer {
    announceUser :: String
  , announcePass :: String
  , announceHost :: String
  , announcePort :: Int
} deriving (Show)
