{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing #-}

-- | Load the configuration file.

module Amelie.Config
       (getConfig)
       where

import Amelie.Types.Config

import Data.ConfigFile
import Database.PostgreSQL.Simple (ConnectInfo(..))

getConfig :: FilePath -> IO Config
getConfig conf = do
  contents <- readFile conf
  let config = do
        c <- readstring emptyCP contents
        [user,pass,host,port]
          <- mapM (get c "ANNOUNCE")
                  ["user","pass","host","port"]
        [pghost,pgport,pguser,pgpass,pgdb]
          <- mapM (get c "POSTGRESQL")
                  ["host","port","user","pass","db"]
        [domain]
          <- mapM (get c "WEB")
                  ["domain"]
        [commits]
          <- mapM (get c "DEV")
                  ["commits"]
                  
        return Config {
           configAnnounce = Announcer user pass host (read port)
         , configPostgres = ConnectInfo pghost (read pgport) pguser pgpass pgdb
         , configDomain = domain
         , configCommits = commits
         }
  case config of
    Left cperr -> error $ show cperr
    Right config -> return config
