{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | IRC announcer.

module Amelie.Model.Announcer
       (newAnnouncer
       ,announce)
       where

import           Amelie.Types

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Env    (env)
import           Control.Monad.IO     (io)
import qualified Data.ByteString.Lazy      as B
import           Data.Monoid.Operator ((++))
import           Data.Text.Lazy       (Text,pack)
import           Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy.IO    as T
import           Network
import           Prelude              hiding ((++))
import           System.IO

-- | Start a thread and return a channel to it.
newAnnouncer :: Announcer -> IO (Chan Text)
newAnnouncer config = do
  putStrLn "Connecting..."
  ans <- newChan
  _ <- forkIO $ announcer config ans (\_ -> return ())
  return ans

-- | Run the announcer bot.
announcer :: Announcer -> Chan Text -> (Handle -> IO ()) -> IO ()
announcer c@Announcer{..} ans cont = do
  h <- connectTo announceHost (PortNumber $ fromIntegral announcePort)
  hSetBuffering h NoBuffering
  let send h line = catch (do B.hPutStr h (encodeUtf8 (line ++ "\n"))
                              T.putStrLn line)
                          (\_ -> do announcer c ans $ \h ->
                                      send h line)
  send h $ "PASS " ++ pack announcePass
  send h $ "USER " ++ pack announceUser ++ " * * *"
  send h $ "NICK " ++ pack announceUser
  cont h
  lines <- getChanContents ans
  forM_ lines $ \line -> send h line

-- | Announce something to the IRC.
announce :: Text -> Text -> Model ()
announce channel line = do
  chan <- env modelStateAnns
  io $ writeChan chan $ "PRIVMSG " ++ channel ++ " :" ++ line
