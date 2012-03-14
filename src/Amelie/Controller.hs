{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Controller routing/handling.

module Amelie.Controller
  (runHandler
  ,output
  ,outputText
  ,goHome
  ,justOrGoHome
  ,getInteger
  ,getString
  ,getPagination)
  where

import Amelie.Types
import Amelie.Types.Cache

import Control.Applicative
import Control.Concurrent.Chan    (Chan)
import Control.Monad.Reader       (runReaderT)
import Data.ByteString            (ByteString)
import Data.ByteString.UTF8       (toString)
import Data.Text.Lazy             (Text,toStrict)
import Database.PostgreSQL.Base   (withPoolConnection)
import Database.PostgreSQL.Simple (Pool)
import Safe                       (readMay)
import Snap.Core                  (Snap,writeText,redirect,getParam)
import Snap.Core                  (modifyResponse,setContentType)
import Text.Blaze                 (Html)
import Text.Blaze.Renderer.Text   (renderHtml)

-- | Run a controller handler.
runHandler :: Config -> Pool -> Cache -> Chan Text -> Controller () -> Snap ()
runHandler conf pool cache anns ctrl = do
  withPoolConnection pool $ \conn -> do
    let state = ControllerState conf conn cache anns
    -- Default to HTML, can be overridden.
    modifyResponse $ setContentType "text/html"
    runReaderT (runController ctrl) state 

-- | Strictly renders HTML to Text before outputting it via Snap.
--   This ensures that any lazy exceptions are caught by the Snap
--   handler.
output :: Html -> Controller ()
output html = outputText $ renderHtml $ html

-- | Strictly renders text before outputting it via Snap.
--   This ensures that any lazy exceptions are caught by the Snap
--   handler.
outputText :: Text -> Controller ()
outputText text = do
  let !x = toStrict $ text
  writeText x

-- | Generic redirect to home page.
goHome :: Controller ()
goHome = redirect "/"

-- | Extract a Just value or go home.
justOrGoHome :: Maybe a -> (a -> Controller ()) -> Controller ()
justOrGoHome x m = maybe goHome m x

-- | Get integer parmater.
getInteger :: ByteString -> Integer -> Controller Integer
getInteger name def = do
  pid <- (>>= readMay . toString) <$> getParam name
  maybe (return def) return pid

-- | Get string.
getString :: ByteString -> String -> Controller String
getString name def = do
  pid <- (>>= return . toString) <$> getParam name
  maybe (return def) return pid

-- | Get pagination data.
getPagination :: Controller Pagination
getPagination = do
  p <- getInteger "page" 1
  limit <- getInteger "limit" 35
  return Pagination { pnPage = max 1 p
                    , pnLimit = max 1 (min 100 limit)
                    , pnRoot = "/"
                    , pnResults = 0
                    , pnTotal = 0
                    }
