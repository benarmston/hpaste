{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

-- | HTML caching.

module Amelie.Controller.Cache
       (newCache
       ,cache
       ,resetCache)
       where
 
import           Amelie.Types
import           Amelie.Types.Cache

import           Control.Applicative      ((<$>))
import           Control.Concurrent
import           Control.Monad.IO         (io)
import           Control.Monad.Reader     (asks)
import qualified Data.Map                 as M
import           Data.Text.Lazy           (Text)
import           Text.Blaze.Html5         (Html)
import           Text.Blaze.Renderer.Text (renderHtml)

-- | Create a new cache.
newCache :: IO Cache
newCache = do
  var <- newMVar M.empty
  return $ Cache var

cache :: Key -> Controller (Maybe Html) -> Controller (Maybe Text)
cache _key generate = fmap (fmap renderHtml) generate

-- | Generate and save into the cache, or retrieve existing from the
-- | cache.
cache' :: Key -> Controller (Maybe Html) -> Controller (Maybe Text)
cache' key generate = do
  Cache var <- asks controllerStateCache
  mapping <- io $ readMVar var
  case M.lookup key mapping of
    Just html -> return $ Just html
    Nothing   -> do
      html <- fmap renderHtml <$> generate
      case html of
        Just html -> io $ modifyMVar_ var (return . M.insert key html)
        Nothing   -> return ()
      return $ html

-- | Reset an item in the cache.
resetCache :: Key -> Controller ()
resetCache key = do
  Cache var <- asks controllerStateCache
  io $ modifyMVar_ var (return . M.delete key)
