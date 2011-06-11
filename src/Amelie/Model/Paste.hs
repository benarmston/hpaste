{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | Paste model.

module Amelie.Model.Paste
  (getLatestPastes
  ,getPasteById
  ,createOrEdit
  ,getAnnotations
  ,getSomePastes
  ,countPublicPastes
  ,getHints)
  where

import Amelie.Types
import Amelie.Model

import Control.Applicative    ((<$>))
import Control.Monad
import Control.Monad.IO
import Data.Maybe             (fromMaybe,listToMaybe)
import Data.Text.IO           as T (writeFile)
import Language.Haskell.HLint
import System.Directory
import System.FilePath

-- | Count public pastes.
countPublicPastes :: Model Integer
countPublicPastes = do
  rows <- singleNoParams ["SELECT COUNT(*)"
                         ,"FROM public_toplevel_paste"]
  return $ fromMaybe 0 rows

-- | Get the latest pastes.
getLatestPastes :: Model [Paste]
getLatestPastes =
  queryNoParams ["SELECT *"
                ,"FROM public_toplevel_paste"
                ,"ORDER BY id DESC"
                ,"LIMIT 20"]

-- | Get the latest pastes.
getSomePastes :: Pagination -> Model [Paste]
getSomePastes Pagination{..} =
  queryNoParams ["SELECT *"
                ,"FROM public_toplevel_paste"
                ,"ORDER BY id DESC"
                ,"OFFSET " ++ show (max 0 (pnPage - 1) * pnLimit)
                ,"LIMIT " ++ show pnLimit]

-- | Get a paste by its id.
getPasteById :: PasteId -> Model (Maybe Paste)
getPasteById pid =
  listToMaybe <$> query ["SELECT *"
                        ,"FROM public_paste"
                        ,"WHERE id = ?"]
                        (Only pid)

-- | Get annotations of a paste.
getAnnotations :: PasteId -> Model [Paste]
getAnnotations pid =
  query ["SELECT *"
        ,"FROM public_paste"
        ,"WHERE annotation_of = ?"]
        (Only pid)

-- | Create a paste, or update an existing one.
createOrEdit :: PasteSubmit -> Model (Maybe PasteId)
createOrEdit paste@PasteSubmit{..} = do
  case pasteSubmitId of
    Nothing  -> createPaste paste
    Just pid -> do updatePaste pid paste
                   return $ Just pid

-- | Create a new paste.
createPaste :: PasteSubmit -> Model (Maybe PasteId)
createPaste ps@PasteSubmit{..} = do
  pid <- single ["INSERT INTO paste"
                ,"(title,author,content,channel,language)"
                ,"VALUES"
                ,"(?,?,?,?,?)"
                ,"returning id"]
                (pasteSubmitTitle,pasteSubmitAuthor,pasteSubmitPaste
                ,pasteSubmitChannel,pasteSubmitLanguage)
  case pid of
    Nothing  -> return ()
    Just p -> do
      hints <- generateHints ps p
      forM_ hints $ \hint ->
        exec ["INSERT INTO hint"
             ,"(paste,type,content)"
             ,"VALUES"
             ,"(?,?,?)"]
             (pid
             ,suggestionSeverity hint
             ,show hint)
  return pid

-- | Get hints for a Haskell paste from hlint.
generateHints :: PasteSubmit -> PasteId -> Model [Suggestion]
generateHints  PasteSubmit{..} (fromIntegral -> pid :: Integer) = io $ do
  tmpdir <- getTemporaryDirectory
  let tmp = tmpdir </> show pid ++ ".hs"
  exists <- doesFileExist tmp
  unless exists $ T.writeFile tmp $ pasteSubmitPaste
  hints <- hlint [tmp,"--quiet","--ignore=Parse error"]
  return hints

getHints :: PasteId -> Model [Hint]
getHints pid =
  query ["SELECT type,content"
        ,"FROM hint"
        ,"WHERE paste = ?"]
        (Only pid)

-- | Update an existing paste.
updatePaste :: PasteId -> PasteSubmit -> Model ()
updatePaste pid PasteSubmit{..} = do
  _ <- exec (["UPDATE paste"
             ,"SET"]
             ++
             map set (words "title author content language channel")
             ++
             ["WHERE id = ?"])
            (pasteSubmitTitle
            ,pasteSubmitAuthor
            ,pasteSubmitPaste
            ,pasteSubmitLanguage
            ,pasteSubmitChannel
            ,pid)
  return ()
  
    where set key = unwords [key,"=","?"]
