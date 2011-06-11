{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Paste model.

module Amelie.Model.Paste
  (getLatestPastes
  ,getPasteById
  ,createOrEdit
  ,getAnnotations
  ,getSomePastes
  ,countPublicPastes)
  where

import Amelie.Types
import Amelie.Model

import Control.Applicative ((<$>))
import Data.Maybe          (fromMaybe,listToMaybe)

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
createPaste PasteSubmit{..} =
  single ["INSERT INTO paste"
         ,"(title,author,content,channel,language)"
         ,"VALUES"
         ,"(?,?,?,?,?)"
         ,"returning id"]
         (pasteSubmitTitle,pasteSubmitAuthor,pasteSubmitPaste
         ,pasteSubmitChannel,pasteSubmitLanguage)

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
