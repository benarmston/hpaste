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
  ,createPaste
  ,getAnnotations
  ,getSomePastes
  ,countPublicPastes
  ,getHints)
  where

import Amelie.Types
import Amelie.Model
import Amelie.Model.Announcer

import Control.Applicative    ((<$>),(<|>))
import Control.Monad
import Control.Monad.Env
import Control.Monad.IO
import Data.Char
import Data.List              (find,intercalate)
import Data.Maybe             (fromMaybe,listToMaybe)
import Data.Monoid.Operator   ((++))
import Data.Text              (Text,unpack,pack)
import Data.Text.IO           as T (writeFile)
import Data.Text.Lazy         (fromStrict)
import Language.Haskell.HLint
import Prelude                hiding ((++))
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
createOrEdit :: [Channel] -> PasteSubmit -> Model (Maybe PasteId)
createOrEdit chans paste@PasteSubmit{..} = do
  case pasteSubmitId of
    Nothing  -> createPaste chans paste
    Just pid -> do updatePaste pid paste
                   return $ Just pid

-- | Create a new paste (possibly editing an existing one).
createPaste :: [Channel] -> PasteSubmit -> Model (Maybe PasteId)
createPaste chans ps@PasteSubmit{..} = do
  pid <- single ["INSERT INTO paste"
                ,"(title,author,content,channel,language,annotation_of)"
                ,"VALUES"
                ,"(?,?,?,?,?,?)"
                ,"returning id"]
                (pasteSubmitTitle,pasteSubmitAuthor,pasteSubmitPaste
                ,pasteSubmitChannel,pasteSubmitLanguage,pasteSubmitId)
  let withPid f = maybe (return ()) (f ps) pid
      parentPid = pasteSubmitId <|> pid
  withPid createHints
  case pasteSubmitChannel >>= lookupChan of
    Nothing   -> return ()
    Just chan -> maybe (return ()) (announcePaste (channelName chan) ps) parentPid
  return parentPid

  where lookupChan cid = find ((==cid).channelId) chans

-- | Create the hints for a paste.
createHints :: PasteSubmit -> PasteId -> Model ()
createHints ps pid = do
  hints <- generateHints ps pid
  forM_ hints $ \hint ->
    exec ["INSERT INTO hint"
         ,"(paste,type,content)"
         ,"VALUES"
         ,"(?,?,?)"]
         (pid
         ,suggestionSeverity hint
         ,show hint)

-- | Announce the paste.
announcePaste :: Text -> PasteSubmit -> PasteId -> Model ()
announcePaste channel PasteSubmit{..} pid = do
  conf <- env modelStateConfig  
  announce (fromStrict channel) $ fromStrict $
    nick ++ " pasted “" ++ pasteSubmitTitle ++ "” at " ++ link conf
  where nick | validNick (unpack pasteSubmitAuthor) = pasteSubmitAuthor
             | otherwise = "“" ++ pasteSubmitAuthor ++ "”"
        link Config{..} = "http://" ++ pack configDomain ++ "/" ++ pack (show pid')
        pid' = fromIntegral pid :: Integer

-- | Is a nickname valid? Digit/letter or one of these: -_/\\;()[]{}?`'
validNick :: String -> Bool
validNick s = first && all ok s && length s > 0 where
  ok c = isDigit c || isLetter c || elem c "-_/\\;()[]{}?`'"
  first = all (\c -> isDigit c || isLetter c) $ take 1 s

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
             [intercalate ", " (map set (words fields))]
             ++
             ["WHERE id = ?"])
            (pasteSubmitTitle
            ,pasteSubmitAuthor
            ,pasteSubmitPaste
            ,pasteSubmitLanguage
            ,pasteSubmitChannel
            ,pid)
  return ()
  
    where fields = "title author content channel language"
          set key = unwords [key,"=","?"]
