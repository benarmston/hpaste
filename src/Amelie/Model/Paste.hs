{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

-- | Paste model.

module Amelie.Model.Paste
  (getLatestPastes
  ,getPasteById
  ,createOrEdit
  ,createPaste
  ,getAnnotations
  ,getSomePastes
  ,countPublicPastes
  ,generateHints
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

-- | Get some paginated pastes.
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
        ,"WHERE annotation_of = ?"
        ,"ORDER BY id ASC"]
        (Only pid)

-- | Create a paste, or update an existing one.
createOrEdit :: [Language] -> [Channel] -> PasteSubmit -> Model (Maybe PasteId)
createOrEdit langs chans paste@PasteSubmit{..} = do
  case pasteSubmitId of
    Nothing  -> createPaste langs chans paste
    Just pid -> do updatePaste pid paste
                   return $ Just pid

-- | Create a new paste (possibly editing an existing one).
createPaste :: [Language] -> [Channel] -> PasteSubmit -> Model (Maybe PasteId)
createPaste langs chans ps@PasteSubmit{..} = do
  res <- single ["INSERT INTO paste"
                ,"(title,author,content,channel,language,annotation_of)"
                ,"VALUES"
                ,"(?,?,?,?,?,?)"
                ,"returning id"]
                (pasteSubmitTitle,pasteSubmitAuthor,pasteSubmitPaste
                ,pasteSubmitChannel,pasteSubmitLanguage,pasteSubmitId)
  when (lang == Just "haskell") $ just res $ createHints ps
  just (pasteSubmitChannel >>= lookupChan) $ \chan ->
    just res $ \pid -> do
      annotated <- maybe (return Nothing) getPasteById pasteSubmitId
      announcePaste annotated (channelName chan) ps pid
  return (pasteSubmitId <|> res)

  where lookupChan cid = find ((==cid).channelId) chans
        lookupLang lid = find ((==lid).languageId) langs
        lang = pasteSubmitLanguage >>= (fmap languageName . lookupLang)
        just j m = maybe (return ()) m j

-- | Create the hints for a paste.
createHints :: PasteSubmit -> PasteId -> Model ()
createHints ps pid = do
  hints <- generateHintsForPaste ps pid
  forM_ hints $ \hint ->
    exec ["INSERT INTO hint"
         ,"(paste,type,content)"
         ,"VALUES"
         ,"(?,?,?)"]
         (pid
         ,suggestionSeverity hint
         ,show hint)

-- | Announce the paste.
announcePaste :: Maybe Paste -> Text -> PasteSubmit -> PasteId -> Model ()
announcePaste annotated channel PasteSubmit{..} pid = do
  conf <- env modelStateConfig  
  announce (fromStrict channel) $ fromStrict $
    nick ++ " " ++ verb ++ " “" ++ pasteSubmitTitle ++ "” at " ++ link conf
  where nick | validNick (unpack pasteSubmitAuthor) = pasteSubmitAuthor
             | otherwise = "“" ++ pasteSubmitAuthor ++ "”"
        link Config{..} = "http://" ++ pack configDomain ++ "/" ++ pid'
        pid' = case annotated of
                 Just Paste{..} -> showPid pasteId ++ "#a" ++ showPid pid
                 Nothing -> showPid pid
        verb = case annotated of
                 Just Paste{..} -> "annotated “" ++ pasteTitle ++ "” with"
                 Nothing -> "pasted"
        showPid p = pack $ show $ (fromIntegral p :: Integer)

-- | Is a nickname valid? Digit/letter or one of these: -_/\\;()[]{}?`'
validNick :: String -> Bool
validNick s = first && all ok s && length s > 0 where
  ok c = isDigit c || isLetter c || elem c "-_/\\;()[]{}?`'"
  first = all (\c -> isDigit c || isLetter c) $ take 1 s

-- | Get hints for a Haskell paste from hlint.
generateHintsForPaste :: PasteSubmit -> PasteId -> Model [Suggestion]
generateHintsForPaste PasteSubmit{..} (fromIntegral -> pid :: Integer) =
  generateHints (show pid) pasteSubmitPaste

-- | Get hints for a Haskell paste from hlint.
generateHints :: FilePath -> Text -> Model [Suggestion]
generateHints pid contents = io $ do
  tmpdir <- getTemporaryDirectory
  let tmp = tmpdir </> pid ++ ".hs"
  exists <- doesFileExist tmp
  unless exists $ T.writeFile tmp $ contents
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
