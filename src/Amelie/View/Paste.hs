{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Paste views.

module Amelie.View.Paste
  (pasteFormlet
  ,page
  ,pasteLink
  ,pasteRawLink)
  where

import           Amelie.Types
import           Amelie.View.Html
import           Amelie.View.Layout

import           Control.Applicative              ((<$>),(<*>),pure)
import           Control.Monad                    (when)
import           Data.ByteString.UTF8
import qualified Data.Map                         as M
import           Data.Monoid.Operator             ((++))
import           Data.Text                        (Text)
import           Data.Text.Encoding               (encodeUtf8)
import           Data.Text.Lazy                   (fromStrict)
import           Data.Time.Show                   (showDateTime)
import           Prelude                          hiding ((++))
import           Safe                             (readMay)
import           Snap.Types                       (Params)
import           Text.Blaze.Html5                 as H hiding (map)
import           Text.Blaze.Html5.Extra
import           Text.Formlet
import           Text.Highlighter.Formatters.Html (format)
import           Text.Highlighter.Lexer           (runLexer)
import           Text.Highlighter.Lexers.Haskell  (lexer)

-- | A formlet for paste submission / editing.
pasteFormlet :: Params -> Bool -> [Text] -> (Formlet PasteSubmit,Html)
pasteFormlet params submitted errors =
  let form = postForm $ do
        formletHtml formlet params
        submitInput "submit" "Submit"
        when submitted $
          when (not (null errors)) $
            mapM_ (p . toHtml) errors
  in (formlet,form)
  
    where formlet =
            PasteSubmit <$> pure pasteId
                        <*> req (textInput "title" "Title")
                        <*> req (textInput "author" "Author")
                        <*> opt (dropInput "language" "Language")
                        <*> opt (dropInput "channel" "Channel")
                        <*> req (areaInput "paste" "Paste")
          pasteId = M.lookup "paste_id" params >>=
                    readMay . concat . map toString >>=
                    return . (fromIntegral :: Integer -> PasteId)

-- | Render the page page.
page :: Paste -> Html
page p@Paste{..} =
  layoutPage $ Page {
    pageTitle = pasteTitle
  , pageBody = viewPaste p
  }

-- | View a paste's details and content.
viewPaste :: Paste -> Html
viewPaste paste@Paste{..} = do
  pasteDetails paste
  pasteContent paste
  
-- | List the details of the page in a dark section.
pasteDetails :: Paste -> Html
pasteDetails paste@Paste{..} =
  darkSection (fromStrict pasteTitle) $ do
      ul ! aClass "paste-specs" $ do
        detail "Paste" $ pasteLink paste $ "#" ++ show pasteId
        detail "Author" $ pasteAuthor
        detail "Channel" $ maybe "-" show pasteChannel
        detail "Created" $ showDateTime pasteDate
        detail "Raw" $ pasteRawLink paste $ ("View raw link" :: Text)
      clear

    where detail title content = do
            li $ do strong (title ++ ":"); toHtml content

pasteContent :: Paste -> Html
pasteContent Paste{..} =
  lightNoTitleSection $ do
    case runLexer lexer (encodeUtf8 (pastePaste ++ "\n")) of
      Right tokens -> format True tokens
      _            -> pre $ toHtml pastePaste
      

-- | The href link to a paste.
pasteLink :: ToHtml html => Paste -> html -> Html
pasteLink Paste{..} inner = href ("/" ++ show pasteId) inner

-- | The href link to a paste, raw content.
pasteRawLink :: ToHtml html => Paste -> html -> Html
pasteRawLink Paste{..} inner = href ("/raw/" ++ show pasteId) inner
