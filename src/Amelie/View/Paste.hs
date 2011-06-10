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
import           Amelie.View.Highlight            (highlightPaste)
import           Amelie.View.Html
import           Amelie.View.Layout

import           Control.Applicative              ((<$>),(<*>),pure)
import           Control.Monad                    (when)
import           Data.ByteString.UTF8             (toString)
import qualified Data.Map                         as M
import           Data.Monoid.Operator             ((++))
import           Data.String                      (fromString)
import           Data.Text                        (Text)
import           Data.Text.Lazy                   (fromStrict)
import           Data.Time.Show                   (showDateTime)
import           Data.Traversable
import           Prelude                          hiding ((++))
import           Safe                             (readMay)
import           Text.Blaze.Html5                 as H hiding (map)
import qualified Text.Blaze.Html5.Attributes      as A
import           Text.Blaze.Html5.Extra
import           Text.Formlet

-- | A formlet for paste submission / editing.
pasteFormlet :: PasteFormlet -> (Formlet PasteSubmit,Html)
pasteFormlet PasteFormlet{..} =
  let form = postForm ! A.action "/new" $ do
        when pfSubmitted $
          when (not (null pfErrors)) $
            H.div ! aClass "errors" $
              mapM_ (p . toHtml) pfErrors
        formletHtml formlet pfParams
        submitInput "submit" "Submit"
  in (formlet,form)
  
    where formlet =
            PasteSubmit
              <$> pure pasteId
              <*> req (textInput "title" "Title")
              <*> req (textInput "author" "Author")
              <*> parse (traverse (fmap LanguageId . integer))
                        (opt (dropInput languages "language" "Language"))
              <*> parse (traverse (fmap ChannelId . integer))
                        (opt (dropInput channels "channel" "Channel"))
              <*> req (areaInput "paste" "Paste")

          pasteId = M.lookup "paste_id" pfParams >>=
                    readMay . concat . map toString >>=
                    return . (fromIntegral :: Integer -> PasteId)
          channels = ("","") :
                     map (\Channel{..} ->
                           (fromString $ show channelId,channelName))
                         pfChannels
          languages = ("","") :
                     map (\Language{..} ->
                           (fromString $ show languageId,languageTitle))
                         pfLanguages

-- | Render the page page.
page :: [Channel] -> [Language] -> Paste -> [Paste] -> Html
page chans langs p@Paste{..} as =
  layoutPage $ Page {
    pageTitle = pasteTitle
  , pageBody = do viewPaste chans langs p
                  viewAnnotations chans langs as
  , pageName = "paste"
  }

-- | View the paste's annotations.
viewAnnotations :: [Channel] -> [Language] -> [Paste] -> Html
viewAnnotations chans langs pastes = do
  mapM_ (viewPaste chans langs) pastes

-- | View a paste's details and content.
viewPaste :: [Channel] -> [Language] -> Paste -> Html
viewPaste chans langs paste@Paste{..} = do
  pasteDetails chans langs paste
  pasteContent langs paste

-- | List the details of the page in a dark section.
pasteDetails :: [Channel] -> [Language] -> Paste -> Html
pasteDetails chans langs paste@Paste{..} =
  darkSection (fromStrict pasteTitle) $ do
      ul ! aClass "paste-specs" $ do
        detail "Paste" $ pasteLink paste $ "#" ++ show pasteId
        detail "Author" $ pasteAuthor
        detail "Language" $ showLanguage langs pasteLanguage
        detail "Channel" $ showChannel chans pasteChannel
        detail "Created" $ showDateTime pasteDate
        detail "Raw" $ pasteRawLink paste $ ("View raw link" :: Text)
      clear

    where detail title content = do
            li $ do strong (title ++ ":"); toHtml content

-- | Show the paste content with highlighting.
pasteContent :: [Language] -> Paste -> Html
pasteContent langs paste =
  lightNoTitleSection $ highlightPaste langs paste

-- | The href link to a paste.
pasteLink :: ToHtml html => Paste -> html -> Html
pasteLink Paste{..} inner = href ("/" ++ show pasteId) inner

-- | The href link to a paste, raw content.
pasteRawLink :: ToHtml html => Paste -> html -> Html
pasteRawLink Paste{..} inner = href ("/raw/" ++ show pasteId) inner
