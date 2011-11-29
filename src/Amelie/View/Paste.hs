{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Paste views.

module Amelie.View.Paste
  (pasteFormlet
  ,page
  ,pasteLink
  ,pasteRawLink)
  where

import           Amelie.Model.Irclogs        (showIrcDateTime)
import           Amelie.Types
import           Amelie.View.Highlight       (highlightPaste)
import           Amelie.View.Hlint           (viewHints)
import           Amelie.View.Html
import           Amelie.View.Layout

import           Control.Applicative         ((<$>),(<*>),pure)
import           Control.Arrow               ((&&&))
import           Control.Monad               (when,join)
import           Data.ByteString.UTF8        (toString)
import           Data.List                   (find)
import qualified Data.Map                    as M
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid.Operator        ((++))
import           Data.Text                   (Text,pack)
import qualified Data.Text                   as T
import           Data.Text.Lazy              (fromStrict)
import           Data.Time.Show              (showDateTime)
import           Data.Traversable
import           Prelude                     hiding ((++))
import           Safe                        (readMay)
import           Text.Blaze.Html5            as H hiding (map)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html5.Extra
import           Text.Formlet

-- | Render the page page.
page :: PastePage -> Html
page PastePage {ppPaste=p@Paste{..},..} =
  layoutPage $ Page {
    pageTitle = pasteTitle
  , pageBody = do viewPaste [] ppChans ppLangs (p,ppHints)
                  viewAnnotations (p : ppAnnotations)
                                  ppChans
                                  ppLangs
                                  (zip ppAnnotations ppAnnotationHints)
  , pageName = "paste"
  }
  
-- | A formlet for paste submission / editing.
pasteFormlet :: PasteFormlet -> (Formlet PasteSubmit,Html)
pasteFormlet pf@PasteFormlet{..} =
  let form = postForm ! A.action (toValue action) $ do
        when pfSubmitted $
          when (not (null pfErrors)) $
            H.div ! aClass "errors" $
              mapM_ (p . toHtml) pfErrors
        formletHtml (pasteSubmit pf) pfParams
        submitInput "submit" "Submit"
  in (pasteSubmit pf,form)
    
  where action = case pfEditPaste of
                   Just Paste{..} -> "/edit/" ++ show (fromMaybe pasteId pasteParent)
                   Nothing        -> "/new"

-- | The paste submitting formlet itself.
pasteSubmit :: PasteFormlet -> Formlet PasteSubmit
pasteSubmit pf@PasteFormlet{..} =
  PasteSubmit
    <$> pure (getPasteId pf)
    <*> req (textInput "title" "Title" editTitle)
    <*> req (textInput "author" "Author" Nothing)
    <*> parse (traverse lookupLang)
              (opt (dropInput languages "language" "Language" (snd defChan)))
    <*> parse (traverse lookupChan)
              (opt (dropInput channels "channel" "Channel" (fst defChan)))
    <*> req (areaInput "paste" "Paste" editContent)
    <*> opt (wrap (H.div ! aClass "spam") (textInput "email" "Email" Nothing))

    where channels = options channelName channelName pfChannels
          languages = options languageName languageTitle pfLanguages
          
          lookupLang slug = findOption ((==slug).languageName) pfLanguages languageId
          lookupChan slug = findOption ((==slug).channelName) pfChannels channelId
          
          defChan = maybe (fromMaybe "" editChan,fromMaybe "haskell" editLanguage)
                          (channelName &&& trim.channelName)
                          (pfDefChan >>= findChan)
          findChan name = find ((==name).trim.channelName) pfChannels
          trim = T.dropWhile (=='#')
          
          editContent = pastePaste <$> pfEditPaste
          editTitle = ((++ " (annotation)") . pasteTitle) <$> pfEditPaste
          editLanguage = join (fmap pasteLanguage pfEditPaste) >>= findLangById
          editChan = join (fmap pasteChannel pfEditPaste) >>= findChanById

          findChanById id = channelName <$> find ((==id).channelId) pfChannels
          findLangById id = languageName <$> find ((==id).languageId) pfLanguages

-- | Get the paste id.
getPasteId :: PasteFormlet -> Maybe PasteId
getPasteId PasteFormlet{..} =
  M.lookup "id" pfParams >>=
  readMay . concat . map toString >>=
  return . (fromIntegral :: Integer -> PasteId)

-- | View the paste's annotations.
viewAnnotations :: [Paste] -> [Channel] -> [Language] -> [(Paste,[Hint])] -> Html
viewAnnotations pastes chans langs annotations = do
  mapM_ (viewPaste pastes chans langs) annotations

-- | View a paste's details and content.
viewPaste :: [Paste] -> [Channel] -> [Language] -> (Paste,[Hint]) -> Html
viewPaste pastes chans langs (paste@Paste{..},hints) = do
  case pasteParent of
    Nothing -> return ()
    Just{}  -> let an = "a" ++ show (fromIntegral pasteId :: Integer)
               in a ! A.name (toValue an) $ return ()
  pasteDetails pastes chans langs paste
  pasteContent langs paste
  viewHints hints

-- | List the details of the page in a dark section.
pasteDetails :: [Paste] -> [Channel] -> [Language] -> Paste -> Html
pasteDetails pastes chans langs paste@Paste{..} =
  darkNoTitleSection $ do
    pasteNav langs pastes paste
    h2 $ toHtml $ fromStrict pasteTitle
    ul ! aClass "paste-specs" $ do
      detail "Paste" $ pasteLink paste $ "#" ++ show pasteId
      detail "Author" $ pasteAuthor
      detail "Language" $ showLanguage langs pasteLanguage
      detail "Channel" $ do showChannel chans pasteChannel
                            " "
                            showContextLink paste chans pasteChannel
      detail "Created" $ showDateTime pasteDate
      detail "Raw" $ pasteRawLink paste $ ("View raw link" :: Text)
    clear

    where detail title content = do
            li $ do strong (title ++ ":"); toHtml content

showContextLink :: Paste -> [Channel] -> Maybe ChannelId -> Html
showContextLink Paste{..} chans chid =
  case chid >>= \chid -> find ((==chid).channelId) chans of
    Nothing -> return ()
    Just Channel{..} -> do
      let uri = "/irc/" ++ T.unpack (T.dropWhile (=='#') channelName) ++
                "/" ++ showIrcDateTime pasteDate ++ "/" ++ show pasteId
      href uri ("Context in IRC" :: String)

-- | Individual paste navigation.
pasteNav :: [Language] -> [Paste] -> Paste -> Html
pasteNav langs pastes paste =
  H.div ! aClass "paste-nav" $ do
    diffLink
    stepsLink
    href ("/edit/" ++ pack (show pid) ++ "") ("Annotate" :: Text)
    " - "
    href ("/report/" ++ pack (show pid) ++ "") ("Report/Delete" :: Text)
    
    where pid = pasteId paste
          pairs = zip (drop 1 pastes) pastes
          parent = fmap snd $ find ((==pid).pasteId.fst) $ pairs
          diffLink =
            case parent of
              Nothing -> return ()
              Just Paste{pasteId=prevId} -> do
                href ("/diff/" ++ show prevId ++ "/" ++ show pid)
                     ("Diff" :: Text)
                " - "
          stepsLink
            | lang == Just "haskell" = do href ("/steps/" ++ show pid)
                                               ("Steps" :: Text)
                                          " - "
            | otherwise = return ()
          lang = pasteLanguage paste >>= (`lookup` ls)
          ls = map (languageId &&& languageName) langs

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
