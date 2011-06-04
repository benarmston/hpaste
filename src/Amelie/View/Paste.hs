{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Paste views.

module Amelie.View.Paste
  (pasteFormlet
  ,page)
  where

import           Amelie.Types
import           Amelie.View.Layout

import           Control.Applicative    ((<$>),(<*>),pure)
import           Control.Monad          (when)
import           Data.ByteString.UTF8
import qualified Data.Map               as M
import           Data.Text              (Text)
import           Safe                   (readMay)
import           Snap.Types             (Params)
import           Text.Blaze.Html5       as H hiding (map)
import           Text.Blaze.Html5.Extra
import           Text.Formlet

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
viewPaste Paste{..} = do
  p "Hello!"
  return ()
