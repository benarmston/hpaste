{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

-- | Forms used throughout.

module Amelie.View.Forms where

import           Amelie.Types

import           Control.Applicative
import           Control.Monad.Error
import           Data.Either.Extra
import           Data.Maybe
import           Prelude                     hiding ((++))
import           Snap.Types
import           Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html5.Extra
import           Text.Formlet

pasteForm :: Snap (Html,Maybe Paste)
pasteForm = do
  params <- getParams
  let value = formletValue formlet params
  submitted <- isJust <$> getParam "submit"
  let form = postForm $ do
        formletHtml formlet params
        submitInput "submit" "Submit"
        when submitted $ whenLeft value (mapM_ (p . toHtml))
  return (form,either (const Nothing) Just $ value)

  where formlet = Paste <$> req (textInput "title" "Title")
                        <*> req (textInput "author" "Author")
                        <*> opt (dropInput "language" "Language")
                        <*> opt (dropInput "channel" "Channel")
                        <*> req (areaInput "paste" "Paste")
