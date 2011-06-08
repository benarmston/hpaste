{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | HTML-specific view functions.

module Amelie.View.Html
  (aClass
  ,aClasses
  ,darkSection
  ,lightSection
  ,lightNoTitleSection
  ,href
  ,clear
  ,showLanguage
  ,showChannel)
  where

import           Amelie.Types

import           Control.Arrow               ((&&&))
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid.Operator        ((++))
import           Data.Text.Lazy              (Text)
import qualified Data.Text.Lazy              as T
import           Prelude                     hiding ((++))
import           Text.Blaze.Html5            as H hiding (map)
import qualified Text.Blaze.Html5.Attributes as A

-- | A class prefixed with amelie-.
aClass :: AttributeValue -> Attribute
aClass name = A.class_ ("amelie-" ++ name)

-- | A class prefixed with amelie-.
aClasses :: [Text] -> Attribute
aClasses names = A.class_ $
  toValue $ T.intercalate " " $ map ("amelie-" ++) names

-- | A dark section.
darkSection :: Text -> Html -> Html
darkSection title inner =
  H.div ! aClasses ["section","section-dark"] $ do
    h2 $ toHtml title
    inner

-- | A light section.
lightSection :: Text -> Html -> Html
lightSection title inner =
  H.div ! aClasses ["section","section-light"] $ do
    h2 $ toHtml title
    inner

-- | A light section with no title.
lightNoTitleSection :: Html -> Html
lightNoTitleSection inner =
  H.div ! aClasses ["section","section-light"] $ do
    inner

-- | An anchor link.
href :: (ToValue location,ToHtml html) => location -> html -> Html
href loc content = H.a ! A.href (toValue loc) $ toHtml content

-- | A clear:both element.
clear :: Html
clear = H.div ! aClass "clear" $ return ()

showLanguage :: [Language] -> Maybe LanguageId -> Html
showLanguage languages lid =
  toHtml $ fromMaybe "-" (lid >>= (`lookup` langs))

    where langs = map (languageId &&& languageTitle) languages

showChannel :: [Channel] -> Maybe ChannelId -> Html
showChannel channels lid =
  toHtml $ fromMaybe "-" (lid >>= (`lookup` langs))

    where langs = map (channelId &&& channelName) channels
