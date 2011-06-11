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
  ,warnNoTitleSection
  ,errorNoTitleSection
  ,href
  ,clear
  ,showLanguage
  ,showChannel
  ,paginate)
  where

import           Amelie.Types

import           Control.Arrow               ((&&&))
import           Control.Monad               (when)
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid.Operator        ((++))
import           Data.Text                   (pack)
import           Data.Text.Lazy              (Text)
import qualified Data.Text.Lazy              as T
import           Prelude                     hiding ((++))
import           Text.Blaze.Html5            as H hiding (map,nav)
import qualified Text.Blaze.Html5.Attributes as A

-- | A class prefixed with amelie-.
aClass :: AttributeValue -> Attribute
aClass name = A.class_ ("amelie-" ++ name)

-- | A class prefixed with amelie-.
aClasses :: [Text] -> Attribute
aClasses names = A.class_ $
  toValue $ T.intercalate " " $ map ("amelie-" ++) names

-- | A warning section.
warnNoTitleSection :: Html -> Html
warnNoTitleSection inner =
  H.div ! aClasses ["section","section-warn"] $ do
    inner

-- | An error section.
errorNoTitleSection :: Html -> Html
errorNoTitleSection inner =
  H.div ! aClasses ["section","section-error"] $ do
    inner

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

-- | Show a language.
showLanguage :: [Language] -> Maybe LanguageId -> Html
showLanguage languages lid =
  toHtml $ fromMaybe "-" (lid >>= (`lookup` langs))

    where langs = map (languageId &&& languageTitle) languages

-- | Show a channel.
showChannel :: [Channel] -> Maybe ChannelId -> Html
showChannel channels lid =
  toHtml $ fromMaybe "-" (lid >>= (`lookup` langs))

    where langs = map (channelId &&& channelName) channels

-- | Render results with pagination.
paginate :: Pagination -> Html -> Html
paginate pn inner = do
  nav pn True
  inner
  nav pn False
  
-- | Show a pagination navigation, with results count, if requested.
nav :: Pagination -> Bool -> Html
nav pn@Pagination{..} showTotal = do
  H.div ! aClass "pagination" $ do
    H.div ! aClass "inner" $ do
      when (pnPage-1 > 0) $ navDirection pn (-1) "Previous"
      toHtml (" " :: Text)
      when (pnResults == pnLimit) $ navDirection pn 1 "Next"
      when showTotal $ do
        br
        toHtml $ results

    where results = unwords [show start ++ "—" ++ show end
                            ,"results of"
                            ,show pnTotal]
          start = 1 + (pnPage - 1) * pnResults
          end = pnPage * pnResults

-- | Link to change navigation page based on a direction.
navDirection :: Pagination -> Integer -> Text -> Html
navDirection Pagination{..} change caption = do
  href url caption

    where url = pnRoot ++ "/page/" ++ 
                pack (show $ pnPage + change)
