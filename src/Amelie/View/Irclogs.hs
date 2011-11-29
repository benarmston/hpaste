{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Irclogs page view.

module Amelie.View.Irclogs
  (page)
  where

import           Amelie.Types
import           Amelie.View.Html
import           Amelie.View.Layout

import           Control.Monad
import           Data.Char
import           Data.Maybe
import           Data.Monoid.Operator ((++))
import           Data.String
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Prelude              hiding ((++))
import           Text.Blaze.Extra
import           Text.Blaze.Html5     as H hiding (map)
import qualified Text.Blaze.Html5.Attributes   as A

-- | Render the irclogs page.
page :: String -> String -> String -> Either String [Text] -> Maybe Integer -> Html
page channel date time entries pid =
  layoutPage $ Page {
    pageTitle = "Development irclogs"
  , pageBody = irclogs pid channel entries
  , pageName = "irclogs"
  }

-- | View the paginated pastes.
irclogs :: Maybe Integer -> String -> Either String [Text] -> Html
irclogs pid channel entries = do
  darkSection "IRC logs" $ do
    p $ do "Channel: #"; toHtml channel
  lightSection (fromString ("#" ++ channel)) $ do
    case entries of
      Left error    -> do "Unable to get logs for this channel and date: "
                          toHtml error
      Right entries -> 
        ul !. "amelie-irc-entries" $
          forM_ entries $ \entry -> do
            let date = toValue $ parseDate entry
                url = "http://hpaste.org/" ++ maybe "0" (T.pack . show) pid
                currentline | T.isSuffixOf url entry = "current"
                            | otherwise = ""
            li !. (toValue (currentline :: Text)) $ do
              a ! A.name date ! A.id date $ return ()
              toHtml entry

  where parseDate = T.replace ":" "-" . T.takeWhile (not.isSpace)
