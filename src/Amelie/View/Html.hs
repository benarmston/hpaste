{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | HTML-specific view functions.

module Amelie.View.Html
  (aClass
  ,aClasses)
  where

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

