{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Page style.

module Amelie.View.Style
  (style)
  where

import Data.Monoid.Operator ((++))
import Data.Text.Lazy       (Text)
import Prelude              hiding ((++))
import Text.CSS

-- | Side-wide style sheet.
style :: Text
style = renderCSS $ runCSS $ do
  rule "body" $ do
    fontFamily "'DejaVu Sans', sans-serif"
    fontSize "13px"
    textAlign "center"
    
  classRule "logo" $ do
    margin "0 0 1em 0"
  
  classRule "wrap" $ do
    maxWidth "50em"
    margin "auto"
    textAlign "left"
  
  classRule "section" $ do
    borderRadius "5px"
    padding "10px"
    border "3px solid #000"
    margin "0 0 1em 0"
     
    subRule "h2" $ do
      margin "0"
      fontSize "1em"
      padding "0 0 0.5em 0"
  
  classRule "section-dark" $ do
    background "#453D5B"
    borderColor "#A9A0D2"
    color "#FFF"

    subRule "h2" $ do
      color "#FFF"
    
    subRule "a" $ do
      color "#8ae0c2"
      textDecoration "none"

    subRule "a:hover" $ do
      textDecoration "underline"

  classRule "section-light" $ do
    background "#FFF"
    borderColor "#EEE"
    color "#000"

    subRule "h2" $ do
      color "#2D2542"

-- | A short-hand for prefixing rules with ‘.amelie-’.
classRule :: Text -> CSS (Either Property Rule) -> CSS Rule
classRule = rule . (".amelie-" ++)
