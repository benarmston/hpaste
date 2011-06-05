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
  layout
  sections
  paste
  utils
  highlighter

-- | General layout styles.
layout :: CSS Rule
layout = do
  rule "body" $ do
    fontFamily "'DejaVu Sans', sans-serif"
    fontSize "13px"
    textAlign "center"
    
  classRule "logo" $ do
    margin "1em 0 1em 0"
    border "0"
  
  classRule "wrap" $ do
    maxWidth "50em"
    margin "auto"
    textAlign "left"

-- | Section styles.
sections :: CSS Rule
sections = do
  classRule "section" $ do
    borderRadius "5px"
    padding "10px"
    border "3px solid #000"
    margin "0 0 1em 0"
     
    subRule "h2" $ do
      margin "0"
      fontSize "1.2em"
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

-- | Paste view styles.
paste :: CSS Rule
paste = do
  classRule "paste-specs" $ do
    margin "0"
    padding "0"
    listStyle "none"
    lineHeight "1.5em"
    
    subRule "strong" $ do
      fontWeight "normal"
      width "8em"
      display "block"
      float "left"

-- | Utility styles to help with HTML weirdness.
utils :: CSS Rule
utils = do
  classRule "clear" $ do
    clear "both"

-- | A short-hand for prefixing rules with ‘.amelie-’.
classRule :: Text -> CSS (Either Property Rule) -> CSS Rule
classRule = rule . (".amelie-" ++)

-- | Styles for the highlighter.
highlighter :: CSS Rule
highlighter = do
  rule ".highlighttable" $ do
    tokens
    lineNumbers

    subRule "pre" $ do
      margin "0"

-- | Tokens colours and styles.
tokens :: CSS (Either Property Rule)
tokens = do
  subRule ".highlight" $ do
    tokenColor "cm" "#555"
    tokenColor "kr" "#397460"
    tokenColor "s" "#366354"
    tokenColor "sc" "#366354"
    tokenColor "se" "#743838"
    tokenColor "kt" "#4F4371"
    tokenColor "ow" "#333"
    tokenColor "o" "#3E394D"
    tokenColor "n" "#343634"
    tokenColor "nf" "#222"

  where token name props = subRule ("." ++ name) $ props
        tokenColor name col = token name $ color col

-- | The line number part.
lineNumbers :: CSS (Either Property Rule)
lineNumbers = do
  subRule ".linenodiv" $ do
    margin "0 1em 0 0"

    subRule "a" $ do
      textDecoration "none"
      color "#555"
