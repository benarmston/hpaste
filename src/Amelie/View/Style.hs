{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Page style.

module Amelie.View.Style
  (style)
  where

import Data.Monoid.Operator ((++))
import Data.Text.Lazy       (Text)
import Prelude              hiding ((++))
import Language.CSS

-- | Side-wide style sheet.
style :: Text
style = renderCSS $ runCSS $ do
  layout
  sections
  paste
  utils
  highlighter
  hints
  form
  home
  browse
  footer
  activity
  ircEntries
  
-- | IRC log entries.
ircEntries :: CSS Rule
ircEntries = do
  classRule "irc-entries" $ do
    marginLeft "0"
    paddingLeft "0"
    listStyle "none"
    rule ".current" $ do
      fontWeight "bold"
      marginTop "1em"
      marginBottom "1em"

-- | Footer.
footer :: CSS Rule
footer = do
  classRule "footer" $ do
    textAlign "center"
    rule "a" $ do 
      textDecoration "none"
    rule "a:hover" $ do
      textDecoration "underline"

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
    margin "auto"
    textAlign "left"
    
  classRule "nav" $ do
    float "right"
    marginTop "1em"

-- | Paste form.
form :: CSS Rule
form = do
  inputs
  classRule "spam" $ do
    display "none"
  classRule "errors" $ do
    color "#743838"
    fontWeight "bold"

-- | Input style.
inputs :: CSS Rule
inputs =
  rule "form p label" $ do
    rule "textarea" $ do
      width "100%"
      height "20em"
      clear "both"
      margin "1em 0 0 0"
         
    rule "textarea, input.text" $ do
      border "2px solid #ddd"
      borderRadius "4px"
    rule "textarea:focus, input.text:focus" $ do
      background "#eee"
      
    rule "span" $ do
      float "left"
      width "7em"
      display "block"

-- | Section styles.
sections :: CSS Rule
sections = do
  classRule "section" $ do
    borderRadius "5px"
    padding "10px"
    border "3px solid #000"
    margin "0 0 0.5em 0"
     
    rule "h2" $ do
      margin "0"
      fontSize "1.2em"
      padding "0"
  
  classRule "section-dark" $ do
    background "#453D5B"
    borderColor "#A9A0D2"
    color "#FFF"

    rule "h2" $ do
      color "#FFF"
    
    rule "a" $ do
      color "#8ae0c2"
      textDecoration "none"

    rule "a:hover" $ do
      textDecoration "underline"

  classRule "section-light" $ do
    background "#FFF"
    borderColor "#EEE"
    color "#000"

    rule "h2" $ do
      color "#2D2542"
   
  classRule "section-error" $ do
    background "#FFDFDF"
    color "#5b4444"
    border "1px solid #EFB3B3"

    rule "pre" $ do
      margin "0"
    rule "h2" $ do
      color "#2D2542"
   
  classRule "section-warn" $ do
    background "#FFF9C7"
    color "#915c31"
    border "1px solid #FFF178"
    rule "pre" $ do
      margin "0"
    rule "h2" $ do
      color "#2D2542"

-- | Paste view styles.
paste :: CSS Rule
paste = do
  classRule "paste-specs" $ do
    margin "0.5em 0 0 0"
    padding "0"
    listStyle "none"
    lineHeight "1.5em"
    
    rule "strong" $ do
      fontWeight "normal"
      width "8em"
      display "block"
      float "left"
  classRule "paste-nav" $ do
    float "right"

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
  diff
  classRule "steps" $ do
    marginTop "1em"
  classRule "steps-expr" $ do
    rule ".text" $ do
      width "300px"

  classRule "code" $ do
    tokens

    rule "pre" $ do
      margin "0"

    rule "td" $ do
      verticalAlign "top"
  lineNumbers
  
-- | Style for diff groups.
diff :: CSS Rule
diff = do
  classRule "diff-both" $
    return ()
  classRule "diff-first" $ do
    backgroundColor "#FDD"
    color "#695B5B"
  classRule "diff-second" $ do
    backgroundColor "#DFD"

-- | Tokens colours and styles.
tokens :: CSS (Either Property Rule)
tokens = do
  rule "pre" $ do
    marginTop "0"
    tokenColor "comment" "#555"
    tokenColor "keyword" "#397460"
    tokenColor "str" "#366354"
    tokenColor "conid" "#4F4371"
    tokenColor "varop" "#333"
    tokenColor "varid" "#333"
    tokenColor "num" "#4F4371"
  rule "pre" $ do
    rule ".diff" $ do
      color "#555"
    rule "code" $ do
      jcolor "title" "#333"
      jcolor "string" "#366354"
      jcolor "built_in" "#397460"
      jcolor "preprocessor" "#4F4371"
      jcolor "comment" "#555"
      jcolor "command" "#397460"
      jcolor "special" "#333"
      jcolor "formula" "#4F4371"
      jcolor "keyword" "#397460"
      jcolor "number" "#4F4371"
      rule ".header" $ do
        color "#555"
      rule ".addition" $ do
        backgroundColor "#FDD"
        color "#695B5B"
      rule ".deletion" $ do
        backgroundColor "#DFD"
        color "#000"
  where token name props = rule (".hs-" ++ name) $ props
        tokenColor name col = token name $ color col
        jcolor name col = rule ("." ++ name) $ color col

-- | The line number part.
lineNumbers :: CSS Rule
lineNumbers = do
  rule ".amelie-line-nums pre" $ do
    margin "0 1em 0 0"
    textAlign "right"
    rule "a" $ do
      textDecoration "none"
      color "#555"
    rule "a:target" $ do
      textDecoration "underline"
      color "#000"

-- | Home page styles.
home :: CSS Rule
home = do
  rule "#new" wrap
  classRule "browse-link" $ do
    margin "1em 0 0 0"
  classRule "latest-pastes" $ do
    marginTop "0.5em"
  
  where wrap = rule ".amelie-wrap" $ do
                 width "50em"

-- | Browse page styles.
browse :: CSS Rule
browse = do
  classRule "pagination" $ do
    textAlign "center"
    margin "1em"

    rule ".amelie-inner" $ do
      margin "auto"
      width "15em"

-- | Developer activity page styles.
activity :: CSS Rule
activity = do
  rule "#activity" $ do
    rule ".amelie-wrap" $ do
      width "50em"

-- | Hlint hints
hints :: CSS Rule
hints = do
  classRule "hint-highlight" $ do
    background "#333"
    color "#999"
    border "1px solid #444"
