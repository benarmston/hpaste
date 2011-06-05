{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | CSS generation.

module Text.CSS
  (module Text.CSS.Types
  ,module Text.CSS.Properties
  ,runCSS
  ,renderCSS
  ,renderPrettyCSS
  ,rule
  ,subRule)
    where

import           Text.CSS.Properties
import           Text.CSS.Types

import           Control.Monad.Writer (MonadWriter,runWriter,tell)
import           Data.Either          (lefts,rights)
import           Data.Monoid          (Monoid(..))
import           Data.Monoid.Operator ((++))
import           Data.Text.Lazy       (Text)
import qualified Data.Text.Lazy       as T
import           Prelude              hiding ((++))

-- | Generate CSS rules.
runCSS :: CSS Rule -> [Rule]
runCSS = snd . runWriter . unCSS

-- | Generate CSS properties.
runBody :: CSS (Either Property Rule) -> [(Either Property Rule)]
runBody = snd . runWriter . unCSS

-- | Render a CSS AST to text, flat.
renderCSS :: [Rule] -> Text
renderCSS = mconcat . map renderRule where
  renderRule (Rule _name [] []) = ""
  renderRule (Rule name props sub) =
    parent ++
    renderCSS (map prefix sub)
      where parent | null props = ""
                   | otherwise = name ++ "{" ++ renderProps props ++ "}"
            prefix subr@Rule{ruleExpr} =
              subr { ruleExpr = name ++ " " ++ ruleExpr }
  renderProps = T.intercalate ";" . map renderProp
  renderProp (Property name value) = name ++ ":" ++ value

-- | Render a CSS AST to text, pretty.
renderPrettyCSS :: [Rule] -> Text
renderPrettyCSS = mconcat . map renderRule where
  renderRule (Rule name props sub) =
    name ++ "{\n" ++ renderProps props ++ "\n}" ++ "\n" ++
    renderCSS (map prefix sub)
      where prefix subr@Rule{ruleExpr} =
              subr { ruleExpr = name ++ " " ++ ruleExpr }
  renderProps = T.intercalate ";\n" . map (("    "++) . renderProp)
  renderProp (Property name value) = name ++ ": " ++ value

-- | Make a CSS rule.
rule :: Text -> CSS (Either Property Rule) -> CSS Rule
rule name getProps = do
  let body = runBody getProps
  tell $ [Rule name (lefts body) (rights body)]

-- | Make a sub-CSS rule.
subRule :: Text -> CSS (Either Property Rule) -> CSS (Either Property Rule)
subRule name getProps = do
  let body = runBody getProps
  tell $ [Right $ Rule name (lefts body) (rights body)]
