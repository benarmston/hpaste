{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module HJScript.Objects.JQuery.Extra where

import           HJScript
import           HJScript.Objects.JQuery
import qualified HJScript.Objects.Math   as Math
import           Prelude                 hiding ((++),max)

-- | jQuery selector.
j :: String -> JObject JQuery
j = selectExpr . string

-- | Set the width of a DOM element.
setWidth :: IsJQuery o => Exp Int -> o -> HJScript ()
setWidth w o = do
  runExp $ methodCall "width" w o

-- | Add a class to an object.
addClass :: IsJQuery o => String -> o -> HJScript ()
addClass w o = do
  runExp $ methodCall "addClass" (string w) o

-- | Does an object have a class?
hasClass :: IsJQuery o => String -> o -> JBool
hasClass w o = do
  methodCall "hasClass" (string w) o

-- | Remove a class from an object.
removeClass :: IsJQuery o => String -> o -> HJScript ()
removeClass w o = do
  runExp $ methodCall "removeClass" (string w) o

-- | Set the width of a DOM element.
css' :: IsJQuery o => String -> String -> o -> HJScript ()
css' key value o = do
  runExp $ methodCall "css" (string key,string value) o

-- | Set the width of a DOM element.
css :: IsJQuery o => String -> String -> o -> HJScript ()
css key value o = do
  runExp $ methodCall "css" (string key,string value) o

-- | Get the width of a DOM element.
getWidth :: IsJQuery o => o -> Exp Int
getWidth o = do
  methodCall "width" () o

-- | When toggling by clicking, run these events on this object.
toggle :: IsJQuery o => HJScript JBool -> HJScript JBool -> o -> HJScript ()
toggle on off query = do
  fnon <- function $ \() -> on
  fnoff <- function $ \() -> off
  runExp $ methodCall "toggle" (fnon,fnoff) query

-- | When toggling by hover, run these events on this object.
hover :: IsJQuery o => HJScript JBool -> HJScript JBool -> o -> HJScript ()
hover on off query = do
  fnon <- function $ \() -> on
  fnoff <- function $ \() -> off
  runExp $ methodCall "hover" (fnon,fnoff) query

-- | For each object in a jQuery selection.
each :: IsJQuery o => HJScript JBool -> o -> HJScript ()
each script query = do
  fn <- function $ \() -> script
  runExp $ methodCall "each" fn query

-- | The jQuery 'this' object.
this' :: JObject JQuery
this' = selectExpr (this :: JObject JQuery)

-- | Parent of a jQuery object.
parent :: IsJQuery o => o -> JObject JQuery
parent o = callMethod "parent" () o

-- | Max.
mathMax :: Exp a -> Exp a -> Exp a
mathMax a b = callMethod "max" (a,b) Math.Math

-- | Simple instance so we can use number literals and simple
-- | arithmetic.
instance Num (Exp Int) where
  a + b = a .+. b
  a * b = a .*. b
  abs = undefined
  signum = undefined
  fromInteger = int . fromIntegral
instance Eq (Exp Int)

class (IsDeref o) => IsJQuery o
instance IsJQuery (JObject JQuery)
instance IsClass (Var JQuery)
instance IsJQuery (Var JQuery)
