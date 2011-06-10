{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Page script.

module Amelie.View.Script
  (script)
  where

import           Data.Text.Lazy          (Text,pack)
import           HJScript
import           HJScript.Objects.JQuery
import qualified HJScript.Objects.Math as Math
import           Prelude                 hiding ((++),max)

-- | All scripts on the site. Not much to do.
script :: Text
script = pack $ show $ snd $ evalHJScript $ do
  ready $ do
    each (setWidth (j ".amelie-wrap")
                   (mathMax (getWidth this' + 50) 500))
         (j ".amelie-code,.amelie-latest-pastes")

-- | jQuery selector.
j :: String -> JObject JQuery
j = selectExpr . string

-- | Set the width of a DOM element.
setWidth :: JObject JQuery -> Exp Int -> HJScript ()
setWidth o w = do
  runExp $ methodCall "width" w o

-- | Get the width of a DOM element.
getWidth :: JObject JQuery -> Exp Int
getWidth o = do
  methodCall "width" () o

-- | For each object in a jQuery selection.
each :: HJScript () -> JObject JQuery -> HJScript ()
each script query
    = do fn <- procedure $ \() -> script
         runExp $ methodCall "each" fn query

-- | The jQuery 'this' object.
this' :: JObject JQuery
this' = selectExpr (this :: JObject JQuery)

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
