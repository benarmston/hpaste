{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

module Text.Blaze.Extra where

import Control.Monad
import Data.Monoid
import Data.Monoid.Operator
import Prelude                     hiding ((++),head,div)
import Text.Blaze.Html5            as H hiding (map)
import Text.Blaze.Html5.Attributes as A
import Network.URI
import Text.Printf
import Data.List (intercalate)

elem !. className = elem ! class_ className

elem !# idName = elem ! A.id idName

linesToHtml :: String -> Html
linesToHtml str = forM_ (lines str) $ \line -> do toHtml line; br

htmlIntercalate :: Html -> [Html] -> Html
htmlIntercalate _ [x] = x
htmlIntercalate sep (x:xs) = do x; sep; htmlIntercalate sep xs
htmlIntercalate _ []  = mempty

htmlCommasAnd :: [Html] -> Html
htmlCommasAnd [x] = x
htmlCommasAnd [x,y] = do x; " and "; y
htmlCommasAnd (x:xs) = do x; ", "; htmlCommasAnd xs
htmlCommasAnd []  = mempty

htmlCommas = htmlIntercalate ", "
