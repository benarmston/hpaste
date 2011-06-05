{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleInstances #-}
{-# OPTIONS -fno-warn-name-shadowing -fno-warn-orphans #-}

-- | Mini formlets library.

module Text.Formlet
       (Formlet(..)
       ,formlet
       ,req
       ,opt
       ,wrap
       ,textInput
       ,dropInput
       ,areaInput
       ,submitInput) where

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.Reader
import           Control.Monad.Trans.Error   (ErrorList(..))
import           Control.Monad.Writer
import qualified Data.Map                    as M
import           Data.Maybe
import           Data.Monoid.Operator
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding
import           Prelude                     hiding ((++))
import           Snap.Types
import           Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

-- | A simple formlet data type, fails on first error.
data Formlet a = Formlet {
   formletValue  :: Params -> Either [Text] a
 , formletName   :: Maybe Text
 , formletHtml   :: Params -> Html
}

-- | Fails on first error, concatenates HTML output.
instance Applicative Formlet where
  pure a = Formlet { formletValue = const (return a)
                   , formletHtml  = const mempty
                   , formletName  = Nothing
                   }
  Formlet f n fhtml <*> Formlet v n' vhtml =
    Formlet { formletValue = \params ->
                case v params of
                  Right x -> f params <*> Right x
                  Left e  -> case f params <*> Left [] of
                               Right x -> return x
                               Left e' -> Left $ e' ++ e
            , formletHtml  = \params -> fhtml params ++ vhtml params
            , formletName  = case (n,n') of
                               (Just{},Just{}) -> Nothing
                               _               -> n `mplus` n'
            }

-- | Normal instance.
instance Functor Formlet where
  fmap f formlet@Formlet{..} = formlet { formletValue = value }
    where value = \params ->
                    case formletValue params of
                      Left e -> Left e
                      Right a -> Right (f a)

-- | The error message for the formlets is a text value.
instance Error Text where noMsg = ""; strMsg = T.pack
instance ErrorList Text where listMsg = return . T.pack

-- | Make a simple formlet.
formlet :: Text -> (Maybe Text -> Html) -> Formlet Text
formlet name html = 
  Formlet { formletValue = \inputs ->
              case (M.lookup (encodeUtf8 name) inputs) of
                Just (value:_) -> return $ decodeUtf8 value
                _ -> throwError $ ["missing input: " ++ name]
          , formletHtml = \inputs ->
              case M.lookup (encodeUtf8 name) inputs of
                Just (value:_) -> html (Just $ decodeUtf8 value)
                _ -> html Nothing
          , formletName = Just name
          }

-- | Make an input required (non-empty text).
req :: Formlet Text -> Formlet Text
req formlet@Formlet{..} =
  formlet { formletValue = \inputs ->
              case formletValue inputs of
                Right v | T.null v ->
                  throwError $ ["required input" ++ maybe "" (": "++) formletName]
                meh -> meh
          }

-- | Make an input optional (empty text is nothing).
opt :: Formlet Text -> Formlet (Maybe Text)
opt formlet@Formlet{..} =
  formlet { formletValue = \inputs ->
              case formletValue inputs of
                Right v | T.null v -> Right Nothing
                meh -> Just <$> meh
          }

-- | Wrap/transform formlet's HTML.
wrap :: (Html -> Html) -> Formlet Text -> Formlet Text
wrap f formlet@Formlet{..} = formlet { formletHtml = f . formletHtml }

-- | Make a text input formlet with a label.
textInput :: Text -> Text -> Formlet Text
textInput name caption =
  formlet name $ \value -> do
    p $ H.label $ do
      toHtml $ caption ++ ": "
      input ! A.name (toValue name)
            ! A.value (toValue $ fromMaybe "" value)

-- | Make a textarea input with a label.
areaInput :: Text -> Text -> Formlet Text
areaInput name caption =
  formlet name $ \value -> do
    p $ H.label $ do
      toHtml $ caption ++ ": "
      textarea ! A.name (toValue name) $ toHtml $ fromMaybe "" value

-- | Make a drop down input with a label.
dropInput :: Text -> Text -> Formlet Text
dropInput name caption =
  formlet name $ \value -> do
    p $ H.label $ do
      toHtml $ caption ++ ": "
      input ! A.name (toValue name)
            ! A.value (toValue $ fromMaybe "" value)

-- | Make a submit (captioned) button.
submitInput :: Text -> Text -> Html
submitInput name caption = p $ do
  p $ H.input ! A.type_ "submit"
              ! A.name (toValue name)
              ! A.value (toValue caption)
