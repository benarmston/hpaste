{-# OPTIONS -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Newtypes; foreign keys and such.

module Amelie.Types.Newtypes
       (PasteId(..)
       ,ChannelId(..)
       ,LanguageId(..))
       where

import Database.PostgreSQL.Simple.Result (Result)
import Database.PostgreSQL.Simple.Param (Param)

newtype PasteId = PasteId Integer
  deriving (Integral,Real,Num,Ord,Eq,Enum,Result,Param)

instance Show PasteId where show (PasteId pid) = show pid

newtype ChannelId = ChannelId Integer
  deriving (Integral,Real,Num,Ord,Eq,Enum,Result,Param)

instance Show ChannelId where show (ChannelId pid) = show pid

newtype LanguageId = LanguageId Integer
  deriving (Integral,Real,Num,Ord,Eq,Enum,Result,Param)

instance Show LanguageId where show (LanguageId pid) = show pid
