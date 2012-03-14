{-# OPTIONS -Wall #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Model-view-controller types.

module Amelie.Types.MVC
       (Controller(..)
       ,Model(..)
       ,ControllerState(..)
       ,ModelState(..))
       where

import Amelie.Types.Cache
import Amelie.Types.Config

import Control.Applicative        (Applicative,Alternative)
import Control.Concurrent.Chan    (Chan)
import Control.Monad              (MonadPlus)
import Control.Monad.Catch        (MonadCatchIO)
import Control.Monad.Reader       (ReaderT,MonadReader)
import Control.Monad.Trans        (MonadIO)
import Data.Text.Lazy             (Text)
import Database.PostgreSQL.Simple (Connection)
import Snap.Core                  (Snap,MonadSnap)

-- | The state accessible to the controller (DB/session stuff).
data ControllerState = ControllerState {
    controllerStateConfig :: Config
  , controllerStateConn   :: Connection
  , controllerStateCache  :: Cache
  , controllerStateAnns   :: Chan Text
  }

-- | The controller monad.
newtype Controller a = Controller {
    runController :: ReaderT ControllerState Snap a
  } deriving (Monad
             ,Functor
             ,Applicative
             ,Alternative
             ,MonadReader ControllerState
             ,MonadSnap
             ,MonadIO
             ,MonadPlus
             ,MonadCatchIO)

-- | The state accessible to the model (just DB connection).
data ModelState = ModelState {
    modelStateConn   :: Connection
  , modelStateAnns   :: Chan Text
  , modelStateConfig :: Config
  }

-- | The model monad (limited access to IO, only DB access).
newtype Model a = Model {
    runModel :: ReaderT ModelState IO a
  } deriving (Monad,Functor,Applicative,MonadReader ModelState,MonadIO)
