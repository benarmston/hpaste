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

import Control.Applicative        (Applicative,Alternative)
import Control.Monad              (MonadPlus)
import Control.Monad.Catch        (MonadCatchIO)
import Control.Monad.Reader       (ReaderT,MonadReader)
import Control.Monad.Trans        (MonadIO)
import Database.PostgreSQL.Simple (Connection)
import Snap.Types                 (Snap,MonadSnap)

-- | The state accessible to the controller (DB/session stuff).
data ControllerState = ControllerState {
    controllerStateConn  :: Connection
  , controllerStateCache :: Cache
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
    modelStateConn :: Connection
  }

-- | The model monad (limited access to IO, only DB access).
newtype Model a = Model {
    runModel :: ReaderT ModelState IO a
  } deriving (Monad,Functor,Applicative,MonadReader ModelState)
