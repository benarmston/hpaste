{-# LANGUAGE RecordWildCards #-}

-- | Simple module for sending emails.

module Network.SendEmail where

import Control.Monad.IO
import Control.Monad.Trans
import Data.Maybe
import Network.SMTP.Simple
import Network.Socket

-- | An email to be sent via SMTP.
data Email =
  Email { emailSMTPHost :: String
        , emailEHLO :: String
        , emailFromName :: String
        , emailToName :: String
        , emailFromEmail :: String
        , emailToEmail :: String
        , emailSubject :: String
        , emailBody :: String
        }

-- | Send an SMTP email.
sendEmail :: (MonadIO m) => Email -> m ()
sendEmail Email{..} =
  io $ do
  addr <- lookupIP emailSMTPHost
  case addr of
    Just ip -> sendSimpleMessages putStrLn ip emailEHLO [msg]
    Nothing -> error "Unable to lookup the SMTP IP."
  where msg = SimpleMessage {
                from    = [NameAddr (Just emailFromName) emailFromEmail]
              , to      = [NameAddr (Just emailToName) emailToEmail]
              , subject = emailSubject
              , body    = emailBody
              }
        -- | Look up the IP address for the SMTP server.
        lookupIP :: MonadIO m => String -> m (Maybe String)
        lookupIP domain = io $ do
          let hints = defaultHints { addrFlags = [AI_ADDRCONFIG, AI_CANONNAME] }
          addrs <- getAddrInfo (Just hints) (Just domain) (Just "smtp")
          return $ listToMaybe $ map (takeWhile (/=':') . show . addrAddress) addrs
