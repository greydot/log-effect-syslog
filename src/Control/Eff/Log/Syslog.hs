{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Eff.Log.Syslog where

import Control.Eff
import Control.Eff.Lift
import Control.Eff.Log
import Control.Monad.Base (MonadBase,liftBase)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Char (toLower)
import Data.Monoid (mconcat)
import System.Posix.Syslog (Priority(..), syslog)

data SyslogMsg = SyslogMsg Priority ByteString

instance LogMessage SyslogMsg where
  -- Format:
  -- [priority] message
  toMsg (SyslogMsg p s) = mconcat [ "["
                                  , pack $ map toLower $ show p
                                  , "] "
                                  , s
                                  ]

syslogLogger :: MonadBase IO m => Logger m SyslogMsg
syslogLogger (SyslogMsg p s) = liftBase $ unsafeUseAsCStringLen s $ syslog Nothing p

logSyslog :: ( LogMessage l
             , MonadBase IO m
             , Member (LogM m SyslogMsg) r
             , Lifted m r)
          => Priority -> l -> Eff r ()
logSyslog p l = logM (SyslogMsg p $ toMsg l)
{-# INLINE logSyslog #-}

logDebug :: ( LogMessage l
            , MonadBase IO m
            , Member (LogM m SyslogMsg) r
            , Lifted m r)
         => l -> Eff r ()
logDebug = logSyslog Debug
{-# INLINE logDebug #-}

logInfo :: ( LogMessage l
           , MonadBase IO m
           , Member (LogM m SyslogMsg) r
           , Lifted m r)
        => l -> Eff r ()
logInfo = logSyslog Info
{-# INLINE logInfo #-}

logNotice :: ( LogMessage l
             , MonadBase IO m
             , Member (LogM m SyslogMsg) r
             , Lifted m r)
          => l -> Eff r ()
logNotice = logSyslog Notice
{-# INLINE logNotice #-}

logWarning :: ( LogMessage l
              , MonadBase IO m
              , Member (LogM m SyslogMsg) r
              , Lifted m r)
           => l -> Eff r ()
logWarning = logSyslog Warning
{-# INLINE logWarning #-}

logError :: ( LogMessage l
            , MonadBase IO m
            , Member (LogM m SyslogMsg) r
            , Lifted m r)
         => l -> Eff r ()
logError = logSyslog Error
{-# INLINE logError #-}

logCritical :: ( LogMessage l
               , MonadBase IO m
               , Member (LogM m SyslogMsg) r
               , Lifted m r)
            => l -> Eff r ()
logCritical = logSyslog Critical
{-# INLINE logCritical #-}

logAlert :: ( LogMessage l
            , MonadBase IO m
            , Member (LogM m SyslogMsg) r
            , Lifted m r)
         => l -> Eff r ()
logAlert = logSyslog Alert
{-# INLINE logAlert #-}

logEmergency :: ( LogMessage l
                , MonadBase IO m
                , Member (LogM m SyslogMsg) r
                , Lifted m r)
             => l -> Eff r ()
logEmergency = logSyslog Emergency
{-# INLINE logEmergency #-}
