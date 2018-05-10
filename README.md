# log-effect-syslog

This package contains a set of utility functions that provide ability to log to
syslog using
(extensible-effects)[https://hackage.haskell.org/package/extensible-effects]
and (log-effect)[https://hackage.haskell.org/package/log-effect].

## Example

```haskell
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Log
import Control.Eff.Log.Syslog

someComp :: ( [LogM IO SyslogMsg] <:: r
            , Lifted IO r
            ) => Eff r ()
someComp = do logInfo "Doing something"
              logDebug "Doing something else"
              {- ... -}
              logInfo "Ok, we're done"

main :: IO ()
main = runLift $ runSyslog "MyProgram" [LogPID] User
               $ someComp
```
