#!/usr/bin/env cabal

{- --------------------------------------------------------------- -}
{- How the EmulatorTrace can know the state of a Contract ?        -}
{-                                                                 -}
{-   Use the Contract monad writer and `observableState` from      -}
{-   `Plutus.Trace`. This function gets a handle's contract state  -}
{-   and reports the observable state field of the instance.       -}
{-                                                                 -}
{-   Note that there is no observable state right after calling    -}
{-   `activateContractWallet` as the instance may not exist yet.   -}
{-   Avoid name shadowing for contract handles that can hide such  -}
{-   situation.                                                    -}
{-                                                                 -}
{-   A useful type for the Contract writer is `Last a`             -}
{-   from `Data.Monoid`.                                           -}
{-                                                                 -}
{- --------------------------------------------------------------- -}

{- cabal:
   build-depends:
    , base
    , data-default
    , freer-extras
    , plutus-contract
    , text
-}

{-# LANGUAGE FlexibleContexts               #-}
{-# LANGUAGE ImportQualifiedPost            #-}
{-# LANGUAGE LambdaCase                     #-}
{-# LANGUAGE NoImplicitPrelude              #-}
{-# LANGUAGE TypeApplications               #-}

import Control.Monad                        ( void )
import Control.Monad.Freer.Extras.Log qualified
    as Extras                               ( logInfo )

import Data.Default                         ( def )
import Data.Monoid                          ( Last(..) )
import Data.Text                            ( Text )

import System.IO                            ( stdout )

import Prelude                              ( String, Maybe(..), IO, (.), ($), (<>), (>>), (>>=), show )

import Plutus.Contract                      ( Contract, EmptySchema, tell )
import Plutus.Contract.Request qualified
    as Contract                             ( waitNSlots )
import Plutus.Contract.Test                 ( w1 )

import Plutus.Trace
    ( EmulatorTrace
    , TraceConfig(..)
    , runEmulatorTraceIO'
    , activateContractWallet
    , waitNSlots
    , observableState
    )

import Plutus.Trace.Emulator.Types          ( UserThreadMsg(..) )
import Wallet.Emulator.MultiAgent           ( EmulatorEvent'(..) )

{- --------------------------------------------------------------- -}
{- Emulator TraceConfig                                            -}
{- --------------------------------------------------------------- -}

simpleTraceConfig :: TraceConfig
simpleTraceConfig =
    TraceConfig
        { showEvent     = simpleShowEvent
        , outputHandle  = stdout
        }
    where
        simpleShowEvent :: EmulatorEvent' -> Maybe String
        simpleShowEvent = \case
            UserThreadEvent (UserLog msg) -> Just $ "*** USER LOG: " <> msg
            _                             -> Nothing

{- --------------------------------------------------------------- -}
{- Off-chain code                                                  -}
{- --------------------------------------------------------------- -}

action :: Contract (Last String) EmptySchema Text ()
action =
    do
        tell (Last $ Just "Begin")
        void (Contract.waitNSlots 3)
        tell (Last $ Just "End")

{- --------------------------------------------------------------- -}
{- Main code                                                       -}
{- --------------------------------------------------------------- -}

emulatorTrace :: EmulatorTrace ()
emulatorTrace = do
    h1 <- activateContractWallet w1 action
    waitNSlots 1 >> showObservableState h1 -- wait for contract instance creation
    waitNSlots 5 >> showObservableState h1 -- wait past action delay
  where
    showObservableState h =
        observableState h >>= Extras.logInfo @String . show . getLast

main :: IO ()
main = runEmulatorTraceIO' simpleTraceConfig def emulatorTrace
