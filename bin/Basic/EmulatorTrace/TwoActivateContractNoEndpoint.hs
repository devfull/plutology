#!/usr/bin/env cabal

{- --------------------------------------------------------------- -}
{- What happen when activating the same contract again ?           -}
{-                                                                 -}
{-   When two `activateContractWallet` are run on the same wallet  -}
{-   and the same contract, the two contracts are initialized      -}
{-   within a different instance.                                  -}
{-                                                                 -}
{-   If the contract has a non empty schema, it could wait for an  -}
{-   endpoint call with `awaitPromise`. Both instance would then   -}
{-   wait until a `callEndpoint` is made for their handler.        -}
{-                                                                 -}
{-   Otherwise, the two contracts are immediately run in parallel. -}
{-                                                                 -}
{- --------------------------------------------------------------- -}

{- cabal:
   build-depends:
    , aeson
    , base
    , data-default
    , plutus-contract
    , text
-}

{-# LANGUAGE ImportQualifiedPost            #-}
{-# LANGUAGE LambdaCase                     #-}
{-# LANGUAGE NoImplicitPrelude              #-}
{-# LANGUAGE TypeApplications               #-}

import Control.Monad                        ( void )

import Data.Aeson qualified
    as Aeson                                ( Value(..) )
import Data.Default                         ( def )
import Data.Text                            ( Text )

import System.IO                            ( stdout )

import Prelude                              ( String, Maybe(..), IO, ($), (<>), show )

import Plutus.Contract                      ( EmptySchema, logInfo )
import Plutus.Contract.Test                 ( w1 )

import Plutus.Trace
    ( EmulatorTrace
    , TraceConfig(..)
    , runEmulatorTraceIO'
    , activateContractWallet
    )

import Plutus.Trace.Emulator.Types          ( ContractInstanceLog(..), ContractInstanceMsg(..) )

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
            InstanceEvent (ContractInstanceLog (ContractLog (Aeson.String msg)) _ _) -> Just $ "*** CONTRACT LOG: " <> show msg
            _                                                                        -> Nothing

{- --------------------------------------------------------------- -}
{- Main code                                                       -}
{- --------------------------------------------------------------- -}

emulatorTrace :: EmulatorTrace ()
emulatorTrace = do
    void $ activateContractWallet w1 $ logInfo @String @() @EmptySchema @Text "h1"
    void $ activateContractWallet w1 $ logInfo @String @() @EmptySchema @Text "h2"

main :: IO ()
main = runEmulatorTraceIO' simpleTraceConfig def emulatorTrace
