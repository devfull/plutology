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

{-# LANGUAGE DataKinds                      #-}
{-# LANGUAGE ImportQualifiedPost            #-}
{-# LANGUAGE LambdaCase                     #-}
{-# LANGUAGE NoImplicitPrelude              #-}
{-# LANGUAGE TypeApplications               #-}

import Control.Monad                        ( void, forever )

import Data.Aeson qualified
    as Aeson                                ( Value(..) )
import Data.Default                         ( def )
import Data.Text                            ( Text )

import System.IO                            ( stdout )

import Prelude                              ( String, Maybe(..), IO, ($), (<>), (>>), show )

import Plutus.Contract
    ( AsContractError
    , Contract
    , Endpoint
    , awaitPromise
    , endpoint
    , logInfo
    )
import Plutus.Contract.Request qualified
    as Contract                             ( waitNSlots )
import Plutus.Contract.Test                 ( w1 )

import Plutus.Trace
    ( EmulatorTrace
    , TraceConfig(..)
    , activateContractWallet
    , callEndpoint
    , runEmulatorTraceIO'
    , waitNSlots
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
{- Off-chain code                                                  -}
{- --------------------------------------------------------------- -}

type ActionSchema = Endpoint "action" String

action :: AsContractError e => String -> Contract w s e ()
action xs =
    actionDelay >> logInfo @String xs
  where
    actionDelay = void $ Contract.waitNSlots 3

endpoints :: Contract () ActionSchema Text ()
endpoints =
    endpointsDelay >> forever (awaitPromise (endpoint @"action" action))
  where
    endpointsDelay = void $ Contract.waitNSlots 3

{- --------------------------------------------------------------- -}
{- Main code                                                       -}
{- --------------------------------------------------------------- -}

emulatorTrace :: EmulatorTrace ()
emulatorTrace = do
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w1 endpoints

    callEndpoint @"action" h1 "h1 first (missed)"

    void $ waitNSlots 5 -- wait past endpoints delay

    callEndpoint @"action" h2 "h2 first"
    callEndpoint @"action" h2 "h2 second (missed)"

    void $ waitNSlots 5 -- wait another action delay

    callEndpoint @"action" h2 "h2 last"

    void $ waitNSlots 5 -- wait contract to finish

main :: IO ()
main = runEmulatorTraceIO' simpleTraceConfig def emulatorTrace
