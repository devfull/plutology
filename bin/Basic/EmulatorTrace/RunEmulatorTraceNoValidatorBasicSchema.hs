#!/usr/bin/env cabal

{- --------------------------------------------------------------- -}
{- How to run a purely off-chain contract with a schema ?          -}
{-                                                                 -}
{-   Use `callEndpoint` from `Plutus.Trace` on an instance         -}
{-   of a contract with a nonempty schema.                         -}
{-                                                                 -}
{- --------------------------------------------------------------- -}

{- cabal:
   build-depends:
    , base
    , plutus-contract
    , text
-}

{-# LANGUAGE DataKinds                      #-}
{-# LANGUAGE ImportQualifiedPost            #-}
{-# LANGUAGE NoImplicitPrelude              #-}
{-# LANGUAGE OverloadedStrings              #-}
{-# LANGUAGE TypeApplications               #-}
{-# LANGUAGE TypeOperators                  #-}

import Data.Text                            ( Text )
import Text.Printf                          ( printf )

import Prelude                              ( IO, Integer, String, (>>), ($) )

import Plutus.Contract
    ( Contract
    , Endpoint
    , type (.\/)
    , awaitPromise
    , endpoint
    , logInfo
    , select
    )

import Plutus.Contract.Test                 ( w1 )

import Plutus.Trace qualified
    as Trace
    ( EmulatorTrace
    , activateContractWallet
    , callEndpoint
    , runEmulatorTraceIO
    )

{- --------------------------------------------------------------- -}
{- Off-chain code                                                  -}
{- --------------------------------------------------------------- -}

type MonetarySchema =
        Endpoint "mint" Integer
    .\/ Endpoint "burn" Integer

mint :: Integer -> Contract w s e ()
mint n = logInfo @String $ printf "Mint %d token" n

burn :: Integer -> Contract w s e ()
burn n = logInfo @String $ printf "Burn %d token" n

endpoints :: Contract () MonetarySchema Text ()
endpoints = awaitPromise (mintPromise `select` burnPromise) >> endpoints
  where
    mintPromise = endpoint @"mint" mint
    burnPromise = endpoint @"burn" burn

{- --------------------------------------------------------------- -}
{- Main code                                                       -}
{- --------------------------------------------------------------- -}

emulatorTrace :: Trace.EmulatorTrace ()
emulatorTrace = do
    h1 <- Trace.activateContractWallet w1 endpoints
    Trace.callEndpoint @"mint" h1 2
    Trace.callEndpoint @"burn" h1 1

main :: IO ()
main = Trace.runEmulatorTraceIO emulatorTrace
