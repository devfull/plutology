#!/usr/bin/env cabal

{- --------------------------------------------------------------- -}
{- How to run an emulator trace on a purely off-chain contract ?   -}
{- --------------------------------------------------------------- -}

{- cabal:
   build-depends:
    , base
    , plutus-contract
    , text
-}

{-# LANGUAGE NoImplicitPrelude              #-}
{-# LANGUAGE OverloadedStrings              #-}
{-# LANGUAGE TypeApplications               #-}

import Control.Monad                        ( void )
import Data.Text                            ( Text )

import Prelude                              ( IO, ($) )

import Plutus.Contract                      ( Contract, EmptySchema, logInfo )
import Plutus.Contract.Test                 ( w1 )

import Plutus.Trace                         ( EmulatorTrace, runEmulatorTraceIO, activateContractWallet )

{- --------------------------------------------------------------- -}
{- Off-chain code                                                  -}
{- --------------------------------------------------------------- -}

contract :: Contract () EmptySchema Text ()
contract = logInfo @Text "Run off-chain code"

emulatorTrace :: EmulatorTrace ()
emulatorTrace = void $ activateContractWallet w1 contract

{- --------------------------------------------------------------- -}
{- Main code                                                       -}
{- --------------------------------------------------------------- -}

main :: IO ()
main = runEmulatorTraceIO emulatorTrace
