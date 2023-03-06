#!/usr/bin/env cabal

{- --------------------------------------------------------------- -}
{- How to mint a token ?                                           -}
{-                                                                 -}
{-   Use `mustMintValue` and provide `plutusV1MintingPolicy`       -}
{-   from `Ledger.Constraints` to `submitTxConstraintsWith`.       -}
{-                                                                 -}
{- --------------------------------------------------------------- -}

{- cabal:
   build-depends:
    , base
    , plutus-contract
    , plutus-ledger
    , plutus-ledger-api
    , plutus-ledger-constraints
    , plutus-script-utils
    , plutus-tx
    , plutus-tx-plugin
    , text
-}

{-# LANGUAGE DataKinds                      #-}
{-# LANGUAGE ImportQualifiedPost            #-}
{-# LANGUAGE NoImplicitPrelude              #-}
{-# LANGUAGE OverloadedStrings              #-}
{-# LANGUAGE TemplateHaskell                #-}
{-# LANGUAGE TypeApplications               #-}

import Control.Monad                        ( void )
import Data.Text                            ( Text )

import Prelude                              ( IO, Integer, (.), ($), (<>), (>>), (>>=) )

import PlutusTx qualified                   ( BuiltinData, compile )

import Plutus.Contract.Test                 ( w1 )

import Plutus.V1.Ledger.Api qualified
    as Ledger                               ( MintingPolicy, mkMintingPolicyScript )
import Plutus.V1.Ledger.Value qualified
    as Ledger                               ( TokenName, Value, singleton )
import Ledger.Tx qualified                  ( getCardanoTxId )

import Ledger.Typed.Scripts qualified
    as Scripts                              ( Any )
import Plutus.Script.Utils.V1.Scripts qualified
    as Script                               ( scriptCurrencySymbol )

import Ledger.Constraints qualified
    as Constraints                          ( mustMintValue, plutusV1MintingPolicy, unspentOutputs )

import Plutus.Trace
    ( EmulatorTrace
    , activateContractWallet
    , runEmulatorTraceIO
    , waitNSlots
    )

import Plutus.Contract
    ( Contract
    , EmptySchema
    , awaitTxConfirmed
    , logInfo
    , ownAddress
    , submitTxConstraintsWith
    , utxosAt
    )

{- --------------------------------------------------------------- -}
{- On-chain code                                                   -}
{- --------------------------------------------------------------- -}

{-# INLINABLE mkMintingPolicy #-}
mkMintingPolicy :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkMintingPolicy _ _ = ()

mintingPolicy :: Ledger.MintingPolicy
mintingPolicy = Ledger.mkMintingPolicyScript
    $$(PlutusTx.compile [||mkMintingPolicy||])

{- --------------------------------------------------------------- -}
{- Off-chain code                                                  -}
{- --------------------------------------------------------------- -}

singleton :: Ledger.MintingPolicy -> Ledger.TokenName -> Integer -> Ledger.Value
singleton = Ledger.singleton . Script.scriptCurrencySymbol

contract :: Contract () EmptySchema Text ()
contract = do
    logInfo @Text "Mint 1 token"

    utxosInWallet <- ownAddress >>= utxosAt

    void $ submitTxConstraintsWith
        @Scripts.Any
        (Constraints.plutusV1MintingPolicy mintingPolicy <> Constraints.unspentOutputs utxosInWallet)
        (Constraints.mustMintValue $ singleton mintingPolicy "token" 1)
            >>= awaitTxConfirmed . Ledger.Tx.getCardanoTxId

emulatorTrace :: EmulatorTrace ()
emulatorTrace =
    void $ activateContractWallet w1 contract
        >> waitNSlots 2

{- --------------------------------------------------------------- -}
{- Main code                                                       -}
{- --------------------------------------------------------------- -}

main :: IO ()
main = runEmulatorTraceIO emulatorTrace
