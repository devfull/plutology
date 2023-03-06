#!/usr/bin/env cabal

{- --------------------------------------------------------------- -}
{- How to get the currency symbol of a minting policy script ?     -}
{- --------------------------------------------------------------- -}

{- cabal:
   build-depends:
    , base
    , plutus-ledger-api
    , plutus-script-utils
    , plutus-tx
    , plutus-tx-plugin
-}

{-# LANGUAGE DataKinds                      #-}
{-# LANGUAGE ImportQualifiedPost            #-}
{-# LANGUAGE NoImplicitPrelude              #-}
{-# LANGUAGE OverloadedStrings              #-}
{-# LANGUAGE TemplateHaskell                #-}

import Prelude                              ( IO, print )
import PlutusTx qualified                   ( BuiltinData, compile )

import PlutusTx.Trace qualified
    as PlutusTx                             ( trace, traceError )

import Plutus.V1.Ledger.Api qualified
    as Ledger                               ( MintingPolicy, mkMintingPolicyScript )

import Plutus.Script.Utils.V1.Scripts qualified
    as Script                               ( scriptCurrencySymbol )

{- --------------------------------------------------------------- -}
{- On-chain code                                                   -}
{- --------------------------------------------------------------- -}

{-# INLINABLE mkMintingPolicy #-}
mkMintingPolicy :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkMintingPolicy _ _ = ()

mintingPolicy :: Ledger.MintingPolicy
mintingPolicy = Ledger.mkMintingPolicyScript
    $$(PlutusTx.compile [||mkMintingPolicy||])

{-# INLINABLE mkMintingPolicyTrace #-}
mkMintingPolicyTrace :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkMintingPolicyTrace _ _ = PlutusTx.trace "trace" ()

mintingPolicyTrace :: Ledger.MintingPolicy
mintingPolicyTrace = Ledger.mkMintingPolicyScript
    $$(PlutusTx.compile [||mkMintingPolicyTrace||])

{-# INLINABLE mkMintingPolicyTraceError #-}
mkMintingPolicyTraceError :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
mkMintingPolicyTraceError _ _ = PlutusTx.traceError "traceError" ()

mintingPolicyTraceError :: Ledger.MintingPolicy
mintingPolicyTraceError = Ledger.mkMintingPolicyScript
    $$(PlutusTx.compile [||mkMintingPolicyTraceError||])

{- --------------------------------------------------------------- -}
{- Main code                                                       -}
{- --------------------------------------------------------------- -}

main :: IO ()
main = do
    print (Script.scriptCurrencySymbol mintingPolicy)
    print (Script.scriptCurrencySymbol mintingPolicyTrace)
    print (Script.scriptCurrencySymbol mintingPolicyTraceError)
