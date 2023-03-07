#!/usr/bin/env cabal

{- --------------------------------------------------------------- -}
{- How to reduce the size of a minting policy script ?             -}
{-                                                                 -}
{-   Use a wrapper that makes an untyped minting policy            -}
{-   from a typed one, such as `mkUntypedMintingPolicy`            -}
{-   defined in plutus-script-utils.                               -}
{-                                                                 -}
{-   To further reduce the size of the script, use                 -}
{-   a custom `ScriptContext` with fewer type references,          -}
{-   by defaulting all unused fields to `BuiltinData`.             -}
{-                                                                 -}
{-   However, a generalised wrapper using a parametric             -}
{-   script context constrained by `FromData p` does not work      -}
{-   with TemplateHaskell, if the `FromData` instances are         -}
{-   created with `makeIsDataIndexed`.                             -}
{-                                                                 -}
{-   This script shows the different `MintingPolicy` sizes         -}
{-   depending on the types in use.                                -}
{-                                                                 -}
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
{-# LANGUAGE DeriveGeneric                  #-}
{-# LANGUAGE DerivingStrategies             #-}
{-# LANGUAGE ImportQualifiedPost            #-}
{-# LANGUAGE NoImplicitPrelude              #-}
{-# LANGUAGE TemplateHaskell                #-}

import Prelude                              ( IO, Eq, print )
import PlutusTx.Prelude                     ( Bool(..), ($), (.), check )
import PlutusTx qualified                   ( BuiltinData, UnsafeFromData(..), compile, makeIsDataIndexed )

import GHC.Generics                         ( Generic )

import Plutus.V1.Ledger.Api qualified
    as Ledger                               ( MintingPolicy, mkMintingPolicyScript )
import Plutus.V1.Ledger.Contexts
    as Ledger                               ( ScriptContext )
import Plutus.V1.Ledger.Crypto qualified
    as Ledger                               ( PubKeyHash )
import Plutus.V1.Ledger.Scripts qualified
    as Ledger                               ( scriptSize, unMintingPolicyScript )
import Plutus.V1.Ledger.Time qualified
    as Ledger                               ( POSIXTimeRange )

import Plutus.Script.Utils.V1.Typed.Scripts qualified
    as Scripts                              ( mkUntypedMintingPolicy )

{- --------------------------------------------------------------- -}
{- Custom ScriptContext                                            -}
{- --------------------------------------------------------------- -}

data TxInfo = TxInfo
    { txInfoInputs         :: PlutusTx.BuiltinData -- unused field
    , txInfoOutputs        :: PlutusTx.BuiltinData -- unused field
    , txInfoFee            :: PlutusTx.BuiltinData -- unused field
    , txInfoMint           :: PlutusTx.BuiltinData -- unused field
    , txInfoDCert          :: PlutusTx.BuiltinData -- unused field
    , txInfoWdrl           :: PlutusTx.BuiltinData -- unused field
    , txInfoValidRange     :: Ledger.POSIXTimeRange
    , txInfoSignatories    :: [Ledger.PubKeyHash]
    , txInfoData           :: PlutusTx.BuiltinData -- unused field
    , txInfoId             :: PlutusTx.BuiltinData -- unused field
    }
    deriving stock (Generic, Eq)

PlutusTx.makeIsDataIndexed ''TxInfo [('TxInfo, 0)]

data CustomScriptContext = CustomScriptContext
    { scriptContextTxInfo  :: TxInfo
    , scriptContextPurpose :: PlutusTx.BuiltinData -- unused field
    }
    deriving stock (Generic, Eq)

PlutusTx.makeIsDataIndexed ''CustomScriptContext [('CustomScriptContext, 0)]

{- --------------------------------------------------------------- -}
{- On-chain code                                                   -}
{- --------------------------------------------------------------- -}

{-# INLINABLE mkUntypedMintingPolicy #-}
mkUntypedMintingPolicy
  :: (PlutusTx.UnsafeFromData r)
  => (r -> CustomScriptContext -> Bool)
  -> PlutusTx.BuiltinData
  -> PlutusTx.BuiltinData
  -> ()
mkUntypedMintingPolicy f r p =
    check $ f
        (PlutusTx.unsafeFromBuiltinData r)
        (PlutusTx.unsafeFromBuiltinData p)

{-# INLINABLE mkMintingPolicy #-}
mkMintingPolicy :: () -> ScriptContext -> Bool
mkMintingPolicy _ _ = True

mintingPolicy :: Ledger.MintingPolicy
mintingPolicy = Ledger.mkMintingPolicyScript
    $$(PlutusTx.compile [||Scripts.mkUntypedMintingPolicy mkMintingPolicy||])

{-# INLINABLE mkMintingPolicyCustomScriptContext #-}
mkMintingPolicyCustomScriptContext :: () -> CustomScriptContext -> Bool
mkMintingPolicyCustomScriptContext _ _ = True

mintingPolicyCustomScriptContext :: Ledger.MintingPolicy
mintingPolicyCustomScriptContext = Ledger.mkMintingPolicyScript
    $$(PlutusTx.compile [||mkUntypedMintingPolicy mkMintingPolicyCustomScriptContext||])

{- --------------------------------------------------------------- -}
{- Main code                                                       -}
{- --------------------------------------------------------------- -}

main :: IO ()
main = do
    printSize mintingPolicy
    printSize mintingPolicyCustomScriptContext
  where
    printSize = print . Ledger.scriptSize . Ledger.unMintingPolicyScript
