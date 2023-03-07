#!/usr/bin/env cabal

{- --------------------------------------------------------------- -}
{- How to run an emulator trace with a simpler trace config ?      -}
{-                                                                 -}
{-   Use `runEmulatorTraceIO` with a custom `TraceConfig`.         -}
{-   This script reuse a minting contract to demonstrate the       -}
{-   simplification in the `WalletEvent` case handling,            -}
{-   when a transaction happens.                                   -}
{-                                                                 -}
{- --------------------------------------------------------------- -}

{- cabal:
   build-depends:
    , aeson
    , base
    , data-default
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
{-# LANGUAGE LambdaCase                     #-}
{-# LANGUAGE OverloadedStrings              #-}
{-# LANGUAGE TemplateHaskell                #-}
{-# LANGUAGE TypeApplications               #-}

import Control.Monad                        ( void )

import Data.Aeson qualified
    as Aeson                                ( Value(..) )
import Data.Default                         ( def )
import Data.Text                            ( Text, unpack )

import System.IO                            ( stdout )

import Prelude                              ( IO, Integer, String, Maybe(..), (.), ($), (<>), (>>), (>>=), show )

import PlutusTx qualified                   ( BuiltinData, compile )

import Plutus.Contract.Test                 ( w1 )

import Plutus.V1.Ledger.Api qualified
    as Ledger                               ( MintingPolicy, mkMintingPolicyScript )
import Plutus.V1.Ledger.Value qualified
    as Ledger                               ( Value, TokenName, Value, adaSymbol, adaToken, singleton, valueOf )
import Ledger.Tx qualified                  ( CardanoTx(..), getCardanoTxFee, getCardanoTxId )

import Ledger.Typed.Scripts qualified
    as Scripts                              ( Any )
import Plutus.Script.Utils.V1.Scripts qualified
    as Script                               ( scriptCurrencySymbol )

import Ledger.Constraints qualified
    as Constraints                          ( mustMintValue, plutusV1MintingPolicy, unspentOutputs )

import Wallet.Emulator.LogMessages          ( TxBalanceMsg(..) )
import Wallet.Emulator.MultiAgent           ( EmulatorEvent'(..) )
import Wallet.Emulator.Wallet               ( WalletEvent(..) )

import Plutus.Trace
    ( EmulatorTrace
    , TraceConfig(..)
    , activateContractWallet
    , runEmulatorTraceIO'
    , waitNSlots
    )

import Plutus.Trace.Emulator.Types
    ( ContractInstanceLog(..)
    , ContractInstanceMsg(..)
    , UserThreadMsg(..)
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
{- Emulator TraceConfig                                            -}
{- --------------------------------------------------------------- -}

valueOfAda :: Ledger.Value -> Integer
valueOfAda v = Ledger.valueOf v Ledger.adaSymbol Ledger.adaToken

showFees :: Ledger.Tx.CardanoTx -> String
showFees tx = show (valueOfAda $ Ledger.Tx.getCardanoTxFee tx) <> " lovelaces"

simpleTraceConfig :: TraceConfig
simpleTraceConfig =
    TraceConfig
        { showEvent     = simpleShowEvent
        , outputHandle  = stdout
        }
    where
        simpleShowEvent :: EmulatorEvent' -> Maybe String
        simpleShowEvent = \case
            UserThreadEvent (UserLog msg)                                            -> Just $ "*** USER LOG: " <> msg
            InstanceEvent (ContractInstanceLog (ContractLog (Aeson.String msg)) _ _) -> Just $ "*** CONTRACT LOG: " <> unpack msg
            InstanceEvent (ContractInstanceLog (StoppedWithError err) _ _)           -> Just $ "*** CONTRACT STOPPED WITH ERROR: " <> err
            (WalletEvent w (TxBalanceLog (FinishedBalancing tx)))                    -> Just $ "*** WALLET EVENT: " <> show w <> " pays " <> showFees tx <> " fees"
            _                                                                        -> Nothing

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
main = runEmulatorTraceIO' simpleTraceConfig def emulatorTrace
