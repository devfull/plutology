#!/usr/bin/env cabal

{- --------------------------------------------------------------- -}
{- How to initialize a mock wallet with an initial token ?         -}
{-                                                                 -}
{-   The simplest approach is to update the default                -}
{-   `InitialDistribution` with an additional `Value`              -}
{-   for some selected known wallets.                              -}
{-                                                                 -}
{-   Then, set the `_initialChainState` with the new               -}
{-   `InitialDistribution` to create a custom `EmulatorConfig`     -}
{-   that can be `runEmulatorTraceIO'`.                            -}
{-                                                                 -}
{-   Remember to add `minAdaTxOut` alongside the token value.      -}
{-   The default distribution splits 100 Ada over 10 Ada-only      -}
{-   outputs per wallet.                                           -}
{-                                                                 -}
{-   By default, the initial distribution splits a given value     -}
{-   over several outputs. To make sure we always have an Ada-only -}
{-   output available during emulation, we create 10 Ada-only      -}
{-   outputs per wallet here. The remainder of the value is put    -}
{-   in another output.                                            -}
{-                                                                 -}
{-   It is nicer to add `minAdaTxOut` alongside the token to avoid -}
{-   taking Ada from the default distribution value to create the  -}
{-   remaining output. This will preserve the default round values -}
{-   of Ada-only outputs.                                          -}
{-                                                                 -}
{-   See Note [Creating wallets with multiple outputs].            -}
{-                                                                 -}
{- --------------------------------------------------------------- -}

{-  NOTE: [How to construct a CurrencySymbol from String ?]

    The `CurrencySymbol` constructor expects a `BuiltinByteString`.
    With `OverloadedStrings`, applying the constructor on a
    hexadecimal string litteral does typecheck.

    However, the encoding would be wrong and the conversion
    to `PolicyId` would fail.

        currencySymbolAsPolicyId :: CurrencySymbol -> Maybe PolicyId
        currencySymbolAsPolicyId (CurrencySymbol x) =
            deserialiseFromRawBytes AsPolicyId (fromBuiltin x)

    Note that `CurrencySymbol` is deriving `IsString`. Be careful
    to use the `fromString` function on a string litteral to construct
    a `CurrencySymbol`.

    Valid currency symbols deserialising `AsPolicyId` are:
      - the empty string, representing the ada policy ;
      - hexadecimal string litterals with 56 characters.
-}

{- cabal:
   build-depends:
    , aeson
    , base
    , composition
    , containers
    , data-default
    , plutus-contract
    , plutus-ledger
    , plutus-ledger-api
    , prettyprinter
    , text
-}

{-# LANGUAGE ImportQualifiedPost            #-}
{-# LANGUAGE LambdaCase                     #-}
{-# LANGUAGE OverloadedStrings              #-}
{-# LANGUAGE RecordWildCards                #-}
{-# LANGUAGE TypeApplications               #-}

import Control.Monad                        ( void )

import Data.Aeson qualified
    as Aeson                                ( Value(..) )
import Data.Composition                     ( (.:) )
import Data.Default                         ( def )
import Data.Map qualified
    as Map                                  ( singleton )
import Data.Map                             ( Map, assocs, unionWith )
import Data.String                          ( fromString )
import Data.Text                            ( Text, unpack )

import Prettyprinter                        ( Pretty (..), (<+>) )
import System.IO                            ( stdout )

import Ledger.Ada                           ( toValue )
import Ledger.Index                         ( minAdaTxOut )
import Ledger.Tx qualified                  ( ChainIndexTxOut(..) )

import Plutus.V1.Ledger.Value qualified
    as Ledger                               ( TokenName(..), Value, singleton )
import Plutus.V1.Ledger.Tx qualified
    as Ledger                               ( TxOutRef(..) )

import Plutus.Contract
    ( Contract
    , EmptySchema
    , logInfo
    , ownAddress
    , utxosAt
    )

import Plutus.Contract.Test                 ( Wallet, InitialDistribution, w1 )
import Plutus.Contract.Trace                ( defaultDist )

import Plutus.Trace
    ( EmulatorTrace
    , EmulatorConfig(..)
    , TraceConfig(..)
    , activateContractWallet
    , runEmulatorTraceIO'
    )

import Plutus.Trace.Emulator.Types
    ( ContractInstanceLog(..)
    , ContractInstanceMsg(..)
    )

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
            InstanceEvent (ContractInstanceLog (ContractLog (Aeson.String msg)) _ _) -> Just $ "*** CONTRACT LOG: " <> unpack msg
            _                                                                        -> Nothing

{- --------------------------------------------------------------- -}
{- Off-chain code                                                  -}
{- --------------------------------------------------------------- -}

showUTxO :: Ledger.TxOutRef -> Ledger.Tx.ChainIndexTxOut -> String
showUTxO txOutRef Ledger.Tx.PublicKeyChainIndexTxOut{..} =
    show (pretty txOutRef <+> pretty _ciTxOutValue)
showUTxO txOutRef Ledger.Tx.ScriptChainIndexTxOut{..} =
    show (pretty txOutRef <+> pretty _ciTxOutValue)

assocsMapM_ :: Monad m => (k -> v -> m a) -> Map k v -> m ()
assocsMapM_ m xs =
    mapM_ (uncurry m) (assocs xs)

contract :: Contract () EmptySchema Text ()
contract =
    ownAddress >>= utxosAt >>= assocsMapM_ (logInfo @String .: showUTxO)

emulatorTrace :: EmulatorTrace ()
emulatorTrace =
    void $ activateContractWallet w1 contract

{- --------------------------------------------------------------- -}
{- Main code                                                       -}
{- --------------------------------------------------------------- -}

defaultDistWith :: InitialDistribution -> EmulatorConfig
defaultDistWith distribution =
    EmulatorConfig
        { _initialChainState = Left $ unionWith (<>) distribution defaultDist
        , _params = def
        }

updateDefaultDistWith :: Wallet -> Ledger.Value -> EmulatorConfig
updateDefaultDistWith = defaultDistWith .: Map.singleton

main :: IO ()
main =
    let currencySymbol = fromString $ replicate 56 'f'
        tokenName      = Ledger.TokenName "T"
        token          = Ledger.singleton currencySymbol tokenName 1
        emulatorConfig = updateDefaultDistWith w1 $ token <> toValue minAdaTxOut
    in
        runEmulatorTraceIO' simpleTraceConfig emulatorConfig emulatorTrace
