#!/usr/bin/env cabal

{- --------------------------------------------------------------- -}
{- How the EmulatorTrace can avoid missed callEndpoint ?           -}
{-                                                                 -}
{-   When a `callEndpoint` is submitted to a busy Contract         -}
{-   instance not waiting for a Promise, this call is never seen   -}
{-   by the instance. Without a careful consideration to the       -}
{-   Contract state from the EmulatorTrace standpoint, the         -}
{-   endpoint calls are easily missed.                             -}
{-                                                                 -}
{-   It is necessary to use the `observableState` given by the     -}
{-   Contract monad writer for taking endpoint call sequencing     -}
{-   hints such as `awaitTxConfirmed` into consideration.          -}
{-                                                                 -}
{-   By `tell`ing that the Contract is `Just AwaitPromise` right   -}
{-   before an `awaitPromise`, it becomes possible to automate     -}
{-   the endpoint call scheduling in the EmulatorTrace.            -}
{-                                                                 -}
{-   In the following example, the Contract is simply waiting for  -}
{-   a number of slots before waiting for the next promise. The    -}
{-   EmulatorTrace must ensure that no endpoint call is made       -}
{-   during the countdown execution.                               -}
{-                                                                 -}
{-   In the scenario, a first burn call is made but the next call  -}
{-   is not protected by awaiting the promise. The second call for -}
{-   a mint is missed. Then the endpoint scheduler gets back on    -}
{-   track.                                                        -}
{-                                                                 -}
{- --------------------------------------------------------------- -}

{- cabal:
   build-depends:
    , aeson
    , base
    , data-default
    , freer-extras
    , freer-simple
    , plutus-contract
    , row-types
    , text
-}

{-# LANGUAGE DataKinds                      #-}
{-# LANGUAGE DeriveAnyClass                 #-}
{-# LANGUAGE DeriveGeneric                  #-}
{-# LANGUAGE DerivingStrategies             #-}
{-# LANGUAGE FlexibleContexts               #-}
{-# LANGUAGE ImportQualifiedPost            #-}
{-# LANGUAGE LambdaCase                     #-}
{-# LANGUAGE TypeApplications               #-}
{-# LANGUAGE TypeFamilies                   #-}
{-# LANGUAGE TypeOperators                  #-}

import Control.Monad.Freer.Extras.Log qualified
    as Extras                               ( logInfo )
import Control.Monad                        ( void, forever )
import Control.Monad.Freer                  ( Eff, Member )

import Data.Aeson qualified
    as Aeson                                ( Value(..) )
import Data.Aeson                           ( FromJSON, ToJSON )
import Data.Default                         ( def )
import Data.Foldable                        ( traverse_ )
import Data.Kind                            ( Type )
import Data.Monoid                          ( Last(..) )
import Data.Row                             ( Row )
import Data.Text                            ( Text )

import GHC.Generics                         ( Generic )
import GHC.Natural                          ( Natural )

import System.IO                            ( stdout )
import Text.Printf                          ( printf )

import Plutus.Contract
    ( Contract
    , Endpoint
    , Promise
    , type(.\/)
    , awaitPromise
    , endpoint
    , logInfo
    , select
    , tell
    )
import Plutus.Contract.Request qualified
    as Contract                             ( waitNSlots )
import Plutus.Contract.Test                 ( w1 )

import Plutus.Trace
    ( ContractHandle
    , EmulatorTrace
    , TraceConfig(..)
    , activateContractWallet
    , callEndpoint
    , nextSlot
    , observableState
    , runEmulatorTraceIO'
    )

import Plutus.Trace.Effects.RunContract     ( RunContract, ContractConstraints )

import Plutus.Trace.Emulator.Types
    ( ContractInstanceLog(..)
    , ContractInstanceMsg(..)
    , UserThreadMsg(..)
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
            UserThreadEvent (UserLog msg)                                            -> Just $ "*** USER LOG: " <> msg
            InstanceEvent (ContractInstanceLog (ContractLog (Aeson.String msg)) _ _) -> Just $ "*** CONTRACT LOG: " <> show msg
            _                                                                        -> Nothing

{- --------------------------------------------------------------- -}
{- Off-chain code                                                  -}
{- --------------------------------------------------------------- -}

data ContractStatus a = AwaitPromise | LiveContract a
    deriving stock (Generic, Show, Eq)
    deriving anyclass (ToJSON, FromJSON)

type Contract' w (s :: Row Type) e a = Contract (Last (ContractStatus w)) s e a
type Promise'  w (s :: Row Type) e a = Promise  (Last (ContractStatus w)) s e a

awaitPromise' :: Promise' w s e a -> Contract' w s e a
awaitPromise' p =
    tell (Last $ Just AwaitPromise) >> awaitPromise p

step :: Natural -> String -> Contract' String s Text ()
step nslots message =
    do
        tell (Last $ Just $ LiveContract message)
        void (Contract.waitNSlots nslots)

action :: Natural -> String -> Contract' String s Text ()
action nsteps name =
    mapM_
        (uncurry step)
        [(1, showStep n) | n <- [1..nsteps]]
  where
    showStep :: Natural -> String
    showStep n = printf "%s: step %d of %d" name n nsteps

type MonetarySchema =
        Endpoint "mint" ()
    .\/ Endpoint "burn" ()

mint :: Contract' String s Text ()
mint = logInfo @String "Mint" >> action 7 "Mint"

burn :: Contract' String s Text ()
burn = logInfo @String "Burn" >> action 7 "Burn"

endpoints :: Contract' String MonetarySchema Text ()
endpoints = forever $ awaitPromise' (mintPromise `select` burnPromise)
  where
    mintPromise = endpoint @"mint" $ const mint
    burnPromise = endpoint @"burn" $ const burn

{- --------------------------------------------------------------- -}
{- Main code                                                       -}
{- --------------------------------------------------------------- -}

sequenceWhile :: Monad m => (a -> Bool) -> [m a] -> m [a]
sequenceWhile _ [] = pure []
sequenceWhile p (m : ms) = do
  a <- m
  if p a
    then (a :) <$> sequenceWhile p ms
    else pure []

infixr 8 `returning`
returning :: Monad m => (a -> m b) -> a -> m a
f `returning` x = f x >> pure x

observeStateWhile ::
    ( ContractConstraints s
    , Member RunContract effs
    , FromJSON a , ToJSON a
    , FromJSON e
    )
    => ContractHandle a (s :: Row Type) e -> (a -> Bool) -> (a -> Eff effs b) -> Eff effs [a]
observeStateWhile h p m =
    sequenceWhile p
        (repeat $ observableState h >>= (m `returning`))

emulatorTrace :: EmulatorTrace ()
emulatorTrace = do
    h1 <- activateContractWallet w1 endpoints
    void nextSlot
    callEndpoint @"burn" h1 () -- non-protected call, not waiting next promise
    traverse_
        (\(call, m) -> call >> nextSlot >> untilNextPromise h1 m)
        [ (callEndpoint @"mint" h1 (), const $ void nextSlot) -- call missed, waiting next promise nonetheless
        , (callEndpoint @"burn" h1 (), logObservableState)
        , (callEndpoint @"mint" h1 (), logObservableState)
        ]
    void nextSlot
  where
    logObservableState =
        Extras.logInfo @String . show
    untilNextPromise h =
        observeStateWhile h ((/= Just AwaitPromise) . getLast)

main :: IO ()
main = runEmulatorTraceIO' simpleTraceConfig def emulatorTrace
